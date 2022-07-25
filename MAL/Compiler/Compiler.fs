namespace itu.dk.MAL

open Microsoft.CodeAnalysis
open BaseProgram
open NameGenerator
open Microsoft.CodeAnalysis.Emit
open System.IO
open Microsoft.CodeAnalysis.CSharp

module Compiler =

  open Gen
  open AST
  open Structure
  open AdaptorCompiler
  open CompileConstants
  open Microsoft.CodeAnalysis.CSharp.Syntax
  open System
  open Analysis
  open SyntaxUtil
  open ToIntermediate
  open FromIntermediate
  open ASTUtil
  open System.Collections.Generic
  open Monads


  type compileState =
    { filterCaches : Map<name * Set<pureTag>, annoILexpr * ILstmt list>
      globalConstants : (name * annoILexpr) list
    }

  let toCompileState (a,b) =
    { filterCaches = a ; globalConstants = b}

  //let actConvertWrap
  let newToken = SyntaxFactory.ParseToken("new")
  let publicToken = SyntaxFactory.ParseToken("public")

  let outputVar = sprintf "out_%s_%s"
  //Since the environment is being used in translating FMAL to IL, instead of IL to C#,
  //all handcrafted IL Vars must explicitly be turned into an access
  let accessBuiltin v = ILexpr.Access((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"), v)
  let accessState v = ILexpr.Access((TRec ["ExecutorState"], Var "state"), v)

  let builtin_counti = (TFun (BIFun ([TRec [""]], TInt)), accessBuiltin "counti")

  let includeTimeZeroOutput = TBool, Access((TRec ["Global"], accessState "Global"), "TimeZeroOutputIncluded")

  type fieldGen =
     | NoField // Field exists on super already
     | HideField // Field exists on super already, but as a superType.
     | NormalField

  type fieldOutput =
    outputFormat * //AST outputFormat as specified in the MAL source code
    string * //Name of the field
    string *
    ILtyp //type of field

  let syntax_get_set =
    let syntaxList = new SyntaxList<AccessorDeclarationSyntax>()
    let getS = SyntaxFactory.AccessorDeclaration(SyntaxFacts.GetAccessorDeclarationKind(SyntaxKind.GetKeyword))
    let setS = SyntaxFactory.AccessorDeclaration(SyntaxFacts.GetAccessorDeclarationKind(SyntaxKind.SetKeyword))
    let syntaxList = syntaxList.Add(getS.WithSemicolonToken(semi))
    let syntaxList = syntaxList.Add(setS.WithSemicolonToken(semi))
    syntaxList

  let genPropGetSet (fieldTyp_cs : TypeSyntax) (fieldName : string) underlyingField baseName hasSet isNew (conv : (string * ConversionType) option) =
    let syntaxList = new SyntaxList<AccessorDeclarationSyntax>()
    let getBlock = SyntaxFactory.Block().AddStatements([|
      match conv with
      | None -> SyntaxFactory.ParseStatement(sprintf "{return this.%s; }" underlyingField)
      | Some (_,ConversionType.Null) -> SyntaxFactory.ParseStatement("{return null; }")
      | Some (converter, ConversionType.PostConversion) -> SyntaxFactory.ParseStatement(sprintf "{return this.%s%s; }" underlyingField converter)
      | Some (converter, ConversionType.Replace) -> SyntaxFactory.ParseStatement(sprintf "{ %s }" converter)
      |])
    let getS     = SyntaxFactory.AccessorDeclaration(SyntaxFacts.GetAccessorDeclarationKind(SyntaxKind.GetKeyword)).WithBody(getBlock)
    let syntaxList = syntaxList.Add(getS)
    let syntaxList =
      if not hasSet
      then
        syntaxList
      else
        let setterString =
          match baseName with
          | None -> sprintf "{ this.%s = value; }" underlyingField
          | Some baseName -> sprintf "{ this.%s = value; base.%s = value; }" underlyingField baseName
        let setBlock = SyntaxFactory.Block().AddStatements([|SyntaxFactory.ParseStatement(setterString)|])
        let setS = SyntaxFactory.AccessorDeclaration(SyntaxFacts.GetAccessorDeclarationKind(SyntaxKind.SetKeyword))
                    .WithBody(setBlock)
        syntaxList.Add(setS)

    let result = SyntaxFactory.PropertyDeclaration(fieldTyp_cs, fieldName)
    let result =
      if isNew
      then result.WithModifiers(SyntaxTokenList.Create(publicToken).Add(newToken))
      else result.WithModifiers(SyntaxTokenList.Create(publicToken))
    result.WithAccessorList(SyntaxFactory.AccessorList(syntaxList))

  let addMembersNS members (node : NamespaceDeclarationSyntax) = node.AddMembers members
  let addMembersNSM members = modifyStateRS <| addMembersNS [|members|]

  let addMembersC members (node : ClassDeclarationSyntax) = node.AddMembers members
  let addMembersCM members = modifyStateRS <| addMembersC [|members|]

  let addModifiers modifiers (cn : ClassDeclarationSyntax) = cn.AddModifiers modifiers
  let addModifiersM modifiers = modifyStateRS <| addModifiers [|modifiers|]

  let addBaseListTypes types (cn : ClassDeclarationSyntax) = cn.AddBaseListTypes types
  let addBaseListTypesM types = modifyStateRS <| addBaseListTypes [|types|]

  /// Generates properties on classNode
  /// baseExcludes contains fields that are inherited from a baseclass
  /// which therefore should be excluded
  let genClass (analyser : Analyser<'eA>)
               (className: string)
               (combTagMap : Map<string, Map<string, typ<Position>>>)
               (interfaces : string list)
               (extInterface : (string * extPropertyInfo seq) option) =
    let fields = combTagMap.[className]
    //let fields =
    //  if ASTUtil.tagIsSubOf analyser.DataStructure.superTypes className "Policy" || ASTUtil.tagIsSubOf analyser.DataStructure.superTypes className "Group"
    //  then Map.add (sprintf "%s_id" className) AST.ASTTInt fields
    //  else fields

    let dataStructure = analyser.DataStructure
    let superName = dataStructure.superTypes.[className]
    let superFields = Option.map (fun name -> dataStructure.totalMap.[name]) superName
    let baseList =
      match superName with
      | None -> interfaces
      | Some superTag -> superTag :: interfaces
    let baseList = List.map (fun nm -> genTypeIdentifier analyser (TRec [nm]) false) baseList
    let baseList, props =
      match extInterface with
      | None -> baseList, Seq.empty
      | Some (extName, props) -> baseList @ [SyntaxFactory.ParseTypeName extName] , props
    let classNode = SyntaxFactory.ClassDeclaration(className)

    let classNode = classNode.AddTypeParameterListParameters(SyntaxFactory.TypeParameter("TFunction"))
    let identifier = SyntaxFactory.IdentifierName "TFunction"
    let sepList = SyntaxFactory.SeparatedList([SyntaxFactory.TypeConstraint <| SyntaxFactory.ParseTypeName "IFunction" :> TypeParameterConstraintSyntax])
    let constraintClause = SyntaxFactory.TypeParameterConstraintClause(identifier, sepList)

    let classNode = classNode.AddConstraintClauses([| constraintClause |])
    let builder =
      compileBuilder {
        do! addModifiersM publicToken
        let baseMapper baseType =
          compileBuilder {
            do! addBaseListTypesM <| SyntaxFactory.SimpleBaseType baseType
            }
        do! mapRS_ baseMapper baseList
        let propMapper (propInfo : extPropertyInfo) =
          compileBuilder {
            let typSyn_cs = SyntaxFactory.ParseTypeName propInfo.fieldTypeStr
            let prop = genPropGetSet
                          typSyn_cs
                          propInfo.fieldName
                          propInfo.underlyingField
                          None
                          propInfo.hasSet
                          false
                          propInfo.converter
            do! addMembersCM prop
          }
        do! mapRS_ propMapper (List.ofSeq props)
        let fieldMapper (fieldName, fieldType) =
          let genFieldTyp = genTypeIdentifier analyser (convertType fieldType) false
          let genFieldAs =
            match superFields with
            | None -> NormalField
            | Some sFields ->
              match Map.tryFind fieldName sFields with
              | None -> NormalField
              | Some sField ->
                if sField = fieldType
                then
                  NoField
                else
                  HideField
          match genFieldAs with
          | NoField -> compileBuilder {return ()}
          | NormalField ->
            let syntaxList2 = SyntaxTokenList.Create(publicToken)
            let prop = SyntaxFactory.PropertyDeclaration(genFieldTyp, fieldName)
                          .WithModifiers(syntaxList2)
                          .WithAccessorList(SyntaxFactory.AccessorList(syntax_get_set))
            compileBuilder {do! addMembersCM prop}
          | HideField ->
            let varDeclerator = SyntaxFactory.VariableDeclarator("_"+fieldName)
            let list = SyntaxFactory.SeparatedList().Add(varDeclerator);
            let varDec = SyntaxFactory.VariableDeclaration(genFieldTyp, list)
            let fldDec = SyntaxFactory.FieldDeclaration(varDec)
            let prop = genPropGetSet genFieldTyp fieldName ("_"+fieldName) (Some fieldName) true true None
            compileBuilder {
              do! addMembersCM fldDec
              do! addMembersCM prop
            }
        do! mapRS_ fieldMapper (Map.toList fields)
      }
    builder analyser classNode |> snd

  /// <summary>
  /// Generates an interface for all union types interaces.
  /// That is the union interface between Risk and Expense is Risk_Expense containing the intersection of
  /// Risk and Expense fields.
  /// Returns an interface map that is a for each tag contains the unions it should implement
  /// </summary>
  let genUnionInterface (subTypes : string list) (interfaceMap : Map<string, string list>) (allUnions : Set<(string list)>) =
    compileBuilder {
      let subTypes = List.sort subTypes

      // Create union interfaces
      let allInterfaces =
        List.filter
          (fun sb ->
            List.length sb > 1 && Set.contains sb allUnions
          ) <| GeneralUtil.powerset subTypes

      let folder interfaceMap tagUnion =
          compileBuilder {
              let! (analyser : Analyser<'eA>) = askRS
              let interfaceName = ASTUtil.tagUnionI tagUnion
              let unionFields =
                List.fold (fun acc tag ->
                  let fields' = analyser.DataStructure.totalMap.[tag]
                  GeneralUtil.mapChoose
                    (fun key field ->
                      match Map.tryFind key fields' with
                      | None -> None
                      | Some v -> tryGreatestComTyp analyser.DataStructure.superTypes false field v
                    ) acc
                ) analyser.DataStructure.totalMap.[List.head tagUnion] (List.tail tagUnion )

              let interfaceMap = List.fold (fun map' tag -> GeneralUtil.mapConcatAdd tag interfaceName map') interfaceMap tagUnion

              let interfaceNode = SyntaxFactory.InterfaceDeclaration(interfaceName).WithModifiers(SyntaxTokenList.Create(publicToken))
              let interfaceNode = interfaceNode.AddTypeParameterListParameters(SyntaxFactory.TypeParameter("TFunction"))
              let identifier = SyntaxFactory.IdentifierName "TFunction"
              let sepList = SyntaxFactory.SeparatedList([SyntaxFactory.TypeConstraint <| SyntaxFactory.ParseTypeName "IFunction" :> TypeParameterConstraintSyntax])
              let constraintClause = SyntaxFactory.TypeParameterConstraintClause(identifier, sepList)
              let interfaceNode = interfaceNode.AddConstraintClauses constraintClause

              let interfaceNode =
                Map.fold (fun (node : InterfaceDeclarationSyntax) fieldName fieldType ->
                  let genFieldTyp = genTypeIdentifier analyser (convertType fieldType) false
                  node.AddMembers(SyntaxFactory.PropertyDeclaration(genFieldTyp, fieldName)
                              .WithAccessorList(SyntaxFactory.AccessorList(syntax_get_set)))
                ) interfaceNode unionFields
              do! addMembersNSM interfaceNode
              return interfaceMap
          }


      // Create TypeSpans
      //let! _ = TypeSpanCompiler.genTypeSpans subTypes allUnions

      return! foldRS folder interfaceMap allInterfaces
    }

  let genClasses analyser =
    let tmp =
      compileBuilder {
        let! (analyser: Analyser<'eA>) = askRS
        let combTagMap = Map.toList <| analyser.DataStructure.combinedTagMap
        let outputTagMap = Map.toList <| analyser.DataStructure.outputMap

        let folder interfaceMap (tag, vs) =
          match Map.tryFind tag analyser.DataStructure.subTypes with
          | Some [] | Some [_] | None -> retRS interfaceMap
          | Some subTypes -> genUnionInterface subTypes interfaceMap analyser.UnionTags

        let! interfaceMap = foldRS folder Map.empty (combTagMap)

        let! _ =
          mapRS
            (fun (tag, vs) ->
              compileBuilder {
                let! _ = TypeSpanCompiler.genTypeSpans [tag] analyser
                match Map.tryFind tag analyser.DataStructure.subTypes with
                | Some [] | Some[_] | None -> return ()
                | Some subTypes ->
                  let! _ = TypeSpanCompiler.genTypeSpans subTypes analyser
                  return ()
              }
            ) (combTagMap)

        //let! _ = TypeSpanCompiler.genTypeSpans ["ICashFlow"] analyser.UnionTags
        //let! _ = TypeSpanCompiler.genTypeSpans ["ThreeStateState"] analyser.UnionTags

        let update (tag,_) =
          compileBuilder {
            if tag = "Transfers"
            then return ()
            else
            let extName =
              match List.tryFind (fun (nm,_) -> nm = tag) BaseProgram.interfaceFields with
              | None -> None
              | Some (_,info) -> Some info
            do! addMembersNSM <| genClass analyser tag (Map.ofList combTagMap) (GeneralUtil.mapFindOrEmpty tag interfaceMap) extName
          }
        do! mapRS_ update combTagMap
      }
    tmp analyser

  let addToMap key stmt map =
    match Map.tryFind key map with
    | None -> map.Add(key,[stmt])
    | Some stmts -> map.Add(key,stmt::stmts)

  let outputColType superTag s =
    match superTag with
    | "Global" -> sprintf "%s[]" s, sprintf "null"
    | "Group" -> sprintf "%s[][]" s, sprintf "null"
    | "Policy" -> sprintf "%s[][]" s, sprintf "null"
    | _ ->  failwith "Currently only output from Global, Group, or Policy"

  /// <summary>
  /// Generates output handling for init, update, and finalize of an output field.
  /// That is: create needed collections, insert into collections, and transform to desired representation
  /// The method does *not* construct iterations over multiple output fields, this is done by placeInContext.
  /// </summary>
  let genOutputHandling entity superTag ((outFormat, fieldName, colName, typ) as outputInfo : fieldOutput) =
    let isOptional =
      match typ with
      | TOption (TDouble)  -> true
      | TDouble -> false
      | _ -> failwith (sprintf "unexpected output type: %A" typ)

    let entTyp = TRec [superTag]
    let addToArrTyp = TFun (BIFun ([TArray TDouble; TInt; TDouble], TUnit))
    let addToArr2dTyp = TFun (BIFun ([TArray TDouble; TInt; TDouble; TDouble], TUnit))
    let dictTyp = TMap (TStr, TArray TDouble, false)
    let innerDictTyp = TMap(TStr, dictTyp, false)
    let realisedDiscountFactorTyp = TFun <| SysFun(TDouble, TDouble)
    let finalizeDiscountTyp =
      TFun (BIFun ([TArray (TDouble); dictTyp; realisedDiscountFactorTyp; TInt ; TArray (TDouble); TArray (TDouble) ], dictTyp))
    let finalizeDiscountValueTyp =
      TFun (BIFun ([TArray (TDouble); TArray (TDouble); realisedDiscountFactorTyp; TInt ; TArray (TDouble); TArray (TDouble) ], dictTyp))
    let varCollection = (dictTyp, accessState colName)
    let accessCollection collection access =
        ILexpr.Access
          (
            collection,
            access
          )
    let newArrTyp2d = TFun (BIFun ([TInt ; TInt], TArray (TArray TDouble)))
    let newArrTypOpt2d = TFun (BIFun ([TInt ; TInt ; TDouble], TArray (TArray TDouble)))
    let newArrTyp = TFun (BIFun ([TInt], TArray TDouble))
    let newArrTypOpt = TFun (BIFun ([TInt ; TDouble], TArray TDouble))

    let entField = (ILtyp.TDouble, ILexpr.Access((entTyp, ILexpr.Var var_ent), fieldName))
    let entFieldLambda =
      (ILtyp.TDouble,
        Lambda("_i", (ILtyp.TDouble,
          ILexpr.Access((ILtyp.TDouble,
            ILexpr.Call( funtyp.Other,
              (ILtyp.TDouble, ILexpr.Access((ILtyp.TDouble, ILexpr.Var (sprintf "_%s" entity)),"index")),
              [ILtyp.TInt, ILexpr.Var "_i"]
              )
            ), fieldName
           )
          )
        )
      )

    let newArr =
      if not isOptional
      then (TArray TDouble,ILexpr.Call(funtyp.Other, (newArrTyp, accessBuiltin var_newArr), [(TDouble, ILexpr.Var var_numbersOfProj)]))
      else (TArray (TOption TDouble),ILexpr.Call(funtyp.Other, (newArrTypOpt, accessBuiltin var_newArrOpt), [(TDouble, ILexpr.Var var_numbersOfProj) ; entField]))

    let newArr2d =
      if not isOptional
      then (TArray TDouble,ILexpr.Call(funtyp.Other, (newArrTyp2d, accessBuiltin var_newArr), [(TInt, ILexpr.Var "___Counter");(TDouble, ILexpr.Var var_numbersOfProj)]))
      else (TArray TDouble,ILexpr.Call(funtyp.Other, (newArrTypOpt2d, accessBuiltin var_newArrOpt), [(TInt, ILexpr.Var "___Counter");(TDouble, ILexpr.Var var_numbersOfProj); entFieldLambda]))

    let addInitOutputCol =
      match superTag with
      | "Group"  -> Some (ILstmt.Assign((TUnit, accessState colName),newArr2d))
      | "Policy" -> Some (ILstmt.Assign((TUnit, accessState colName),newArr2d))
      | "Global" -> Some (ILstmt.Assign((TUnit, accessState colName),newArr))
      | _ -> failwith "Currently only output from Global, Group, or Policy"

    let addToOutputCol =
      match superTag with
      | "Group"
      | "Policy" ->
        ILstmt.Expression
          ( TUnit
          , ILexpr.Call(funtyp.Other, (addToArr2dTyp, accessBuiltin var_addToArr), [(dictTyp, accessState colName); (ILtyp.TDouble, ILexpr.Var "__iter") ; (ILtyp.TDouble, ILexpr.Var var_projNumber) ; entField])
          )
      | "Global" ->
        ILstmt.Expression
          ( TUnit
          , ILexpr.Call(funtyp.Other, (addToArrTyp, accessBuiltin var_addToArr), [(dictTyp, accessState colName); (ILtyp.TDouble, ILexpr.Var var_projNumber) ; entField])
          )

      | _ -> failwith "Currently only output from Global, Group, or Policy"

    let outputContainer, outputMethod =
      if outFormat = CashFlow then
          match superTag with
          | "Global" -> (dictTyp,      Var var_cashFlowOutput), (TUnit, accessBuiltin "___AddTimeDependentGlobalOutput")
          | "Group"  -> (innerDictTyp, Var var_cashFlowOutput),  (TUnit, accessBuiltin "___AddTimeDependentGroupOutput")
          | "Policy" -> (innerDictTyp, Var var_cashFlowOutput), (TUnit, accessBuiltin "___AddTimeDependentPolicyOutput")
          | _ -> failwith "Currently only output from Global, Group, or Policy"
      else
          match superTag with
          | "Global" -> (dictTyp, Var var_scalarOutput),      (TUnit, accessBuiltin "___AddScalarGlobalOutput")
          | "Group"  -> (innerDictTyp, Var var_scalarOutput),  (TUnit, accessBuiltin "___AddScalarGroupOutput")
          | "Policy" -> (innerDictTyp, Var var_scalarOutput), (TUnit, accessBuiltin "___AddScalarPolicyOutput")
          | _ -> failwith "Currently only output from Global, Group, or Policy"

    let finalizeOutput =
      let discVar,typ =
        if superTag = "Global"
        then var_finalizeDiscountValue,finalizeDiscountValueTyp
        else var_finalizeDiscount, finalizeDiscountTyp
      let outputVar = ILtyp.TArray ILtyp.TDouble, accessState colName
      let outputVar =
        match outFormat with
        | CashFlow -> outputVar
        | MissingFormat -> failwith "impossible"
        | PresentValueMid | PresentValueEnd ->
          let discountType =
            match outFormat with
            | PresentValueMid -> 0
            | PresentValueEnd -> 1
            | CashFlow -> failwith "impossible"
            | MissingFormat -> failwith "impossible"
          ILtyp.TArray ILtyp.TDouble,
            ILexpr.Call
              ( funtyp.Other
              , (typ, accessBuiltin discVar)
              , [ TArray (TDouble), Access((dictTyp, accessState "Global"), var_projectionTimes)
                ; outputVar
                ; realisedDiscountFactorTyp
                    , accessState var_realisedDiscountFactor
                ; (TInt, ILexpr.CstI(discountType) )
                ; realisedDiscountFactorTyp
                  , ILexpr.Var var_realisedDiscountFactorContinuousPayments
                ; realisedDiscountFactorTyp
                  , ILexpr.Var var_realisedDiscountFactorContinuousPaymentsInclPolicyEndTimes
                ]
              )
      if superTag = "Global"
      then
        ILstmt.Expression(TUnit, ILexpr.Call(funtyp.Other, outputMethod,
                            [outputContainer; ILtyp.TStr, ILexpr.CstS fieldName; outputVar; TUnit, accessCollection (TUnit, Var "state") "annotations"])) // We do not need collection names for global
      else
        ILstmt.Expression(TUnit, ILexpr.Call(funtyp.Other, outputMethod,
                            [ outputContainer
                            ; ILtyp.TStr, ILexpr.CstS fieldName
                            ; outputVar
                            ; (TInt , ILexpr.Var (sprintf "%sSize" entity))
                            ; (TFun IFun , ILexpr.Var (sprintf "%sFun" entity))
                            ; TUnit, accessCollection (TUnit, Var "state") "annotations"]))

    (addInitOutputCol, addToOutputCol, finalizeOutput)

  let includeCaches comp =
    compileBuilder{
      let! env = askRS
      let! compileState = getStateRS
      return comp env {emptyState with filterCaches = compileState.filterCaches ; globalConstants = compileState.globalConstants}
    }

  /// <summary>
  /// Places output handling made by 'stmts' in the correct context during initialize and update.
  /// E.g., for update and groups a loop is constructed that iterates through all kinds of groups
  /// </summary>
  let placeOutputHandlingInContext superTag entity stmts isInit =
    let stmts = ILstmt.Block(stmts)
    compileBuilder {
      match superTag with
      | "Global" ->
        let fresh_ent = wishName var_ent
        let lb = ILstmt.Let(fresh_ent, (TRec ["Global"], accessState "Global"), false)
        return ILstmt.Block([lb; renameVar var_ent fresh_ent stmts])
      | "Policy" ->
        let fresh_ent = wishName var_ent
        if isInit
        then
          let enum = (ILtyp.TTypeSpan(TRec ["Policy"]), accessState "Policies")
          return
           ILstmt.Block
              [ ILstmt.Declare(fst enum, "_Policy")
              ; ILstmt.Assign((fst enum, ILexpr.Var "_Policy"), enum)
              ; ILstmt.Assign((TUnit, Var "___Counter"), (TInt, ILexpr.Call(funtyp.Other, builtin_counti, [enum]) ) )
              ; stmts ]
        else
          return
            ILstmt.For
              ( "__iter" //Important this is not a wished name since it is used by input generation
              , [ fresh_ent
                , (TArray(TRec ["Policy"]), accessState "Policies")
                ]
              , renameVar var_ent fresh_ent stmts
              , None
              )
      | "Group" ->
        let! (enum, preStmts) =
          if entity = superTag
          then
            compileBuilder {return (TArray(TRec ["Group"]), accessState "Groups"), None}
          else
            compileBuilder {
            let grpType = nos_TList <| AST.TRec (noSyntaxAnno,["Group"])
            let ASTFilter =
              AST.expr.Filter
                ( nos_TList <| AST.TRec (noSyntaxAnno, [entity])
                , noColonSyntax
                , AST.Var (grpType, noConstSyntax, (noConstSyntax, "Groups"))
                , [(noSyntaxAnno, entity)]
                )
            //let comp e env = exprConvert e <| environment.extendMany env
            let! (expr,ilgenState, stmts) = exprConvert ASTFilter |> includeCaches
            do! putStateRS {filterCaches = ilgenState.filterCaches ; globalConstants = ilgenState.globalConstants}
            return (expr, Some <| Seq.toList (Seq.rev stmts))
            }
        let fresh_ent = wishName var_ent
        return
          renameVar var_ent fresh_ent <|
            if isInit
            then
              let entityName = sprintf "_%s" entity
              Option.foldBack
                (fun stmt1 stmt2 -> ILstmt.Block <| Seq.append stmt1 (Seq.singleton stmt2))
                preStmts
                (ILstmt.Block [ ILstmt.Declare(fst enum, entityName)
                              ; ILstmt.Assign((fst enum, ILexpr.Var entityName), enum)
                              ; ILstmt.Assign((TUnit, Var "___Counter"), (TInt, ILexpr.Call(funtyp.Other, builtin_counti, [(fst enum, ILexpr.Var entityName)]) ) )
                              ; stmts ])
            else
              Option.foldBack
                (fun stmt1 stmt2 -> ILstmt.Block <| Seq.append stmt1 (Seq.singleton stmt2))
                preStmts
                (ILstmt.For
                  ( "__iter" //Important this is not a wished name since it is used by input generation
                  , [fresh_ent , enum]
                  , stmts
                  , None
                  ))
      | _ ->
        failwith "Currently only output from Global, Group, or Policy"
        return ILstmt.Block([])
    }


  type gen_outputHandling = Map<string, fieldOutput list>

  type gen_executerState =
    { classDec : ClassDeclarationSyntax
    ; stateFields : string list
    ; output_handling : gen_outputHandling
    }

  let accessEnv =
    [ "Projection", ExecutorState;
      "Global", ExecutorState;
      "Groups", ExecutorState;
      "CashFlows", ExecutorState;
      "Policies", ExecutorState;
      "Equities", ExecutorState;
      var_newArr, BuiltIn;
      var_newArrOpt, BuiltIn;
      var_addToArr, BuiltIn;
      var_finalizeDiscount, BuiltIn;
      var_finalizeDiscountValue, BuiltIn;
      var_numbersOfProj, Local;
      var_projNumber, Local;
      var_isNull, BuiltIn]

  let outTags = ["Group"; "Global"; "Policy"]
  let outSuperTag (analyser : Analyser<'eA>) tag =
      let rec inner t =
          if (not <| List.contains t outTags) && tagIsSubType analyser.DataStructure.subTypes t
          then superOf analyser.DataStructure.subTypes t |> inner
          else t in inner tag

  let addMembersToClass fldDec =
    compileBuilder {
      let! (exec : gen_executerState) = getStateRS
      do! putStateRS {exec with classDec = exec.classDec.AddMembers([|fldDec|])}
    }

  let addStmtToClassBody bodyStmts =
    compileBuilder {
      let! (exec : gen_executerState) = getStateRS
      let oldConst = getConstructor exec.classDec
      let cDec = exec.classDec.ReplaceNode(oldConst, oldConst.AddBodyStatements([|bodyStmts|]))
      do! putStateRS {exec with classDec = cDec}
    }

  let addCollectionToState colName =
    compileBuilder {
      let! (exec : gen_executerState) = getStateRS
      do! putStateRS {exec with stateFields = colName::exec.stateFields}
    }

  let addFieldOutputToMap entity fieldOutput =
    compileBuilder {
      let! (exec : gen_executerState) = getStateRS
      do! putStateRS {exec with output_handling = addToMap entity fieldOutput exec.output_handling}
    }

  let modeToString = function
    | Debug -> "ProjectionOutputEntityAnnotation.Debug"
    | Average -> "ProjectionOutputEntityAnnotation.Average"

  let genExecutorState (analyser : Analyser<'eA>) : gen_executerState =
    let classDec = getClass <| SyntaxFactory.ParseMemberDeclaration(Template.executorStateTemplate)
    let stateFields =
      [var_global_output; var_group_output; var_policy_output; var_projectionTimes;
       var_realisedDiscountFactorContinuousPaymentsInclPolicyEndTimes;
       var_realisedDiscountFactorContinuousPayments;
      var_global_scalars; var_group_scalars; var_policy_scalars]
    // Adds annotation initialisers and output fields to classDec
    // Adds outputNames to stateFields
    // Builds outputHandling for each outputvar
    let innerMap entity (fieldNames, outs) =
      let superTag = outSuperTag analyser entity
      let (typ, format, outPos, modes) = outs
      compileBuilder {
        let colName = outputVar entity (List.head fieldNames)
        let typ' = convertType typ
        let nonNullableTyp =
          match typ' with
          | ILtyp.TOption t -> ILtyp.TOption t
          | t -> ILtyp.TOption t

        let colElementTyp_string = genTypString analyser.DataStructure.subTypes <| nonNullableTyp <| typeIdentifierOptions
        let colTyp_string, colInitializer = outputColType superTag colElementTyp_string
        let initializer = SyntaxFactory.EqualsValueClause (SyntaxFactory.ParseExpression (colInitializer))
        let name = SyntaxFactory.VariableDeclarator(colName).WithInitializer(initializer)
        let list = SyntaxFactory.SeparatedList().Add(name);
        let dec = SyntaxFactory.VariableDeclaration(SyntaxFactory.ParseTypeName colTyp_string, list)
        let fldDec = SyntaxFactory.FieldDeclaration(dec).WithModifiers(SyntaxTokenList.Create(publicToken))
        do! addMembersToClass fldDec
        let annotations = String.concat ", " <| List.map modeToString modes
        let newBody =
          SyntaxFactory.ParseStatement(
            sprintf "annotations[\"%s\"] = new HashSet<ProjectionOutputEntityAnnotation>(new ProjectionOutputEntityAnnotation[] {%s});"
              fieldNames.Head
                annotations)
        do! addStmtToClassBody newBody
        do! addFieldOutputToMap entity (format, (fieldNames.Head), colName, typ')
      }
    let outerMap (entity, outs) = mapRS_ (innerMap entity) (Map.toList outs)
    let executorStateBuilder = mapRS_ outerMap (Map.toList analyser.DataStructure.outputMap)
    let startState = {classDec = classDec; stateFields = stateFields; output_handling = Map.empty}
    snd <| executorStateBuilder analyser startState

  let genMethod (environment : ILGenEnvironment<'eA>) (f : string, args, (convertedExpr, stmts), isExport) =
    let stmts = List.rev stmts
   // let analysis = Dataflow.collectDependencies stmts
    let stmts' =
      List.fold
        (fun ss s -> let s = genStmt () s environment in List.rev (s)@ss) [] stmts
    let typ' = genTypeIdentifier environment.analyser (match convertedExpr with None -> TUnit | Some (ilt, _) -> ilt) false
    let args' =
      List.toArray
        <|
          List.map
            (fun (_,(_,x),typ) ->
              let typ' = genTypeIdentifier environment.analyser (convertType typ) false
              SyntaxFactory.Parameter(SyntaxFactory.Identifier x).WithType(typ')) args
    let method = SyntaxFactory.MethodDeclaration(typ', f)
    let method = method.AddParameterListParameters(args')
    let method = if isExport then method.AddModifiers([|SyntaxFactory.Token SyntaxKind.PublicKeyword|]) else method
    let method = method.AddBodyStatements (Array.ofList <| List.rev stmts')
    match convertedExpr with
    | Some e -> method.AddBodyStatements (SyntaxFactory.ReturnStatement(genExpr environment.analyser false e))
    | None -> method

  let rec private replaceCondExpr' forceStep cond map start  =
    let recurse = replaceCondExpr' false cond map
    match start with
    | e when cond e && not forceStep -> map e |> replaceCondExpr' true cond map
    | AST.Pair(ea,sA,e1,e2) -> AST.Pair(ea,sA, recurse e1, recurse e2)
    | AST.Proj(ea, ps, e, i) -> AST.Proj(ea, ps, recurse e, i)
    | AST.Filter(ea, fs, e, tl) -> AST.Filter(ea, fs, recurse e, tl)
    | AST.Let(ea, ls, lbs, e) -> AST.Let(ea, ls, List.map (fun (lb, i, ex) -> (lb, i, recurse ex)) lbs, recurse e)
    | AST.If(ea, is, e, t, f) -> AST.If(ea, is, recurse e, recurse t, recurse f)
    | AST.Match(ea, ms, e, cases) -> AST.Match(ea, ms, recurse e, List.map (fun (mcs, pattern, ex) -> (mcs, pattern, recurse ex)) cases)
    | AST.Map(ea, fcs, forIns, wOpt, e) ->
      let forIns' = List.map (replaceCondForins cond map) forIns
      AST.Map(ea, fcs, forIns', Option.map (fun (ws, e) -> (ws, recurse e)) wOpt, recurse e)
    | AST.List(ea, lsyn, es,uA) -> AST.List(ea, lsyn, List.map (fun (s,e) -> s,recurse e) es, uA)
    | AST.FCall(ea, fc, e, args) -> AST.FCall(ea, fc, recurse e, List.map (fun (ls, e) -> (ls, recurse e)) args)
    | AST.BinOp(ea, sa, op, e1, e2) -> AST.BinOp(ea, sa, op, recurse e1, recurse e2)
    | AST.EPar(ea, ps, e) -> AST.EPar(ea, ps, recurse e)
    | AST.CreateRec (eA, sA, name, fields) -> CreateRec(eA, sA, name, List.map (fun (name, expr) -> (name, recurse expr)) fields)
    | AST.ESome (ea, sa, e) -> AST.ESome (ea, sa, recurse e)
    | AST.EMissing _ | AST.CDouble _ | AST.CBool _ | AST.CStr _ | AST.Var _ | AST.ENone _ | AST.CEnum _ -> start
  and replaceCondForins cond map forIn =
    let (sA, iters) = forIn
    let replaceSingle (n,e) = (n, replaceCondExpr' false cond map e)
    let reg many = List.map replaceSingle many
    ( sA
    , Choices.choiceMap reg replaceSingle iters
    )
  let replaceCondExpr = replaceCondExpr' false

  let rec replaceCondStmt cond map start =
    let recurse = replaceCondStmt cond map
    match start with
    | s when cond s -> map s
    | SUpdate(fcs, forIn, wOpt, stmt) -> SUpdate(fcs, forIn, wOpt, recurse stmt)
    | SBlock (sA,stmts) -> SBlock (sA,List.map recurse stmts)
    | SIf (ifS, cExp, tStmt, fOpt) -> SIf(ifS, cExp, recurse tStmt, Option.map (fun (s,stm) -> s,recurse stm) fOpt)
    | SMatch(mSyn, exp, cases) -> SMatch(mSyn, exp, List.map (fun (syn, t, s) -> (syn, t, recurse s)) cases)
    | SDo _ | SDoCSharp _ | SLet _ | SAss _
    | SkippedStmt _ | SOverwrite _ | STransfer _ -> start
  let rec replaceCondExprInStmt cond map start =
    let recurseS = replaceCondExprInStmt cond map
    let recurseE = replaceCondExpr cond map
    match start with
    | SUpdate(fcs, forIn, wOpt, stmt) ->
      let forIn = replaceCondForins cond map forIn
      SUpdate(fcs, forIn, Option.map (fun (ws, e) -> (ws, recurseE e)) wOpt, recurseS stmt)
    | SAss(sas, assProj, e) -> SAss(sas, assProj, recurseE e)
    | SLet(lbs, i, e) -> SLet(lbs, i, recurseE e)
    | SDo(ac, location, args) -> SDo(ac, location, List.map (fun (m,e) -> (m,recurseE e)) args)
    | SBlock (sA,stmts) -> SBlock (sA,List.map recurseS stmts)
    | SOverwrite(sA, assProj, e) -> SOverwrite(sA, assProj, recurseE e)
    | STransfer(sA, assProj1, e, assProj2) -> STransfer(sA, assProj1, recurseE e, assProj2)
    | SkippedStmt _ -> start
    | SDoCSharp (sA, ident) -> SDoCSharp (sA, ident)
    | SIf(ifs, e, t, fOpt) -> SIf(ifs, recurseE e, recurseS t, Option.map (fun (s, stm) -> s,recurseS stm) fOpt)
    | SMatch(mSyn, e, cases) -> SMatch(mSyn, recurseE e, List.map (fun (syn, t, s) -> (syn, t, recurseS s)) cases)

  let replaceFunc cond map dec =
    match dec with
    | AST.FunDec(fds, i, ps, expr) ->
      AST.FunDec(fds, i, ps, replaceCondExpr cond map expr)
    | AST.ActDec(fds, i, ps, stmt) ->
      AST.ActDec(fds, i, ps, replaceCondExprInStmt cond map stmt)
    | _ -> dec
  let replaceAct cond map dec =
    match dec with
    | AST.ActDec(fds, i, ps, stmt) ->
      AST.ActDec(fds, i, ps, replaceCondStmt cond map stmt)
    | _ -> dec

  let inlining (compilerOptions : CompilerOptions) decs =
    if not compilerOptions.inln then decs else
    let closures = ClosureAnalysis.closureAnalysisDecs decs
    let ordering = List.distinct (closures.initFuns@closures.manageFuns)
    let funcName name =
      function
      FunDec(_, (_, f), _, _) -> f = name
      | ActDec(_, (_, f), _, _) -> f = name
      | _ -> false
    let nameMatch name = function AST.Var(_,_,(_,i)) -> i = name | _ -> false
    List.fold
      (fun decs fname ->
        let func = List.find (funcName fname) decs
        let isCallToFunc = function FCall (_,_,e,_) -> nameMatch fname e | _ -> false
        let replacer =
          match func with
          | FunDec(_, _, ps, expr) ->
            let mapFCall =
              function
              | FCall(_,_,_,args) ->
                let args = List.map snd args
                let vars = List.map (fun (_,(_,v),_) -> v) ps
                let parameters = List.zip args vars
                List.fold (fun e (arg, var) -> replaceCondExpr (nameMatch var) (fun _ -> arg) e) expr parameters
              | _ -> failwith "error"
            replaceFunc isCallToFunc mapFCall
          | ActDec(_,_,ps, stmts) ->
            let isDoFunc stmt =
              match stmt with
              | SDo (_, ((_,(_,nm)), []), _) -> nm = fname  // Only inline actions from current module by match []
              | _ -> false
            let mapSDo =
              function
              | SDo(_, nm, args) ->
                let args = List.map snd args
                let vars = List.map (fun (_,(_,v),_) -> v) ps
                let parameters = List.zip args vars
                List.fold (fun s (arg, var) -> replaceCondExprInStmt (nameMatch var) (fun _ -> arg) s) stmts parameters
              | _ -> failwith "error"
            replaceAct isDoFunc mapSDo
          | _ -> failwith "error"
        List.map replacer decs
      ) decs ordering
  type ILFunDec =
    name * //name of function
    paramInfo<Position> list * //original AST paramInfo
    (annoILexpr option * ILstmt list) * //optional return value and body of ILStatements
    bool //is the function exported?

  type ILStatehandler = {
    InitMethod      : ILstmt list;
    UpdateMethod    : ILstmt list;
    OutputMethod    : ILstmt list;
    FinalizeMethod  : ILstmt list
    Functions       : ILFunDec list;
    Modules         : Map<string, ClassDeclarationSyntax>;
    Caches          : FieldDeclarationSyntax list;
  }
  let specialFuncs = ["init"; "manage"; "finalize";]
  let isSpecial f = List.contains f specialFuncs
  let isExportedDec = function Export _ -> true | _ -> false
  let functionMap (dec : dec<typ<Position>,Position>) =
    compileBuilder {
      let! (env : ILGenEnvironment<'eA>) = askRS
      match dec with
      | FunDec(_, (_, f), args, expr)
      | Export(_,FunDec(_, (_, f), args, expr)) ->
        let vars' = List.map (fun (_, (_,x), _) -> (x,Local)) args
        let! res, stmt, state =
          funConvert expr
          |> withEnv (env.extendMany vars')
          |> includeCaches
        do! putStateRS <| toCompileState state
        return Some (f, args, (res, Seq.toList stmt), isExportedDec dec)
      // init, manage and finalize in main module are handled specifically
      | ActDec(_, (_, f), _, _) when isSpecial f -> return None
      | ActDec(_, (_, f), args, stmt)
      | Export(_, ActDec(_, (_, f), args, stmt)) ->
        let env' = List.map (fun (_, (_,x), _) -> (x,Local)) args
        let! res, stmt, state =
          actConvert stmt (List.contains f env.analyser.Closures.initFuns)
          |> withEnv (env.extendMany env')
          |> includeCaches
        do! putStateRS <| toCompileState state
        return Some (f, args, (res, Seq.toList stmt), isExportedDec dec)
      | _ -> return None
    }

  let executorStateType = "ExecutorState<TFunction, TOutput>" |> SyntaxFactory.ParseTypeName
  let stateAssign = Assign((TRec ["ExecutorState"], Var "state"), (TRec ["ExecutorState"], Var "_state"))

  let verifyMethod method declaration =
    if Seq.length declaration > 0 then () else failwith (sprintf "an %s block must exists!" method)

  let generateInitMethod decs =
    let initBlockMaybe =
      List.tryFind
        (fun dec ->
          match dec with
          | ActDec (_,(_,"init"),_,_)
          | Export(_, ActDec (_,(_,"init"),_,_)) -> true
          | _ -> false
        ) decs
    compileBuilder {
      match initBlockMaybe with
      | Some(AST.ActDec (_,(_,"init"),_,stmt))
      | Some(AST.Export(_, AST.ActDec (_,(_,"init"),_,stmt))) ->
        let! stmt, state = genILStmt stmt true |> includeCaches
        do! putStateRS <| toCompileState state
        return seq {
          yield stateAssign
          yield! stmt
        }
      | _ -> return Seq.empty
    }

  let generateManageMethod decs =
    let manageBlockMaybe =
      List.tryFind
        (fun dec ->
          match dec with
          | ActDec (_,(_,"manage"),_,_)
          | Export(_, ActDec (_,(_,"manage"),_,_)) -> true
          | _ -> false
        ) decs
    compileBuilder {
      match manageBlockMaybe with
      | Some(AST.ActDec (_,(_,"manage"),_,stmt))
      | Some(AST.Export(_, AST.ActDec (_,(_,"manage"),_,stmt))) ->
          let! stmt,state = genILStmt stmt false |> includeCaches
          do! putStateRS <| toCompileState state
          return seq {
            yield stateAssign
            yield! stmt
          }
      | _ -> return Seq.empty
    }

  let mkModuleConstructor (moduleId : name) =
    let param = SyntaxFactory.Parameter(syntaxList [], SyntaxFactory.TokenList(), executorStateType, SyntaxFactory.Identifier("_state"), null)
    let paramslist = SyntaxFactory.ParameterList(sepSyntaxList [param])
    let assignment = SyntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, SyntaxFactory.IdentifierName("state"), SyntaxFactory.IdentifierName("_state"))
    let body = assignment |> SyntaxFactory.ExpressionStatement :> StatementSyntax |> List.singleton |> SyntaxFactory.Block
    SyntaxFactory
      .ConstructorDeclaration(syntaxList [], SyntaxFactory.TokenList(), SyntaxFactory.Identifier moduleId, paramslist, null, body)
      .AddModifiers([|SyntaxFactory.Token SyntaxKind.PublicKeyword|])

  let genModule (moduleId : name, declarations) =
    let moduleClassName = sprintf "%sModule" moduleId
    let modul = moduleClassName |> SyntaxFactory.ClassDeclaration
    let stateDecl = "state" |> SyntaxFactory.VariableDeclarator |> List.singleton |> sepSyntaxList
    let field = SyntaxFactory.VariableDeclaration(executorStateType, stateDecl) |> SyntaxFactory.FieldDeclaration :> MemberDeclarationSyntax
    let constructor = mkModuleConstructor moduleClassName :> MemberDeclarationSyntax
    compileBuilder {
      let! environment = askRS
      let! funcs = mapRS functionMap declarations
      let funcs = List.choose id funcs
      let funcs = List.map (fun d -> genMethod environment d :> MemberDeclarationSyntax) funcs
      return
        modul
        .AddMembers(Array.ofList <| field::constructor::funcs)
        //.AddModifiers([|SyntaxFactory.Token SyntaxKind.PublicKeyword|])
    }

  let initModule (moduleId : name) =
    let moduleType = sprintf "%sModule" moduleId |> SyntaxFactory.ParseTypeName
    let vardecl = moduleId |> sprintf "Module____%s" |> SyntaxFactory.Identifier |> SyntaxFactory.VariableDeclarator |> List.singleton |> sepSyntaxList
    let moduleField = (moduleType, vardecl) |> SyntaxFactory.VariableDeclaration |> SyntaxFactory.FieldDeclaration
    moduleField.AddModifiers([|SyntaxFactory.Token SyntaxKind.StaticKeyword|])

  let genILCode
    (environment : ILGenEnvironment<'eA>)
    (gen_ExecState : gen_executerState)
    (decs, vars, moduleDecs : Map<string, dec<typ<Position>, Position> list * environment<varPlacement>>) =

    let computation =
      compileBuilder {
        let! (env : ILGenEnvironment<'eA>) = askRS
        let folder (entity, outputHandling) =
          compileBuilder {
            let superTag = outSuperTag env.analyser entity
            let stmts_init, stmts_upd, stmts_finalize =
              List.map (genOutputHandling entity superTag) outputHandling |>
              List.unzip3
            let stmts_init = List.choose id stmts_init
            let! init    = placeOutputHandlingInContext superTag entity stmts_init true
            let! update  = placeOutputHandlingInContext superTag entity stmts_upd false
            let finalize = ILstmt.Block(stmts_finalize)
            return (init, update, finalize)
          }
        // Unpacks output handling to be used in both init, update, and finalize
        let! res = mapRS folder (Map.toList gen_ExecState.output_handling)
        let (output_init, output_update, output_finalize) = List.unzip3 res

        let! update = generateManageMethod decs
        do verifyMethod "manage" update

        let finalize = stateAssign::output_finalize
        let! functions = mapRS functionMap decs
        let functions = List.choose id functions

        let declareCache (key, (ae,_)) =
          match ae with
          | (typ, Var name) ->
            SyntaxFactory.FieldDeclaration(
              SyntaxFactory.VariableDeclaration(
                genTyp env.analyser typ typeIdentifierOptions,
                sepSyntaxList [SyntaxFactory.VariableDeclarator name]
              )
            )
          | _ -> failwith "Tried to cache non-var expression"

        let declareGlobalConst (name : string, (typ,_)) =
          SyntaxFactory.FieldDeclaration(
            SyntaxFactory.VariableDeclaration(
              genTyp env.analyser typ typeIdentifierOptions,
              sepSyntaxList [SyntaxFactory.VariableDeclarator name]
            )
          ).AddModifiers([|SyntaxFactory.Token SyntaxKind.StaticKeyword|])

        let moduleMap (moduleID, (declarations, variables)) =
          compileBuilder {
            let! result = genModule (moduleID, declarations)
            return moduleID, result
          } |> withEnv (environment.extendMany variables)
        let! ilModuleDecs = mapRS moduleMap (Map.toList moduleDecs)

        let! (compileState : compileState) = getStateRS

        let! init = compileBuilder {
          let! body = generateInitMethod decs
          do verifyMethod "init" body
          let projCount = TInt, Access((TArray(TDouble),Access((TRec ["Global"], accessState "Global"), "ProjectionTimes")), "Length")
          let projAmount =
            If ( includeTimeZeroOutput
               , Assign((TInt, Var "___NumberOfProjs"), (TInt, Binop( Plus, projCount, (TInt, CstI 1))))
               , Assign((TInt, Var "___NumberOfProjs"), projCount)
               )
          let counter = Declare(TInt, "___Counter")
          //let! (compileState : compileState) = getStateRS
          let cacheStmts = List.collect (snd >> snd) <| Map.toList compileState.filterCaches
          let globalConst = List.map (fun (n,e) -> Assign((fst e, Var n), e) ) compileState.globalConstants
          let moduleInstances =
            moduleDecs |> Map.toList |>
            List.map (fst >> fun moduleId ->
              let moduleType = TCsObj(sprintf "%sModule" moduleId)
              Assign((moduleType, Var (sprintf "Module____%s" moduleId)),
                    (moduleType, Init(moduleType, [TUnit, Var "_state"]))))
          return seq {
            yield! globalConst
            yield! moduleInstances
            yield! cacheStmts
            yield! body
            yield projAmount
            yield counter
            yield! output_init
          }

        }
        return
          {
            InitMethod = Seq.toList init;
            UpdateMethod = Seq.toList update;
            OutputMethod = output_update;
            FinalizeMethod = Seq.toList finalize;
            Functions = functions;
            (* ModuleClass = Map<string, funcs> ~ Class list *)
            Modules = Map.ofList ilModuleDecs
            Caches = (List.map declareCache <| Map.toList compileState.filterCaches) @ List.map declareGlobalConst compileState.globalConstants
          }
      }
      //
    computation (environment.extendMany vars) { filterCaches = Map.empty ; globalConstants = [] } |> fst

  let genCSExecutor (environment : ILGenEnvironment<'eA>) (ilExecutor : ILStatehandler) =
    let executor = SyntaxFactory.ParseMemberDeclaration(Template.executorTemplate)
    let executor =
      let oldInitMethod = (getMethod executor "Initialize")
      let stmts' = compStmts ilExecutor.InitMethod environment
      let initMethod = oldInitMethod.AddBodyStatements(Array.ofList stmts')
      let cacheDecls = ilExecutor.Caches |> List.map (fun e -> e :> SyntaxNode)
      let moduleDeclarations =
        ilExecutor.Modules |> Map.toList |>
        List.map (fst >> initModule >> fun s -> s :> SyntaxNode)
      executor.ReplaceNode (oldInitMethod, cacheDecls@moduleDeclarations@[(initMethod :> SyntaxNode)])

    let executor =
      let oldUpdateMethod = (getMethod executor "Update")
      let stmts' = compStmts ilExecutor.UpdateMethod environment
      let updateMethod = oldUpdateMethod.AddBodyStatements(Array.ofList stmts')
      executor.ReplaceNode (oldUpdateMethod, updateMethod)

    let executor =
      let oldOutputMethod = (getMethod executor "AddOutput")
      let stmts' = compStmts ilExecutor.OutputMethod environment
      let outputMethod = oldOutputMethod.AddBodyStatements(Array.ofList stmts')
      executor.ReplaceNode (oldOutputMethod, outputMethod)


    let executor =
      let oldUpdateMethod = (getMethod executor "Finalize")
      let stmts' = compStmts ilExecutor.FinalizeMethod environment
      //var polGrpCollection = new Dictionary<string, string>();
      //for (var i = 0; i < state.Policies.Count(); i++)
      //{
      //    var pol = state.Policies[i];
      //    //StringBuilder sb = new StringBuilder(\"\", 50);
      //    var res = \"\";
      //    for (var j = 0; j < pol.Groups.Count(); j++)
      //    {
      //      if (j != 0) {
      //        //sb.Append(\",\");
      //        res += \",\";
      //      }
      //      res += pol.Groups[j].Name;
      //      //sb.Append(pol.Groups[j].Name);
      //    }
      //    polGrpCollection.Add(pol.Name, sb.ToString());
      //}
      let updateMethod =
        oldUpdateMethod.AddBodyStatements(Array.ofList (stmts' @ [
          SyntaxFactory.ParseStatement("
              {
                return outputFactory.Create(outputTimes, cashFlowOutput, scalarOutput, state.annotations);
              }")]))
      executor.ReplaceNode (oldUpdateMethod, updateMethod)

    let executor =
        let oldParams = (SyntaxUtil.getMethod executor "Params")
        let newParamMethod =
          Map.fold(fun (method : MethodDeclarationSyntax) key fields ->
            if tagIsSubOf environment.analyser.DataStructure.superTypes key "Param"
            then
               Map.fold (fun method field _ -> method.AddBodyStatements([| SyntaxFactory.ParseStatement(sprintf "int %s_%s = 0;" key field) |]) ) method fields
            else method
           ) oldParams environment.analyser.DataStructure.totalMap
        executor.ReplaceNode (oldParams, newParamMethod)

    let methods = List.map (fun d -> genMethod environment d :> SyntaxNode) ilExecutor.Functions
    let modules = List.map (snd >> fun d -> d :> SyntaxNode) (Map.toList ilExecutor.Modules)
    let lastMethod = (getMethod executor "Finalize")
    executor.InsertNodesAfter(lastMethod, methods@modules)

  let wrapupCSharp (environment : ILGenEnvironment<'eA>) classDec executor =
    let sourceName = environment.compilerOptions.outName
    let program = SyntaxFactory.CompilationUnit()
    let addImport (prog : CompilationUnitSyntax) name = prog.AddUsings(SyntaxFactory.UsingDirective(SyntaxFactory.ParseName(name)))
    let imports =
      [ "Gen"
      ; "Stubs"
      ; "System"
      ; "System.Diagnostics"
      ; "System.Linq"
      ; "System.Text"
      ; "System.Numerics"
      ; "System.Threading.Tasks"
      ; "System.Collections.Generic"
      ; "System.Collections.Concurrent"
      ; "ActulusServices.Shared.Projections"
      ]
    let program = List.fold addImport program imports
    let ns = SyntaxFactory.NamespaceDeclaration(SyntaxFactory.ParseName(sourceName))
    // Create class declarations
    let declarationBuild = compileBuilder {
        do! genClasses
        do! addMembersNSM <| getClass (SyntaxFactory.ParseMemberDeclaration(Template.dictExtend))
        do! addMembersNSM classDec
        do! addMembersNSM <| genAdapterExecutor environment.analyser environment.compilerOptions
        do! addMembersNSM executor
    }
    let _,ns = declarationBuild environment.analyser ns
    program.AddMembers(ns)

  /// <summary>
  /// Handles our initial and transformation on the plain AST.
  /// That is:
  ///   1. Typing
  ///   2. Pruning let bindings
  /// </summary>
  let manageAST (environment : ILGenEnvironment<'eA>) executorState =
    let ((_, decs), modules) =
      if not (environment.analyser.Errors = [])
      then failwith <| sprintf "Can't compile due to type errors %A" environment.analyser.Errors
      else environment.analyser.TypedProgram

    let baseEnv = seq {
      // Module environment
      yield! Seq.map (fun (moduleName, _) -> (moduleName, Module)) <| Map.toList modules
      yield! accessEnv
      // Builtins
      yield! Seq.map (fun (f,_) -> f, BuiltIn) BuiltIns.builtInTypes
      yield! Seq.map(fun str -> (str, ExecutorState)) executorState.stateFields
      }

    let renameAndPruneDecs decs =
      let methodDecs =
           List.choose
             (fun dec ->
               match dec with
               | FunDec(_, (_, f), _, _)
               | ActDec(_, (_, f), _, _)
               | Export(_, FunDec(_, (_, f), _, _))
               | Export(_, ActDec(_, (_, f), _, _)) -> Some (f, Local)
               | _ -> None
             ) decs
      let env = Seq.append baseEnv methodDecs
      // Both environment and modules are renamed with an identity-map
      let envRename = Seq.map (fun (x,_) -> (x,x)) env
      let decs = Rename.unVarDecs envRename decs |> inlining environment.compilerOptions
      PruneLetBindings.pruneDecs decs, env

    let moduleDecs =
      Map.toSeq modules
      |> Seq.map
          (fun (k,(_,depKind, analyser)) ->
            let dec, env = renameAndPruneDecs (snd <| fst analyser.TypedProgram)
            k, (dec, Seq.toList env)
          )
      |> Map.ofSeq

    let mainDecs, mainEnv = renameAndPruneDecs decs

    mainDecs, Seq.toList mainEnv, moduleDecs

  let createProgram (analyser : Analyser<'eA>) (compilerOptions : CompilerOptions) (executorState : gen_executerState) =
    let ILEnv = new ILGenEnvironment<'eA>([], analyser, compilerOptions)
    manageAST     ILEnv executorState |>
    genILCode     ILEnv executorState |>
    genCSExecutor ILEnv |>
    wrapupCSharp  ILEnv executorState.classDec

  let compile (analyser: Analyser<'eA>) (compilerOptions : CompilerOptions) =
    let gen_ExecState = genExecutorState analyser
    let program = createProgram analyser compilerOptions gen_ExecState
    let sourceProgram = program.NormalizeWhitespace().ToFullString()
    let buffer = System.Text.Encoding.UTF8.GetBytes(sourceProgram);
    let sourceProgramUTF8 = System.Text.Encoding.UTF8.GetString(buffer);

    let sourceDir = (sprintf "%s\\%s.cs"    compilerOptions.outPath compilerOptions.outName);
    let dllDir    = (sprintf "%s\\%s.dll"   compilerOptions.outPath compilerOptions.outName);
    let pdbDir    = (sprintf "%s\\%s.pdb"   compilerOptions.outPath compilerOptions.outName);
    let sourceLinkDir  = (sprintf "%s\\%s_sourceLink.cs"   compilerOptions.outPath compilerOptions.outName);
    use file = System.IO.File.CreateText(sourceDir)
    try
      file.WriteLine (sourceProgramUTF8)
    with
      e -> printfn "%A" e

    let option = new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary)
    let optLevel =
      match compilerOptions.optLevel with
      | ToDebug -> OptimizationLevel.Debug
      | ToRelease -> OptimizationLevel.Release

    let option = option.WithOptimizationLevel(optLevel)

    let libs =
      List.map (fun (typ:Type) -> MetadataReference.CreateFromFile(System.Reflection.Assembly.Load(typ.Assembly.FullName).Location) :> MetadataReference)
        [ typeof<Object> //mscorlib
        ; typeof<System.Console>
        ; typeof<System.Linq.Queryable> //linq
        ; typeof<System.Text.StringBuilder> //linq
        ; typeof<ISet<_>> //ISet<T>
        ; typeof<System.Collections.ArrayList>
        ; typeof<Dummy> //fma-gen
        //; typeof<Stubs.DummyCashFlowFactory> //stubs_lib
        ; typeof<IFunction> //acutulus
        ; typeof<System.Numerics.Vector> //simd
        ; typeof<System.Collections.Concurrent.ConcurrentDictionary<obj,obj>>
        ]

    let sourceText = Microsoft.CodeAnalysis.Text.SourceText.From(buffer, buffer.Length, System.Text.Encoding.UTF8, canBeEmbedded = true)

    let sourceName = compilerOptions.outName
    let program = CSharpSyntaxTree.ParseText(sourceText, new CSharpParseOptions(), path = sourceDir)
    let compilation = CSharpCompilation.Create(sprintf "%s.dll" sourceName, [program], libs, option)
    let embeddedTexts = new List<EmbeddedText>()

    let emitOptions = new EmitOptions(debugInformationFormat = DebugInformationFormat.Embedded, pdbFilePath = pdbDir);

    embeddedTexts.Add(EmbeddedText.FromSource(sourceDir, sourceText));

    let dllStream = File.Create(dllDir);
    let pbdStream = File.Create(pdbDir);
    let sourceLink = File.Create(sourceLinkDir);

    let res = compilation.Emit(dllStream, pbdStream, sourceLinkStream = sourceLink);

    sourceLink.Close();
    dllStream.Close();
    pbdStream.Close();

    if res.Success then None
    else Seq.iter (fun e -> printfn "%A" e) res.Diagnostics
         Some <| Seq.map (fun e -> sprintf "a%A" e) res.Diagnostics
