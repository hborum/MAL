namespace itu.dk.MAL

module FromIntermediate =
  open AST
  open Microsoft.CodeAnalysis
  open Microsoft.CodeAnalysis.CSharp
  open Microsoft.CodeAnalysis.CSharp.Syntax
  open System.Numerics
  open ToIntermediate
  open Dataflow
  open SyntaxUtil
  open NameGenerator

  let tagName = snd
  let identName (_,_, n) = n
  let semi = SyntaxFactory.Token(SyntaxKind.SemicolonToken)

  let mutable parallelCount = 0
  let mkLoopVar () =
    parallelCount <- parallelCount + 1
    sprintf "range_%d" parallelCount

  //Builders are helpers allowing us for easier ways of building
  //AST-chunks. No conversion/generation are involved in these
  let buildIdent (x: string) =
    SyntaxFactory.IdentifierName(x)

  let buildAccess e1 projName =
    SyntaxFactory.MemberAccessExpression(
        SyntaxKind.SimpleMemberAccessExpression,
        e1,
        buildIdent projName
      ):> ExpressionSyntax

  let buildAccesses (e : ExpressionSyntax) projs : ExpressionSyntax =
    List.fold buildAccess e projs

  let buildVarDec (x : string) (t : TypeSyntax) (e : ExpressionSyntax) =
    let varDeclerator =
      SyntaxFactory.VariableDeclarator(x)
        .WithInitializer(SyntaxFactory.EqualsValueClause(e))
    let list = SyntaxFactory.SeparatedList().Add(varDeclerator)
    SyntaxFactory.VariableDeclaration(t, list)

  let buildLocalVarDec (x : string) (t : TypeSyntax) (e : ExpressionSyntax) =
    SyntaxFactory.LocalDeclarationStatement
      <| buildVarDec x t e :> StatementSyntax

  let buildCallOn (e : ExpressionSyntax) x args =
    let access = buildAccess e x
    let args =
      SyntaxFactory.ArgumentList
        <| SyntaxFactory.SeparatedList (List.map SyntaxFactory.Argument args)
    SyntaxFactory.InvocationExpression(
      access
    , args
    )

  let buildIndex e1 e2 =
    let index =
      SyntaxFactory.BracketedArgumentList
        <| SyntaxFactory.SingletonSeparatedList (SyntaxFactory.Argument e2)
    SyntaxFactory.ElementAccessExpression
      ( e1
      , index
      )

  let binopKind op =
      match op with
      | Plus -> SyntaxKind.AddExpression
      | Minus -> SyntaxKind.SubtractExpression
      | Mult -> SyntaxKind.MultiplyExpression
      | Mod  -> SyntaxKind.ModuloExpression
      | Div -> SyntaxKind.DivideExpression
      | Eq -> SyntaxKind.EqualsExpression
      | LT -> SyntaxKind.LessThanExpression
      | LTE -> SyntaxKind.LessThanOrEqualExpression
      | GT -> SyntaxKind.GreaterThanExpression
      | GTE -> SyntaxKind.GreaterThanOrEqualExpression
      | LOR -> SyntaxKind.LogicalOrExpression
      | LAND -> SyntaxKind.LogicalAndExpression

  let vector_types = [TInt; TDouble]

  let notNullable = function
    | TDouble
    | TInt
    | TBool
    | TEnum _ -> true
    | _ -> false

  type typGenOptions =
    { useInterface : bool         // Generate IDictionary over Dictionary
    ; useUnionInterfaces : bool   // Generate Risk_Expense over Group
    ; addGeneric : bool           // Generate Risk<TFunction> over Risk
    ; useVirtual : bool // useVirtual
    }

  let recordsWithListFields =
    ["Transfers", ["TimePoints"; "Values"]
    ;
    ] |> Map.ofList

  let convertListFieldToArray typ e x =
    match typ with //Special handling of CashFlows being directly included in generated code
    | TRec [tag] when Map.containsKey tag recordsWithListFields && recordsWithListFields.[tag] |> List.contains x ->
      buildCallOn e "ToArray" [] :> ExpressionSyntax
    | _ -> e

  ///<summary>
  ///Returns a C#-valid type string.
  ///´unionI´ specifies whether to generate union interface names or not.
  ///</summary>
  let genTypString inh typ (option : typGenOptions) =
    let rec inner typ option =
      match typ with
      | TDouble        -> "double"
      | TInt           -> "int"
      | TBool          -> "bool"
      | TStr           -> "string"
      | TCsObj s       -> s
      | TVector (typ, Empty) -> sprintf "%s[]" <| inner typ {option with useInterface = false}
      | TVector (typ, _) ->
        if List.contains typ vector_types
        then sprintf "Vector<%s>" <| inner typ {option with useInterface = true}
        else
        if typ = TBool
        then "Vector<long>"
        else sprintf "%s[]" <| inner typ {option with useInterface = true}
      | TRec(["ThreeStateResult"])
      | TRec(["OneStateResult"]) when option.useVirtual = true -> "IPolicyIdPeriodResult"
      | TRec(pureTags)   ->
        let className =
          match pureTags with
          | tag::[] ->
            match Map.tryFind tag Constants.typeNameConversionMALtoEdlund with
            | None -> tag
            | Some converted_tag ->
              if option.useInterface = false
              then converted_tag //.Substring(1)
              else converted_tag
          | tags ->
            if option.useUnionInterfaces
            then ASTUtil.tagUnionI tags
            else ASTUtil.superOfOrT inh (List.head tags)
        if option.addGeneric && not (List.contains className Constants.nonGenericNames)
        then sprintf "%s<TFunction>" className
        else className
      | TArray(t1)     ->
          let elemType = inner t1 {option with useInterface = true}
          sprintf "%s[]" elemType
      | TTypeSpan(t1)  ->
        sprintf "TypeSpan_%s<TFunction>" (inner t1 {option with useInterface = false ; addGeneric = false})
      | TMap(t1,t2, isReadable)     ->
        if option.useInterface && not isReadable
        then sprintf "IReadOnlyDictionary<%s,%s>" (inner t1 {option with useInterface = true}) (inner t2 {option with useInterface = true})
        else sprintf "Dictionary<%s,%s>" (inner t1 {option with useInterface = true}) (inner t2 {option with useInterface = true})
      | TFun(f)           -> innerFun f
      | TOption t     ->
        let t' = inner t {option with useInterface = true}
        if notNullable t
        then sprintf "Nullable<%s>" t'
        else t'
      | TUnit -> "void"
      | TEnum(enumName) -> enumName
      | TList(t) ->
        let t' = inner t {option with useInterface = true}
        sprintf "List<%s>" t'
      | TPair(t1,t2) -> sprintf "Tuple<%s,%s>" <| inner t1 {option with useInterface = true} <| inner t2 {option with useInterface = true}
    and innerFun f =
      match f with
      | IFun -> "TFunction"
      | SysFun(t1,t2) -> sprintf "Func<%s,%s>" (inner t1 {option with useInterface = true}) (inner t2 {option with useInterface = true})
      | BIFun(ins, out) ->
        let typs = List.foldBack (fun t s -> (inner t {option with useInterface = true}) + "," + s) ins (inner out {option with useInterface = true})
        sprintf "Func<%s>" typs
      | ParFun -> failwith "impossible"
    inner typ option

  ///<summary>
  ///Gets the element type of a list.
  ///</summary>
  let getElementType t =
    match t with
    | TArray(t1) | TVector (t1, _) | TTypeSpan(t1) -> t1
    | _ -> failwith "impossible"

  // todo why does this exists? It has something to do with array initialisation
  let rec genLeafTypString inh typ options =
    match typ with
    | TDouble        -> "double", -1
    | TInt           -> "int", -1
    | TBool          -> "bool", -1
    | TStr           -> "string", -1
    | TCsObj s       -> s , -1
    | TEnum s        -> s, -1
    | TPair (t1,t2)  -> genTypString inh typ options, -1
    | TVector (t1, _) ->
      let t, depth = genLeafTypString inh t1 options
      t , depth + 1
    | TRec(pureTags)  -> (genTypString inh typ options), -1
    | TArray(t1)     ->
      let t, depth = genLeafTypString inh t1 options
      t , depth + 1
    | TTypeSpan(t1)     ->
      sprintf "TypeSpan<%s>" <| genTypString inh t1 options, -1
      //let t, depth = genLeafTypString inh t1 options
      //t , depth + 1
    | TMap(t1,t2,isReadable) -> genTypString inh typ options, -1
    | TFun(SysFun(t1, t2)) -> sprintf "Func<%s,%s>" (genTypString inh t1 options) (genTypString inh t2 options), -1
    | TFun(IFun) -> "TFunction", -1
    | TFun(t1) -> genTypString inh typ options, -1//failwith "impossible"
    | TOption t     ->
      let t',d = genLeafTypString inh t options
      if notNullable t
      then sprintf "Nullable<%s>" t' , d
      else t' , d
    | TUnit -> "void", -1

  let genTyp (analyser : Analysis.Analyser<'eA>) typ options =
    SyntaxFactory.ParseTypeName <| genTypString analyser.DataStructure.subTypes typ options

  let typeIdentifierOptions = {useInterface = true; useUnionInterfaces = true; addGeneric = true; useVirtual = false}
  let specificTypeIdentifierOptions = {useInterface = true; useUnionInterfaces = false; addGeneric = true; useVirtual = false}
  let constructorIdentifierOptions = {useInterface = false; useUnionInterfaces = true; addGeneric = true; useVirtual = false}
  let malNameOptions = {useInterface = false; useUnionInterfaces = false; addGeneric = false; useVirtual = false}

  // Generates a type string used to identify the type. E.g. IDictionary<String, Risk<TFunction>> nm = ....
  let genTypeIdentifier (analyser : Analysis.Analyser<'eA>) typ useVirtual =
    genTyp (analyser : Analysis.Analyser<'eA>) typ {typeIdentifierOptions with useVirtual = useVirtual}

  // Generates a type string used to identify a specific, i.e. non-unioninterface, type. E.g. Risk<TFunction> nm = ....
  let genSpecificTypeIdentifier (analyser : Analysis.Analyser<'eA>) typ =
    genTyp (analyser : Analysis.Analyser<'eA>) typ specificTypeIdentifierOptions

  // Generates a type string suitable for construction. E.g. = new Dictionary<String, Risk<TFunction>>
  let genTypeConstructorIdentifier (analyser : Analysis.Analyser<'eA>) typ =
    genTyp (analyser : Analysis.Analyser<'eA>) typ constructorIdentifierOptions

  // Generates a type string suitable for construction. E.g. Risk
  let genMalName (analyser : Analysis.Analyser<'eA>) typ =
    genTyp (analyser : Analysis.Analyser<'eA>) typ malNameOptions

  let genLeafType inh typ options =
    let t,d = genLeafTypString inh typ options
    SyntaxFactory.ParseTypeName t , d

  let genLiteral kind token =
      match kind with
      | SyntaxKind.StringLiteralExpression ->
          let a = SyntaxFactory.Literal((string)token)
          SyntaxFactory.LiteralExpression(kind, a) :> ExpressionSyntax
      | _ -> SyntaxFactory.LiteralExpression(kind, SyntaxFactory.ParseToken(token)) :> ExpressionSyntax

  let genIsPattern e tag x_is =
    let desigVar = SyntaxFactory.SingleVariableDesignation(SyntaxFactory.Identifier(x_is))
    let decPattern = SyntaxFactory.DeclarationPattern(tag, desigVar)
    SyntaxFactory.IsPatternExpression(e, decPattern)

  let sepSyntaxList<'T when 'T:>SyntaxNode> fromList =
      let toList = new SeparatedSyntaxList<'T>()
      toList.AddRange fromList
  let syntaxList<'T when 'T:>SyntaxNode> fromList =
      let toList = new SyntaxList<'T>()
      toList.AddRange fromList

  let null_var = Var "null"

  let mempty = function
  | TInt -> CstI 0
  | TDouble -> CstD 0.0
  | TBool -> CstB false
  | TStr -> CstS ""
  | TEnum _ -> CstI 0
  | TCsObj _
  | TRec _    | TArray _
  | TMap _    | TFun _
  | TVector _ | TOption _
  | TPair _   | TTypeSpan _ -> null_var
  | TUnit-> failwith "unexpected type"

  let genExpr (analyser : Analysis.Analyser<'eA>) parenthesized e =
    let inh = analyser.DataStructure.subTypes
    let arrayRanks rank depth =
      List.fold
        (fun (g : SyntaxList<ArrayRankSpecifierSyntax>) _ ->
          g.Add(SyntaxFactory.ArrayRankSpecifier())) rank [0..depth]
    let rec mkVector elem_typ vect_typ (args : annoILexpr list) =
      let vl_code = inner false (TInt, vl)
      let arr_typ, depth = genLeafType inh elem_typ {useInterface = true ; useUnionInterfaces = true; addGeneric = true ; useVirtual = false}
      let arr_creation =
          let arr_ident =
            SyntaxFactory.ArrayType(
                arr_typ,
                syntaxList [
                    sepSyntaxList [
                        SyntaxFactory.OmittedArraySizeExpression() :> ExpressionSyntax
                    ] |> SyntaxFactory.ArrayRankSpecifier
                ])
          if List.length args > 1
          then List.map (inner false) args
          else
          match vect_typ with
          | Empty ->
            let arr_rank = sepSyntaxList [vl_code]
            let rank_spec = arrayRanks (syntaxList [SyntaxFactory.ArrayRankSpecifier arr_rank]) depth
            let arr_ident = SyntaxFactory.ArrayType(arr_typ, rank_spec)
            [SyntaxFactory.ArrayCreationExpression(arr_ident) :> ExpressionSyntax]
          | Repeat ->
            let elem_list =  inner false args.Head |> List.replicate (Vector<double>.Count) |> sepSyntaxList
            let arr_init = SyntaxFactory.InitializerExpression(SyntaxKind.ArrayInitializerExpression, elem_list)
            [SyntaxFactory.ArrayCreationExpression(arr_ident, arr_init) :> ExpressionSyntax]
          | TrueVector -> [inner parenthesized args.Head]
      let vectorArgs = SyntaxFactory.ArgumentList <| (sepSyntaxList <| List.map (SyntaxFactory.Argument) arr_creation)
      if List.contains elem_typ vector_types && match vect_typ with Empty -> false | _ -> true
      then
          let token = SyntaxFactory.Identifier("Vector")
          let vtyp = SyntaxFactory.GenericName(token, SyntaxFactory.TypeArgumentList <| sepSyntaxList [genTypeIdentifier analyser elem_typ false])
          SyntaxFactory.ObjectCreationExpression(vtyp, vectorArgs, null)
          :> ExpressionSyntax
      else arr_creation.Head
    and inner parenthesized ((a,e) as ae : annoILexpr) =
      let res =
        match e with
        | CstD d -> SyntaxFactory.ParseExpression(sprintf "%f" d)
        | CstI i -> SyntaxFactory.ParseExpression(sprintf "%d" i)
        | CstS s -> genLiteral SyntaxKind.StringLiteralExpression s
        | TypeOf typ ->
          let typString = genTypString inh typ typeIdentifierOptions
          SyntaxFactory.ParseExpression(sprintf "typeof(%s)" typString)
        | Filter (e1,typ) ->
          let e1' = inner true e1
          let elemTypStr = genTypString inh typ {typeIdentifierOptions with addGeneric = false}
          buildCallOn e1' (sprintf "Filter_%s" elemTypStr) [] :> ExpressionSyntax
        | ENone _ -> SyntaxFactory.ParseExpression("null")
        | ESome (e1) ->
          let e1' = inner parenthesized e1 //should (Some e1) be handled differently than compiling e1?
          e1'
          //if notNullable (fst e1) then
          //  let typ = genTypeIdentifier analyser a
          //  let args = SyntaxFactory.ArgumentList <| SyntaxFactory.SeparatedList().Add(SyntaxFactory.Argument e1')
          //  SyntaxFactory.ObjectCreationExpression(typ, args, null) :> ExpressionSyntax
          //else e1'
        | CstEnum s -> SyntaxFactory.ParseExpression(s)
        | ILnull -> SyntaxFactory.ParseExpression("null")
        | CstB b ->
          SyntaxFactory.LiteralExpression(
              if b then SyntaxKind.TrueLiteralExpression else SyntaxKind.FalseLiteralExpression
          ) :> ExpressionSyntax
        | Init(typ, args) ->
          match typ with
          | TVector (et, vt) -> mkVector et vt args
          | TArray (t) ->
            let e = SyntaxFactory.ArrayRankSpecifier( [inner true <| List.head args] |> sepSyntaxList)
            let g = new SyntaxList<ArrayRankSpecifierSyntax>()
            let arTyp, depth = genLeafType inh t {useInterface = true ; useUnionInterfaces = true; addGeneric = true ; useVirtual = false}
            let g = arrayRanks (g.Add(e)) depth
            SyntaxFactory.ArrayCreationExpression(
              SyntaxFactory.ArrayType(
                arTyp,
                g
              )
            ) :> ExpressionSyntax
          | t ->
            let args = List.map (inner true >> SyntaxFactory.Argument) args |> sepSyntaxList |> SyntaxFactory.ArgumentList
            SyntaxFactory.ObjectCreationExpression(genTypeConstructorIdentifier analyser typ, args, null) :> ExpressionSyntax
        | Var x -> buildIdent x :> ExpressionSyntax
        | Access (e,x) ->
          let e_comp = inner (x="CopyTo") e
          let e_comp = buildAccess e_comp x
          let e_comp = convertListFieldToArray (fst e) e_comp x

          let cast =
            match fst e with
            //| TRec(tags) when tags.Length > 1 ->
            //  if isSimpleType a then e_comp
            //  else
            //    SyntaxFactory.CastExpression(genTypeIdentifier analyser a, e_comp)
            //    |> SyntaxFactory.ParenthesizedExpression
            //    :> ExpressionSyntax
            | _ -> e_comp
          cast
        | Lambda(x,e1) ->
          let e1' = inner true e1
          SyntaxFactory.SimpleLambdaExpression(
              SyntaxFactory.Parameter(
                  SyntaxFactory.Identifier(x))
            ).WithExpressionBody( e1' ) :> ExpressionSyntax
        | Index (e1,e2) ->
          let e1' = inner true e1
          let e2' = inner true e2
          buildIndex e1' e2' :> ExpressionSyntax
        | Binop(op, e1, e2) ->
          let e1_comp = inner true e1
          let e2_comp = inner true e2
          SyntaxFactory.BinaryExpression(binopKind op, e1_comp, e2_comp) :> ExpressionSyntax
        | Pair(e1, e2) ->
          let t1,t2 = fst e1, fst e2
          let t1' = genTypString analyser.DataStructure.subTypes t1 typeIdentifierOptions
          let t2' = genTypString analyser.DataStructure.subTypes t2 typeIdentifierOptions
          let e1_comp = inner true e1
          let e2_comp = inner true e2
          buildCallOn <| buildIdent "Tuple" <| (sprintf "Create<%s,%s>" t1' t2') <| [e1_comp;e2_comp] :> ExpressionSyntax
        | UList(es) ->
          let es' = List.map (inner false) es |> sepSyntaxList
          let init = SyntaxFactory.InitializerExpression(SyntaxKind.ArrayInitializerExpression, es')
          let elem_typ =
            match a with
            | TArray(t) -> t
            | TTypeSpan(t) -> t
            | _ -> failwith "impossible"

          let arr_typ = genTyp analyser elem_typ {useInterface = true ; useUnionInterfaces = true; addGeneric = true ; useVirtual = false}
          let arr_ident =
            SyntaxFactory.ArrayType(
                arr_typ,
                syntaxList [
                    sepSyntaxList [
                        SyntaxFactory.OmittedArraySizeExpression() :> ExpressionSyntax
                    ] |> SyntaxFactory.ArrayRankSpecifier
                ])
          SyntaxFactory.ArrayCreationExpression(arr_ident, init) :> ExpressionSyntax
        | Call(ftyp, e1, args) ->
          let func = inner false e1
          let args =
            match fst e1 with
            | TFun (BIFun(argTs, _)) ->
                let cast (at, ag) =
                    if fst ag |> isUnionType
                    then SyntaxFactory.CastExpression(genTypeIdentifier analyser at false, inner true ag) :> ExpressionSyntax
                    else inner true ag
                List.zip argTs args |> List.map cast
            | _ -> List.map (inner true) args

          let sepArgs = sepSyntaxList <| List.map SyntaxFactory.Argument args
          match ftyp with
          | IFunc ->
              buildCallOn func "Evaluate" args :> ExpressionSyntax
          | Map ->
              let sepArgs = SyntaxFactory.BracketedArgumentList(sepArgs)
              SyntaxFactory.ElementAccessExpression(func, sepArgs) :> ExpressionSyntax
          | Other ->
              SyntaxFactory.InvocationExpression(func, SyntaxFactory.ArgumentList(sepArgs)) :> ExpressionSyntax
      if parenthesized
      then SyntaxFactory.ParenthesizedExpression(res) :> ExpressionSyntax
      else res
    inner parenthesized e
  let genSimpleAssignExpr v1 v2 = SyntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, v1, v2) |> SyntaxFactory.ExpressionStatement
  let lpar,rpar = SyntaxFactory.Token(SyntaxKind.OpenParenToken), SyntaxFactory.Token(SyntaxKind.CloseParenToken)
  let lbrac,rbrac = SyntaxFactory.Token(SyntaxKind.OpenBraceToken), SyntaxFactory.Token(SyntaxKind.CloseBraceToken)

  let genVarDec analyser (x : string) e =
    let eSyntax = genExpr analyser true e
    let tSyntax = genTypeIdentifier analyser (fst e) true
    buildVarDec x tSyntax eSyntax

  let genLocalVarDec analyser (x : string) e =
    let eSyntax = genExpr analyser false e
    match fst e with
    | TUnit ->
      SyntaxFactory.ExpressionStatement(eSyntax) :> StatementSyntax
    | _ ->
      let tSyntax = genTypeIdentifier analyser (fst e) true
      buildLocalVarDec x tSyntax eSyntax

  let rec genStmt analysis stmt (environment : ILGenEnvironment<'eA>) : StatementSyntax list =
    let genOper iterBindings body operation =
      match operation with
      | Some (_, oper) -> genStmt analysis (Block <| seq { yield! iterBindings; yield body; yield! oper}) environment
      | None -> genStmt analysis (Block <| seq { yield! iterBindings; yield body}) environment

    let analyser = environment.analyser

    let genDest iterBindings body counterName destination =
      let body, handle_ex =
        match destination with
        | Some (dest, elem, _, faulty) ->
          Block <| seq { yield iterBindings; yield! body; yield call_add (TArray(fst elem)) (TInt, Var counterName) (dest) elem }, faulty
        | None ->
          Block <| seq { yield iterBindings; yield! body}, false
      let bodyStmt = genStmt analysis body environment
      if handle_ex
      then
          let (dest, (typ, _), _, _) = destination.Value
          let value_to_be_discarded =
              call_add (typ) (TInt, Var counterName) (dest) (typ, (mempty typ))
              |> (fun s -> genStmt analysis s environment)
              |> syntaxList
              |> SyntaxFactory.Block
          let catch = SyntaxFactory.CatchClause(null, null, value_to_be_discarded)
          [SyntaxFactory.TryStatement(SyntaxFactory.Block(syntaxList bodyStmt), syntaxList [catch], null) :> StatementSyntax]
      else bodyStmt

    ///<summary>
    /// Generates a loop and its body.
    /// <param> When `reuseCounter` is `Some` then use the name in it</param>
    ///</summary>
    let genLoop newCounter =
        function
        | For (counterName, iters, stmt1, d) ->
          //let counterName = Option.defaultValue counterName newCounter
          let incrementCount =
              SyntaxFactory.AssignmentExpression(
                  SyntaxKind.AddAssignmentExpression, buildIdent counterName, genLiteral SyntaxKind.NumericLiteralExpression "1")

          let (leadX,leadE), tail = GeneralUtil.headTail iters
          //let enum_expr = genExpr analyser false leadE
          let elemType = (getElementType <| fst leadE)

          let enumBindings =
            match elemType with
            //| TRec _ -> tail // We are generating foreach so one binding handled by foreach
            | _ -> iters
          let condConst = wishName "condConst"
          
          let enumBindings = 
            List.map (fun (elem_name, e) ->
              let elemTyp = (getElementType <| fst e)
              let elemTyp_string = genTypString analyser.DataStructure.subTypes elemTyp typeIdentifierOptions
              let elemTyp_syntax = SyntaxFactory.ParseTypeName elemTyp_string
              let e_syntax       = genExpr analyser true e

              match elemTyp with
              | _ ->
                // Only building inLoop since we can index directly
                let indexer        = buildIndex e_syntax <| buildIdent counterName
                let elem_bind      = buildVarDec elem_name elemTyp_syntax indexer
                None, [elem_bind]
            ) enumBindings

          let preStatements = List.choose id <| List.map fst enumBindings
          let inLoopElems   = List.collect id <| List.map snd enumBindings

          // Generate a decleration for counter when it is not reused
          let varDec = if newCounter then buildVarDec counterName (genTypeIdentifier analyser TInt false) (SyntaxFactory.ParseExpression "0") else null

          let bodyStmt =
            //SyntaxFactory.ExpressionStatement incrementCount :> StatementSyntax ::
            //genDest [] stmt1 counterName d
            match d with
            | Some (_, oper) -> genStmt analysis (Block <| seq {yield stmt1; yield! oper}) environment
            | None -> genStmt analysis stmt1 environment

          let loop =
              let countFun = Access((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"), "counti")
              let countCall = (TInt, ILexpr.Call(funtyp.Other, (TFun (ILfunTyp.BIFun ([fst leadE], TInt)), countFun), [leadE]))
              let cond =
                TBool,
                (Binop
                  ( GT
                  , (TInt, Var condConst)
                  , (*(TInt, Binop(Plus,*) (TInt, Var counterName)(*, (TInt, CstI 1))))*)
                  )
                )
              let countFun = genExpr analyser false countCall
              let cond = genExpr analyser false cond

              SyntaxFactory.Block(
               [| buildLocalVarDec condConst (genTypeIdentifier analyser TInt false) countFun
               ; SyntaxFactory.ForStatement
                  ( varDec
                  , SyntaxFactory.SeparatedList<ExpressionSyntax>()  // initializers
                  , cond  // condition
                  , SyntaxFactory.SeparatedList<ExpressionSyntax>([incrementCount] : ExpressionSyntax list)  // incrementors
                  , SyntaxFactory.Block ((List.map (fun s -> SyntaxFactory.LocalDeclarationStatement s :> StatementSyntax) inLoopElems) @ bodyStmt)  // statement
                  ) :> StatementSyntax
                |]
              ) :> StatementSyntax

          let preAndLoop =
            let preBlock = preStatements @ [loop]
            SyntaxFactory.Block preBlock :> StatementSyntax

          let stmt =
            if environment.compilerOptions.parallelize // && Dataflow.canBeParallelized analysis stmt
            then
              let keyword = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.IntKeyword)) :> TypeSyntax
              let paramName = getName()
              let param =
                SyntaxFactory.Parameter(
                  new SyntaxList<AttributeListSyntax>(),
                  new SyntaxTokenList(),
                  keyword,
                  SyntaxFactory.Identifier paramName,
                  null)
              let countName = getName()
              let countFun = Access((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"), "count")
              let countCall = (TInt, ILexpr.Call(funtyp.Other, (TFun (ILfunTyp.BIFun ([fst leadE], TInt)), countFun), [leadE]))
              let count =
                SyntaxFactory.VariableDeclarator(countName).WithInitializer(
                  SyntaxFactory.EqualsValueClause(SyntaxFactory.CastExpression(keyword, genExpr analyser false countCall))
                )
                |> fun i -> SyntaxFactory.VariableDeclaration(genTypeIdentifier analyser TInt false, sepSyntaxList [i])
                |> SyntaxFactory.LocalDeclarationStatement :> StatementSyntax
                //genVariableDecleration inh countName countCall |> SyntaxFactory.LocalDeclarationStatement :> StatementSyntax
              let sliceName = getName()
              let slice =
                genLocalVarDec analyser sliceName <| (TInt, Binop(Div, (TInt, Var countName), (TInt, CstI 4)))
              let endName = getName()
              let endDeclarator =
                SyntaxFactory.ParseStatement (sprintf "int %s = %s*(%s+1) + (%s==3 ? %s %% 4 : 0);\n" endName sliceName paramName paramName countName)
              let paramList = sepSyntaxList [param] |> SyntaxFactory.ParameterList
              let bodyStmt =
                SyntaxFactory.ForStatement(
                  genVarDec analyser counterName (TInt, Binop(Mult, (TInt, Var paramName), (TInt, Var sliceName))),
                  sepSyntaxList [],
                  SyntaxFactory.ParseExpression <| sprintf "%s < %s" counterName endName,
                  sepSyntaxList [SyntaxFactory.ParseExpression <| sprintf "%s+=1" counterName],
                  SyntaxFactory.Block bodyStmt
                ) :> StatementSyntax

              let start = genExpr analyser  false (TInt, CstI 0)
              let exEnd = genExpr analyser  false (TInt, CstI 4)
              let bodyDelegate = SyntaxFactory.AnonymousMethodExpression(paramList, SyntaxFactory.Block [count; slice; endDeclarator; bodyStmt])
              buildCallOn (buildIdent "Parallel") "For" [start; exEnd; bodyDelegate :> ExpressionSyntax]
                |> SyntaxFactory.ExpressionStatement :> StatementSyntax
            else preAndLoop
          match d with
          | Some (init,_) ->
          let init = List.collect (fun s -> genStmt analysis s environment) init
          in init@[stmt]
          | None -> [stmt]
        | _ -> failwith "can only generate loops"
    match stmt with
    | Let (x, e, _) ->
      let varDec = genLocalVarDec environment.analyser x e
      [varDec]
    | Assign (x, e) ->
      let simplAssExpr = genSimpleAssignExpr (genExpr analyser false x) (genExpr analyser  true e)
      [simplAssExpr.WithSemicolonToken(semi):> StatementSyntax]
    | VectorLoop(counterName, vectors, stmt, dest, wrapup) ->
      let vl_code = genExpr analyser false (TInt, vl)
      let incrementor =
          SyntaxFactory.AssignmentExpression(
              SyntaxKind.AddAssignmentExpression, buildIdent counterName, vl_code)
      let iterDec = genLocalVarDec analyser counterName (TInt, CstI 0)
      let countFun = Access((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"), "count")
      let leadE = (snd <| List.head vectors)
      let countCall = (TInt, ILexpr.Call(funtyp.Other, (TFun (ILfunTyp.BIFun ([fst leadE], TInt)), countFun), [leadE]))
      let iterations = TInt,Binop(Minus, countCall, (TInt, vl))
      let counterExp = (TInt, Var counterName)
      let cond = (TBool,Binop( GTE, iterations, counterExp)) |> genExpr analyser false
      let (res_vect, elem_indexer, size, _) = dest.Value
      let initIter (iterName, sourceCollection) =
        let genBinding i =
          let elem_type =
            match fst sourceCollection with
            | TTypeSpan inner_typ | TArray inner_typ -> inner_typ
            | _ -> failwith "Vector-loops can only iterate typespans or arrays"
          let offset = (TInt, Binop(Plus, counterExp, ilInt i))
          Let(sprintf "%s_%d" iterName i, (elem_type, Index(sourceCollection, offset)), true)
        List.map genBinding [0..Vector<double>.Count-1]
      let iterInits = List.collect initIter vectors
      //let initIters =
      let transfer, res_init =
          match fst (elem_indexer 0) with
          | TVector(innertyp, _)
          | innertyp ->
              let res_typ =  TArray(innertyp)
              let res_init = genStmt analysis (Let(res_vect, (res_typ, Init(res_typ, [TInt, size])), false)) environment
              let index = TInt, Var counterName
              let assignToIndex i = Assign((innertyp, Index((res_typ, Var res_vect), (TInt, Binop(Plus, index, ilInt i)))), elem_indexer i)
              List.map assignToIndex [0..Vector<double>.Count-1], res_init
              //transfer_vector elem (res_typ, Var res_vect) index, res_init
          //| _ -> failwith "VectorLoop with non-vector result"
      let bodyStmt = genStmt analysis (Block(iterInits@stmt::transfer)) environment |> syntaxList |> SyntaxFactory.Block
      let loop =
        SyntaxFactory.ForStatement
          ( null  // declaration
          , SyntaxFactory.SeparatedList<ExpressionSyntax>()  // initializers
          , cond  // condition
          , SyntaxFactory.SeparatedList<ExpressionSyntax>([incrementor :> ExpressionSyntax])  // incrementors
          , SyntaxFactory.Block bodyStmt   // statement
          ) :> StatementSyntax
      let wrapup = genLoop false wrapup
      res_init @ iterDec :: loop :: wrapup.Tail
        //SyntaxFactory.ForEachStatement(genTyp inh elementTyp, x, , block) )
    | For _ -> genLoop true stmt
    | Foreach(counterName, key, value, source, body, dest) ->
      let ktyp, vtyp = match fst source with TMap (kt,vt,_) -> kt,vt | _ -> failwith "impossible"
      let increment = Assign((TInt, Var counterName), (TInt, Binop(Plus,((TInt, Var counterName)),(TInt, CstI 1))))
      let kvpName = wishName "kvp"
      let typString t = genTypString analyser.DataStructure.subTypes t typeIdentifierOptions
      // A bit hacky to consider KeyValuePair as a TRec
      let kvp = TRec([sprintf "KeyValuePair<%s,%s>" (typString ktyp) (typString vtyp)]), Var(kvpName)
      let kBinding = Let(key, (ktyp, Access(kvp, "Key")), false)
      let vBinding = Let(value, (vtyp, Access(kvp, "Value")), false)
      let bodyWithDest = genOper [kBinding; vBinding; increment] body dest
      let counterDecl = genStmt analysis (Let(counterName, (TInt, CstI -1), false)) environment
      let loopBody = SyntaxFactory.Block(syntaxList bodyWithDest)
      let foreach = SyntaxFactory.ForEachStatement(genTyp analyser (fst kvp) {typeIdentifierOptions with addGeneric = false}, SyntaxFactory.Identifier kvpName, genExpr analyser false source, loopBody)
      match dest with
      | Some (init, _) ->
        let init = List.collect (fun s -> genStmt analysis s environment) init
        in init @ counterDecl @ [foreach]
      | None -> counterDecl @ [foreach]
    | DoCSharp (extIdent) ->
      let stmt_str = sprintf "ExternalActions.%s(state.Equities.Filter<MAL_Equity<TFunction>>(), state.Groups.Filter<MAL_Group<TFunction>>(), state.Policies.Filter<MAL_Policy<TFunction>>());" extIdent
      [SyntaxFactory.ParseStatement(stmt_str)]
    | Match (e, branches) ->
      let x_match = genExpr analyser  true e
      let objectType = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.ObjectKeyword)) :> TypeSyntax
      let x_asObject = x_match;
      let branches' =
        List.map(
          fun (typ, x', stmt) ->
            let stmt' = genStmt analysis stmt environment
            let colon = SyntaxFactory.Token(SyntaxKind.ColonToken)
            let desig = SyntaxFactory.SingleVariableDesignation <| SyntaxFactory.Identifier x'
            let matchType = SyntaxFactory.ParseTypeName <| "@"+genTypString analyser.DataStructure.subTypes typ typeIdentifierOptions
            let pat = SyntaxFactory.CasePatternSwitchLabel(SyntaxFactory.DeclarationPattern(matchType, desig), colon)
            let label = [pat :> SwitchLabelSyntax] |> syntaxList
            let breakS = SyntaxFactory.BreakStatement():>StatementSyntax
            let stmt' = syntaxList (stmt'@ [breakS])
            SyntaxFactory.SwitchSection(label,syntaxList [SyntaxFactory.Block stmt'])
          ) branches
      let exceptArg = sepSyntaxList<ArgumentSyntax> [] |> SyntaxFactory.ArgumentList
      let except = SyntaxFactory.ThrowStatement(SyntaxFactory.ObjectCreationExpression(buildIdent "ArgumentException", exceptArg, null))
      let defSection = SyntaxFactory.SwitchSection(syntaxList [SyntaxFactory.DefaultSwitchLabel()], syntaxList [except])
      let switch = SyntaxFactory.SwitchStatement(x_asObject, syntaxList (branches'@[defSection]))
      [switch :> StatementSyntax]
    | Block stmts -> genStmts analysis stmts environment
    | SkipIfNot (e) ->
      let w = genExpr analyser  true e
      [SyntaxFactory.IfStatement(SyntaxFactory.PrefixUnaryExpression(SyntaxKind.LogicalNotExpression, w), SyntaxFactory.ContinueStatement()):>StatementSyntax]
    | BreakIfNot (e) ->
      let w = genExpr analyser  true e
      [SyntaxFactory.IfStatement(SyntaxFactory.PrefixUnaryExpression(SyntaxKind.LogicalNotExpression, w), SyntaxFactory.BreakStatement()):>StatementSyntax]
    | Declare (typ, x) ->
      let var = sepSyntaxList [SyntaxFactory.VariableDeclarator x]
      [SyntaxFactory.VariableDeclaration(genTypeIdentifier analyser typ false, var)
      |> SyntaxFactory.LocalDeclarationStatement
      :> StatementSyntax]
    | If(e, stmt1, stmt2) ->
      let conditional = genExpr analyser  true e
      let tbranch = genStmt analysis stmt1 environment |> SyntaxFactory.Block
      let fbranch = genStmt analysis stmt2 environment
      let fbranch = SyntaxFactory.ElseClause <|SyntaxFactory.Block fbranch
      [SyntaxFactory.IfStatement(conditional, tbranch, fbranch)
      :> StatementSyntax]
    | Expression e ->
      let syn = genExpr analyser  false e
      [SyntaxFactory.ExpressionStatement syn :> StatementSyntax]

  and genStmts analysis stmts (environment : ILGenEnvironment<'eA>) =
    Seq.foldBack (fun s ss -> let s' = genStmt analysis s environment in s'@ss) stmts []

  let compStmts stmts = genStmts () stmts