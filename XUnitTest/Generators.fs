module Generators

open itu.dk.MAL
open AST
open ASTUtil
open Structure
open FsCheck

let private minimalDecs, minimalDeps =
  let prog = MALInterface.MalFromFile "../../../../examples/" "minimal.fmal"
  let (_,typModul), deps  = prog.Analyser.TypedProgram
  in  typModul.[0..typModul.Length-2],
      Map.toList deps |>
      List.map (fun (modulName, (syn, depKind, analyser)) -> (depKind, (syn, modulName), {modul = (ImportModul({kwModule = noSyntaxAnno}, (syn, modulName)),analyser.TypedProgram |> fst |> snd); source = "dep"; dependencies = [] }))

module private Helpers =
  open Gen

  let keys = Map.toList >> List.map fst
  let csProvider = new Microsoft.CSharp.CSharpCodeProvider()
  let minimalAnalysis = (MALInterface.MalFromFile "../../../../examples/" "minimal.fmal").Analyser
  let minimalData = minimalAnalysis.DataStructure
  let safeExtend dataTag (fieldId, typ) itm =
    let typ = Printer.printTyp typ
    match Map.tryFind typ itm with
    | Some records -> Map.add typ ((dataTag, fieldId)::records) itm
    | None -> Map.add typ [dataTag, fieldId] itm
  let invertTagMap (tm : tagMap<'sA>) =
    let ftm = Map.map (fun _ -> Map.toList) tm |> Map.toList
    List.foldBack (fun (d,fs) -> List.foldBack (safeExtend d) fs) ftm Map.empty
  type placements = ToIntermediate.varPlacement
  let placeVar placement (n,t) = (n,(t,placement))
  let stateVars =
    [
    ("Policies", nos_TList (nos_TRec ["Policy"]))
    ("Groups", nos_TList (nos_TRec ["Group"]))
    ("Equities", nos_TList (nos_TRec ["Equity"]))
    ("CashFlows", nos_TList (nos_TRec ["CashFlow"]))
    ("Global", nos_TRec ["Global"])
    ("Projection", nos_TRec ["Projection"])
    ] |> List.map (placeVar placements.ExecutorState)
  let builtInVars = List.map (placeVar placements.BuiltIn) BuiltIns.builtInTypes
  type genEnv =
    {
    variables : environment<typ<Position>*ToIntermediate.varPlacement>
    subtypes  : subTypeMap
    datafields: Map<string, (pureTag * pureIdentifier) list>
    dataTypes : tagMap<Position>
    }
    static member Default = {
      variables = stateVars@builtInVars
      subtypes = Map.fold (fun sm t _ -> if not <| Map.containsKey t sm then Map.add t [] sm else sm) minimalData.subTypes minimalData.totalMap;
      datafields = invertTagMap minimalData.combinedTagMap
      dataTypes = minimalData.totalMap
    }
    member this.AddLocalVariable (v,t) = {this with variables = (v,(t, placements.Local))::this.variables}
    member this.AddManyLocalVariables vs = {this with variables = List.map (placeVar placements.Local) vs@this.variables}
    member this.ShadowVariables = lazy List.distinctBy fst this.variables
    member this.RegisterDataType d fts =
      let sts = if this.subtypes.ContainsKey d then this.subtypes else this.subtypes.Add(d, [])
      let dts = if this.dataTypes.ContainsKey d then this.dataTypes else this.dataTypes.Add(d, Map.empty)
      List.fold (fun (e : genEnv) -> e.RegisterField d) {this with subtypes = sts; dataTypes=dts} fts

    member this.AddTypeExtension(subType, extendedType) = {this with subtypes = this.subtypes.Add(extendedType, subType::this.subtypes.[extendedType])}
    member this.TypeIsSuper t = not this.subtypes.[t].IsEmpty
    member this.Types = lazy (Map.toList this.subtypes |> List.map fst)
    member this.FieldsWithType t = match Map.tryFind (Printer.printTyp t) this.datafields with Some v -> v | None -> []
    member this.RegisterField d ft = {this with datafields = safeExtend d ft this.datafields; dataTypes = Map.add d (this.dataTypes.[d].Add(ft)) this.dataTypes}
    member this.AllSubtypesOf t = let imm = this.subtypes.TryFind t |> Option.defaultValue [] in t::imm@List.collect this.AllSubtypesOf imm

  let getBuiltin name = List.find (fst >> (=) name) genEnv.Default.variables |> fun (s,(t,_)) -> Var(t, noConstSyntax, (noConstSyntax, s))
  let createMap = getBuiltin "createMap"
  let index = getBuiltin "index"

  type generatorConstants = {
    typeDepth : int;
  }

  let constants = {typeDepth = 4}

  let private includeIf pred gen = if pred then [gen] else []

  let uniqueListOf<'a> = listOf >> Gen.map List.distinct

  let constantFromType<'a> (exprCons, typ) =
    gen {
      let! value = Arb.generate<'a>
      return exprCons (typ, noConstSyntax, value)
    }

  let firstChar = '_'::['a'..'z']@['A'..'Z']
  let varChars = ['0'..'9']@firstChar
  let genId =
    let nameGen =
      gen {
        let! startChar = elements firstChar
        let! rest = elements varChars |> listOf
        return (noConstSyntax, new System.String(startChar::rest |> Array.ofList))
      }
    nameGen |> where (fun (_,name) -> not <| List.contains name keywords)

  let toIdentifier s = noConstSyntax,s
  let intSqrt = float >> sqrt >> int >> (fun d -> d-1)
  let genEnumKey = elements [kw_lumpedState; kw_lumpedStateWithSurrender; kw_state; kw_freePolicyRule]
  let genEnumValue = function
    | kw when kw = kw_lumpedState ->
      gen { let! value = elements lumpedState_Keywords in return value}
    | kw when kw = kw_lumpedStateWithSurrender ->
      gen { let! value = elements lumpedStateSurrender_Keywords in return value}
    | kw when kw = kw_state ->
      gen { let! value = elements state_Keywords in return value};
    | kw when kw = kw_freePolicyRule ->
      gen { let! value = elements freePolicy_Keywords in return value}
    | _ -> failwith "Not a legal enum keyword"

  let isNormal d = not (System.Double.IsInfinity d || System.Double.IsNaN d)
  let genDouble variables = constantFromType (CDouble, nos_TDouble) |> where (fun e -> match e with CDouble(_,_, f) -> isNormal f)
  let genBool   variables = constantFromType (CBool, nos_TBool)
  let genStr    variables = genId |> Gen.map (fun (_,s) -> CStr(nos_TStr, noConstSyntax, s))
  let genEnum k variables = gen {
    let! value = genEnumValue k
    return CEnum(nos_TEnum k, noConstSyntax, value)
  }
  let genEnum_ variables = gen {
    let! kw = genEnumKey
    return! genEnum kw variables
  }
  let genVar   (env : genEnv) = gen {
    let! value,(typ,_) = elements env.ShadowVariables.Value
    return Var(typ, noConstSyntax, (noConstSyntax, value))
  }
  let binOps =
    // Match output type constraint with BinOp and input type constraint
    let arithOps = [Plus; Minus; Mult; Div; Mod] |> List.map (fun op -> (op, Some nos_TDouble))
    let compOps = [LT; LTE; GT; GTE] |> List.map (fun op -> (op, Some nos_TDouble))
    let logicOps = [LOR; LAND] |> List.map (fun op -> (op, Some nos_TBool))
    let eqOp = (Eq, None)
    function
    | None -> eqOp::logicOps@compOps@arithOps
    | Some(TBool _) -> eqOp::logicOps@compOps
    | Some(TDouble _) -> arithOps
    | Some(TStr _) -> [Plus, Some nos_TStr]
    | _ -> failwith "No Binop produces the specified type."
  let legalBinopType = [nos_TBool; nos_TDouble; nos_TStr]
  let atomGens variables =
    [genDouble; genBool; genStr; genEnum_; genVar] |>
    List.map ((|>) variables) |>
    oneof

  let baseTypes = [nos_TBool; nos_TDouble; nos_TStr]

  let idCombination env = constant (env, [])
  let combiner generator cont =
    fun env ->
      gen {
        let! (environment',bind) = generator env
        let! (environment'',binds) = cont environment'
        return (environment'',bind::binds)
      }
  let makeContextSensitiveGens listGen contextTakingGen context =
    gen {
      let! gens = listGen (constant contextTakingGen)
      return! List.foldBack combiner gens idCombination context
    }

  let includeEnvVariablesInGen (env : genEnv) (specificType : typ<Position>) =
    let eligibleVars = List.filter (snd >> fst >> (=) specificType) env.ShadowVariables.Value
    genVar {env with variables = eligibleVars} |> includeIf (not eligibleVars.IsEmpty)

  let primTypes =
    gen { let! enumTyp = genEnumKey in return! constant <| nos_TEnum enumTyp }::
    List.map constant [nos_TBool; nos_TDouble; nos_TStr]//; nos_TReserve] //nos_IFun?

  let rec genType size =
    if size <= 0 then oneof primTypes else
    primTypes @ [
    gen { let! elmTyp = genType (size-1) in return nos_TList elmTyp }
    gen { let! elmTyp = genType (size-1) in return nos_TOption elmTyp }
    gen { let! left,right = genType (size/2) |> two in return nos_TPair left right }
    gen { let! key,value = genType (size/2) |> two in return nos_TMap key value }
    ] |> oneof

  let instanceType size specificType = gen {
    let! typ = genType size
    return Option.defaultValue typ specificType
   }

  let precendences = List.map (snd >> List.map snd) Parser.operandsInfo
  let lookupPrecedence (op : binOp) = List.findIndex (List.contains op) precendences

  let preservePrecedence precedence exp =
    let wrap = EPar(eAnnoOf exp, noParSyntax, exp)
    match exp with
    | If(_,_,_,_,_)
    | ESome(_,_,_) -> wrap
    | BinOp(_,_,bOp, _,_) -> if lookupPrecedence bOp <= precedence then wrap else exp
    | _ -> exp

  let stringToRec = List.singleton >> nos_TRec

  let nonCreatableRecs =
    BaseProgram.allEntityTypes@
    [
    "BaseEntity"; "Input"; "Param"; "Result";
    "Asset"; "AssetInput"; "Transfers"; "TimeSeries";
    "Global"; "Projection"; "GlobalInput"; "GlobalParam";
    "InterpretedDiscountFactors"; "DiscountFunction"; "ReserveSequence"; "DiscountFunctionCalculator";
    "Equity"; "EquityInput"; "EquityResult";
    "CashFlow"; "WithExpenses"; "WithoutExpenses"; "PolicyPaidExpense"; "ActualExpense";
    "Policy"; "PolicyResult"; "OneStatePolicy"; "ThreeStatePolicy"; "ThreeStateResult"; "ThreeStateState"; "ThreeStateInput";
    "LumpedStatePeriodResult"; "LumpedStateParameters"; "ICountable";
    "Group"; "ReserveGroup"; "Interest"; "Risk"; "Expense"; "ReserveGroupInput"; "GroupResult"; "MarketRateInterest"
    ]

  let isCreatableRec tag = not <| List.contains tag nonCreatableRecs

  let rec isConstrictedType = function
    | TRec(_, tags) -> List.exists (fun t -> List.contains t nonCreatableRecs) tags
    | TGeneric(_, GenericSysFun, _) -> true
    | TFun(ParametricFun _) -> true
    | TGeneric(_, GenericList, typs) -> List.exists (snd >> isConstrictedType) typs
    | _ -> false

  let functionOutputMatchesType (env :genEnv) desiredType = function
  | TFun(BIFun(inputArgs, outputType)) ->
    List.forall (not << isConstrictedType) inputArgs &&
    match desiredType, outputType with
    | TRec(_, [desiredRec]), TRec(_, [outputRec]) -> List.contains outputRec <| env.AllSubtypesOf desiredRec
    | _ -> desiredType = outputType
  | _ -> false

  let parenthesizeAddress e =
    match e with
    | Var _
    | EPar _ -> e
    | _ -> EPar(eAnnoOf e, noParSyntax, e)

  let obtainNonCreatable tag =
    let t = stringToRec tag
    let takeFirst collection =
      FCall(t, noFCallSyntax, index,
        [noListSep, Var(nos_TList t, noConstSyntax, (noConstSyntax, collection)); noListSep, CDouble(nos_TDouble, noConstSyntax, 0.0)]
      )
    List.map constant <|
    match tag with
    | "Policy" -> [takeFirst "Policies"]
    | "Group" -> [takeFirst "Groups"]
    | "CashFlow" -> [takeFirst "CashFlows"]
    | "Equity" -> [takeFirst "Equities"]
    | "ICountable" -> [Var(nos_TList <| stringToRec "Policy", noConstSyntax, (noConstSyntax, "Policies"))] //Hack to fix new ICountable interface
    | _ -> []
  let rec constrainGenToType size (env : genEnv) (specificType : typ<Position>) =
    let genGeneric ident types =
      match ident, types with
      | GenericPair, (_,l)::(_,r)::_ ->
        [gen {
          let! left = genExpr (size/2) env (Some l)
          let! right = genExpr (size/2) env (Some r)
          return Pair(nos_TPair l r, noBinaryGenericSyntax, left, right)
        }]
      | GenericOption, (_,t)::_ ->
            [
            gen { return ENone(nos_TOption t, noNoneSyntax, t) }
            gen {
              let! someVal = genExpr (size-1) env (Some t)
              return ESome(nos_TOption t, noSyntaxAnno, someVal)
            }]
      | GenericList, (_,t)::_ ->
          [gen {
            let innerSize = intSqrt size
            let! elems = genExpr innerSize env (Some t) |> listOf |> Gen.resize 3
            let elemSyntax = List.map (fun e -> (noListSep,e)) elems
            return List(nos_TList t, noUlistSyntax, elemSyntax, Some <| TGeneric(noGenericSyntax,ident,types))
          }]
      | GenericSysFun, _ -> //Verboden
        let validFields = env.FieldsWithType specificType |> List.filter (fst >> isCreatableRec)
        [gen {
          let! (dataName, fieldName) = elements validFields
          let dataTypes = dataName::env.subtypes.[dataName] |> List.map (List.singleton >> nos_TRec)
          let! chosenType = elements dataTypes
          let! source = genExpr (size-1) env (Some chosenType)
          return Proj(specificType, noProjSyntax, source, (noConstSyntax, fieldName))
        }]
      | GenericMap, (_,k)::(_,v)::_ ->
        [gen {
          let kvpType = nos_TList (TGeneric(noGenericSyntax, GenericPair, [(noListSep, k); (noListSep, v)]))
          let! kvps = genExpr size env (Some kvpType)
          return FCall(specificType, noFCallSyntax, createMap, [noListSep, kvps])
        }]
      | _ -> failwith <| sprintf "Cannot constrain type to %A" specificType
    let genRecord tag =
      gen {
        let fieldNames, fieldTypes = env.dataTypes.[tag] |> Map.toList |> List.unzip
        let innerSize = size/(max 1 fieldTypes.Length)
        let! values = Gen.collect (Some >> genExpr innerSize env) fieldTypes
        let fields = List.zip (List.map toIdentifier fieldNames) values
        return CreateRec(nos_TRec [tag], noCreateRecSyntax, toIdentifier tag, fields)
      }
    match specificType with
    | TDouble _ -> [genDouble env]
    | TBool _ -> [genBool env]
    | TStr _ -> [genStr env]
    | TEnum (_,key) -> [genEnum key env]
    | TGeneric (_, ident, types) -> genGeneric ident types
    | TRec(_, tag::_) when List.contains tag nonCreatableRecs -> obtainNonCreatable tag
    | TRec(_, [tag]) -> [genRecord tag]
    | TReserve _ ->
      let validFields = env.FieldsWithType specificType
      [gen {
        let! (dataName, fieldName) = elements validFields
        let dataTypes = dataName::env.subtypes.[dataName] |> List.map (List.singleton >> nos_TRec)
        let! chosenType = elements dataTypes
        let! source = genExpr (0) env (Some chosenType)
        return Proj(specificType, noProjSyntax, source, (noConstSyntax, fieldName))
      }]
    | _ -> []

  and primGen specificType variables =
    let valids =
      constrainGenToType 0 variables specificType @
      includeEnvVariablesInGen variables specificType
    oneof valids
  ///<summary>
  ///Constructs a generator for a random AST expression, limited by <cref>size</cref>, which possibly makes use of variables included in <cref>env</cref>.
  ///</summary>
  ///<param name="size">A limit on how many sub-expressions the expression will contain.</param>
  ///<param name="env">A list of defined variables along with their type.</param>
  ///<param name="specificType">An optinal parameter to constrain the generated expressions to a specific type.
  /// If no constraint is provided, the generator will choose a type at random.</param>
  and genExpr size (env : genEnv) (specificType : typ<Position> option)   =
    let bindingGen size env =
        gen {
          let! name = genId
          let! rhs = genExpr (size-1) env None
          return env.AddLocalVariable(snd name, eAnnoOf rhs), (noLetBindingSynax, name, rhs)
        }
    let typeStables typeInstance =
      let specificType = Some typeInstance
      // BinOp
      includeIf (List.exists ((=) typeInstance) legalBinopType)
        (gen {
          let! op,inputType = binOps specificType |> elements
          let precedence = lookupPrecedence op
          let! left = genExpr (size/2) env inputType
          let! right = genExpr (size/2) env (Some <| eAnnoOf left)
          let left, right = preservePrecedence precedence left, preservePrecedence precedence right
          return BinOp(typeInstance, noSyntaxAnno, op, left, right)
        })
      @
      // FCall
      let validFuncs = List.filter (snd >> fst >> functionOutputMatchesType env typeInstance) env.ShadowVariables.Value
      includeIf (validFuncs.Length > 0)
        (gen {
          let! (funcName, (funcTyp,_)) = elements validFuncs
          let neededArgs =
            match funcTyp with
            | TFun(BIFun(inputTypes, _)) -> List.map Some inputTypes
            | _ -> failwith "functionOutputMatchesType should ensure that only BIFuns are valid"
          let innerSize = intSqrt size
          let! argGen = Gen.collect (genExpr innerSize env) neededArgs
          let argExps = List.map (fun arg -> noListSep, arg) argGen
          let fname = Var(funcTyp, noConstSyntax, (noConstSyntax,funcName))
          return FCall(typeInstance, noFCallSyntax, fname, argExps)
        })
      @
      (
      match typeInstance with
      | TGeneric(_, GenericList, (_,t)::_) ->
        // Map
        gen {
          let innerSize = intSqrt size
          let forInGen env =
            gen {
              let! colElemTypes = nonEmptyListOf (genType innerSize)
              let colTypes = List.map (fun t -> Some <| TGeneric(noGenericSyntax, GenericList, [(noListSep, t)])) colElemTypes
              let colGen typ = Gen.zip genId (genExpr innerSize env typ)
              let! sourceCols = Gen.collect colGen colTypes
              let iterVars = List.zip (List.map (fst >> snd) sourceCols) colElemTypes
              return env.AddManyLocalVariables iterVars,(noForInSyntax, Choice1Of2 sourceCols)
            }
          let! innerEnv, sources = resize innerSize <| makeContextSensitiveGens nonEmptyListOf forInGen env
          //20% chance for loops to have a where-clause
          let whereGen = genExpr innerSize innerEnv (Some nos_TBool) |> Gen.map (fun e -> Some (noWhereSyntax, e))
          let! wOpt = frequency [(4, constant None); (1, whereGen)]
          let elemType = match typeInstance with TGeneric(_, _, (_, t)::_) -> t | _ -> failwith "Lists should have at least one inner type"
          let! body = genExpr innerSize innerEnv (Some elemType)
          return Map(typeInstance, noForComputeSyntax, sources, wOpt, body)
        }::
        // Filter
        match t with
        //| TRec (*TODO: Gen filter requires a list of commonSuperType *)
        | _ -> []
      | _ -> [])
      @
      // Proj
      let validFields = env.FieldsWithType typeInstance |> List.filter (fst >> isCreatableRec)
      includeIf (not validFields.IsEmpty)
        (gen {
          let! (dataName, fieldName) = elements validFields
          let dataTypes = dataName::env.subtypes.[dataName] |> List.map (List.singleton >> nos_TRec)
          let! chosenType = elements dataTypes
          let! source = genExpr (size-1) env (Some chosenType)
          return Proj(typeInstance, noProjSyntax, parenthesizeAddress source, (noConstSyntax, fieldName))
        })
      @ [
      // Let
      gen {
        let innerSize = intSqrt size
        let! innerVars, bindings = resize innerSize <| makeContextSensitiveGens nonEmptyListOf (bindingGen innerSize) env
        let! body = genExpr innerSize innerVars specificType
        return Let(eAnnoOf body, {kwIn = noSyntaxAnno; kwEnd = noSyntaxAnno}, bindings, body)
      };
      // If
      gen {
        let! condition = genExpr (size/3) env (Some nos_TBool)
        let! trueBranch, falseBranch = two <| genExpr (size/3) env specificType
        return If(eAnnoOf trueBranch, noIfSyntax, condition, trueBranch, falseBranch)
      }
      // Match
      gen {
        let! matchedType = env.subtypes |> Map.filter (fun _ -> List.length >> (<) 1) |> keys |> elements |> where isCreatableRec
        let matchRec = (nos_TRec [matchedType])
        let! matchOn = genExpr (size-1) env <| Some matchRec
        let subTypes = env.subtypes.[matchedType]
        let caseGen subType = gen {
          let! name = genId
          let pattern = PatTag((noSyntaxAnno, subType), name)
          let! caseExp = genExpr (size/subTypes.Length) (env.AddLocalVariable(snd name, nos_TRec [subType])) specificType
          return (noMatchCaseSyntax, pattern, caseExp)
        }
        let! cases = Gen.collect caseGen subTypes
        return Match(typeInstance, noMatchSyntax, matchOn, cases)
      }
      // EPar
      gen {
        let! exp = genExpr (size-1) env specificType
        return EPar(eAnnoOf exp, noParSyntax, exp)
      }
    ]
    gen {
      let! typeInstance = instanceType (min constants.typeDepth size) specificType
      let prim = primGen typeInstance env
      if size <= 0
      then return! prim
      else return! oneof (prim::typeStables typeInstance)
    }

  let genStmt env =
    let nonNestedStmts (env : genEnv) size =
      let acts = List.filter (snd >> fst >> function TAct _ -> true | _ -> false) env.ShadowVariables.Value
      //Do
      includeIf (acts.Length > 0)
        (gen {
          let! actName, (actType, place) = elements acts
          let paramsTypes = match actType with TAct(paramTypes) -> paramTypes | _ -> failwith "We have guarenteed all elements to be of type TAct"
          let innerSize = size/(paramsTypes.Length+1)
          let! parameters = List.map Some paramsTypes |> Gen.collect (genExpr innerSize env)
          let args = List.zip (List.replicate parameters.Length noListSep) parameters
          return env, SDo(noACallSyntax, ((actType, (noConstSyntax, actName)), []), args)
        })
      @
      // Assign
      let typeIsNotConstricted (_,(t,p)) = match t with TFun _ | TAct _ -> false | _ -> not (isConstrictedType t)
      let fieldIsNotConstricted = snd >> isConstrictedType >> not
      let hasAssignableField (_,(t,_)) =
        let stringTyp = Printer.printTyp t
        Map.containsKey stringTyp env.dataTypes &&
        List.exists fieldIsNotConstricted <| Map.toList env.dataTypes.[stringTyp]
      let (.||.) f g x = f x || g x
      let recVars = env.ShadowVariables.Value |> List.filter (hasAssignableField .||. typeIsNotConstricted)
      includeIf (recVars.Length <> 0)
        (gen {
          let! (chosenVar, (recType, place)) as recVar = elements recVars
          let srcProj = recType, (noConstSyntax, chosenVar)
          if hasAssignableField recVar then
            let! (field, fieldType) = elements (env.dataTypes.[Printer.printTyp recType] |> Map.toList) |> where fieldIsNotConstricted
            let proj = srcProj, [(fieldType, noProjSyntax, (noConstSyntax, field))]
            let! newVal = genExpr size env (Some fieldType)
            return env, SAss(noAssignSyntax, proj, newVal)
          else
            let! newVal = genExpr size env (Some recType)
            return env, SAss(noAssignSyntax, (srcProj,[]), newVal)
        })
      @ [
      // DoC#
      // Transfer
      // Overwrite
      // Let
      gen {
        let! name = genId
        let! value = genExpr (size-1) env None
        return env.AddLocalVariable(snd name, eAnnoOf value), SLet(noLetBindingSynax, name, value)
      }]
    let rec genSizedStmt env size =
      (
      if size <= 0 then nonNestedStmts env size
      else
      nonNestedStmts env size @
      [
      // Block
      gen {
        let innerSize = intSqrt size
        let! env,stmts = resize innerSize <| makeContextSensitiveGens nonEmptyListOf (fun e -> genSizedStmt e innerSize) env
        return env, SBlock(noSyntaxAnno, stmts)
      }
      // Update
      gen {
        let innerSize = intSqrt size
        let forInGen env =
          gen {
            let! colElemTypes = listOfLength 1 (genType innerSize)//Updates can only happen over a single collection
            let colTypes = List.map (fun t -> Some <| TGeneric(noGenericSyntax, GenericList, [(noListSep, t)])) colElemTypes
            let colGen typ = Gen.zip genId (genExpr innerSize env typ)
            let! sourceCols = Gen.collect colGen colTypes
            let iterVars = List.zip (List.map (fst >> snd) sourceCols) colElemTypes
            return env.AddManyLocalVariables iterVars,(noForInSyntax, Choice1Of2 sourceCols)
          }
        let! innerEnv, sources = forInGen env
        //20% chance for loops to have a where-clause
        let whereGen = genExpr innerSize innerEnv (Some nos_TBool) |> Gen.map (fun e -> Some (noWhereSyntax, e))
        let! wOpt = frequency [(4, constant None); (1, whereGen)]
        let! _,body = genSizedStmt innerEnv innerSize
        return env, SUpdate(noForComputeSyntax, sources, wOpt, body)
      }
      // Match
      gen {
        let! matchedType = env.subtypes |> Map.filter (fun _ -> List.length >> (<) 1) |> keys |> elements |> where isCreatableRec
        let matchRec = (nos_TRec [matchedType])
        let! matchOn = genExpr (size-1) env <| Some matchRec
        let subTypes = env.subtypes.[matchedType]
        let caseGen subType = gen {
          let! name = genId
          let pattern = PatTag((noSyntaxAnno, subType), name)
          let! _,caseExp = genSizedStmt (env.AddLocalVariable(snd name, nos_TRec [subType])) (size/subTypes.Length)
          return (noSwitchCaseSyntax, pattern, caseExp)
        }
        let! cases = Gen.collect caseGen subTypes
        return env, SMatch(noSwitchSyntax, matchOn, cases)
      }
      // If
      gen {
        let! condition = genExpr (size/3) env (Some nos_TBool)
        let! _,trueBranch = genSizedStmt env (size/3)
        let! falseBranch = oneof [constant None; genSizedStmt env (size/3) |> map (snd >> fun s -> Some(noSelseSyntax, s))]
        return env, SIf(noSifSyntax, condition, trueBranch, falseBranch)
      }
      ]) |> oneof
    genSizedStmt env |> sized

  let genAct nameGen (env : genEnv) =
    gen {
      let! name = nameGen |> where (fun (_,name) -> not <| List.contains name (List.map fst env.variables))
      let! inputNames = uniqueListOf genId
      let! inputTypes = listOfLength (List.length inputNames) (genType constants.typeDepth)
      let env' = env.AddManyLocalVariables(List.zip (List.map snd inputNames) inputTypes)
      let parameters = List.map2 (fun n t -> { kwColon = noSyntaxAnno; kwComma = Some noSyntaxAnno}, n, t) inputNames inputTypes
      let! _,body = genStmt env'
      return env.AddLocalVariable(snd name, TAct(inputTypes)), ActDec(noActDecSyntax, name, parameters, body)
    }
  let genFun (env : genEnv) =
    gen {
      let! name = genId |> where (fun (_,name) -> not <| List.contains name (List.map fst env.variables))
      let! inputNames = uniqueListOf genId
      let! inputTypes = listOfLength (List.length inputNames) (genType constants.typeDepth)
      let env' = env.AddManyLocalVariables(List.zip (List.map snd inputNames) inputTypes)
      let parameters = List.map2 (fun n t -> { kwColon = noSyntaxAnno; kwComma = Some noSyntaxAnno}, n, t) inputNames inputTypes
      let! body = sized <| fun s -> genExpr s env' None
      let fTyp = TFun(BIFun(inputTypes, eAnnoOf body))
      return env.AddLocalVariable(snd name, fTyp), FunDec(noFunDecSyntax, name, parameters, body)
    }

  let genData (env : genEnv) =
    gen {
      let! name = genId |> where (fun (_,name) -> csProvider.IsValidIdentifier name &&  not <| List.contains name (List.map fst <| Map.toList env.dataTypes))
      let nameTag = snd name
      let! extension = Gen.frequency [(2, constant None); (1, elements env.Types.Value |> where isCreatableRec |> Gen.map Some)]
      let superFields = extension |> Option.map (fun k -> Map.find k env.dataTypes |> Map.toList) |> Option.defaultValue []
      let! fieldNames = genId |> where (fun (_,name) -> name <> nameTag && (not <| List.contains name (List.map fst superFields)) && csProvider.IsValidIdentifier name) |> uniqueListOf
      let! fieldTypes = genType constants.typeDepth |> listOfLength fieldNames.Length
      let fields = List.map2 (fun n t -> (noColonSyntax, n, t)) fieldNames fieldTypes
      let fields' = List.map (fun i -> i,None) fields
      let env' = if extension.IsSome then env.AddTypeExtension(nameTag, extension.Value) else env
      let flatfields = superFields @ List.map (fun (_,(_,f),t) -> (f,t)) fields
      return env'.RegisterDataType nameTag flatfields, Data (noDataSyntax, { name = name; extends = extension; fields = fields' })
    }
  let genDec variables =
    [
    genFun;
    genAct genId;
    ]
    |> List.map ((|>) variables)
    |> oneof

  let genManage = genAct <| constant (noConstSyntax, "Manage")
  let genInit   = genAct <| constant (noConstSyntax, "Init")



  let genProgram =
    let vars = genEnv.Default
    gen {
      let! vars, datas = makeContextSensitiveGens listOf genData vars
      let! vars, decs = makeContextSensitiveGens listOf genDec vars
      let! _,init = genInit vars
      let! _,manage = genManage vars
      let modul = MainModul, minimalDecs@datas@init::manage::decs@[EndOfFile(noSyntaxAnno)]
      return { modul = modul; source = "Auto-generated"; dependencies = minimalDeps}
    }

module private FailureGenerators =
  open Helpers
  open ErrorMsgs
  open Printer
  let fakePos errorFunction message = errorFunction message (Missing 0) (Missing 0)
  let rec potentiallyWrongExpr size env specificType =
    let badCondIf = gen {
      let! notBoolType = genType size |> Gen.where ((<>) nos_TBool)
      let! cond = genExpr (size/3) env (Some notBoolType)
      let! tErrs, t = potentiallyWrongExpr (size/3) env specificType
      let! fErrs, f = potentiallyWrongExpr (size/3) env (Some <| eAnnoOf t)
      let err = fakePos if_expr_must_be_boolean (printTyp notBoolType)
      return err::tErrs@fErrs, If(eAnnoOf t, noIfSyntax, cond, t, f)
    }
    let differentBranches = gen {
      let! cond = genExpr (size/3) env (Some nos_TBool)
      let! t = genExpr (size/3) env specificType
      let typeOfTrueBranch = eAnnoOf t
      let! differentBranchType = genType size |> Gen.where ((<>) typeOfTrueBranch)
      let! f = genExpr (size/3) env (Some differentBranchType)
      let tt,tf = printTyp typeOfTrueBranch, printTyp differentBranchType
      let err = fakePos if_expr_same_branches (tt, tf)
      return [err], If(eAnnoOf t, noIfSyntax, cond, t, f)
    }
    let cannotMatch = gen {
      let matchableType = function TGeneric(_,GenericOption,_) | TRec(_,_) -> true | _ -> false
      let! typeToMatch = genType size |> Gen.where (not << matchableType)
      let! exprToMatch = genExpr (size-1) env <| Some typeToMatch
      let err = fakePos cannot_match (printTyp typeToMatch)
      let pattern = PatNone noSyntaxAnno
      let err2 = fakePos wrong_match_branches (printPattern pattern)
      let! caseExp = genExpr size env specificType
      let resType = eAnnoOf caseExp
      let case = (noMatchCaseSyntax, pattern, caseExp)
      return [err; err2], Match(resType, noMatchSyntax, exprToMatch, [case])
    }
    let badOper = gen {
      let! oper,inputType = Gen.elements <| binOps specificType
      let! lErrs,leftOperand = potentiallyWrongExpr (size/2) env inputType
      let! rightOperand = genExpr (size/2) env None |> Gen.where (eAnnoOf >> (<>) (eAnnoOf leftOperand))
      let err = fakePos operator_not_compatible (printBinOp oper, eAnnoOf leftOperand |> printTyp, eAnnoOf rightOperand |> printTyp)
      let resType = Typer.binOpResultType oper
      let prec = lookupPrecedence oper
      let left = preservePrecedence prec leftOperand
      let right = preservePrecedence prec rightOperand
      return err::lErrs, BinOp(resType, noSyntaxAnno, oper, left, right)
    }
    let goodExpr = genExpr size env specificType |> Gen.map (fun exp -> [],exp)
    let errorExpr = Gen.oneof [badCondIf; differentBranches; cannotMatch]
    Gen.frequency [(1, goodExpr); (1, errorExpr)]

  let genFaultyProg =
    let vars = genEnv.Default
    gen {
      let! vars, datas = makeContextSensitiveGens Gen.listOf genData vars
      let! vars, decs = makeContextSensitiveGens Gen.listOf genDec vars
      let! _,faultyFunc = genFun vars
      let! errs,faultyFunc =
        match faultyFunc with
        | FunDec(sA, ident, paraminfo, _) ->
          potentiallyWrongExpr 3 vars None |> Gen.map (fun (errs,wexp) -> errs,FunDec(sA, ident, paraminfo, wexp))
        | _ -> failwith "impossible"
      let modul = MainModul, minimalDecs@datas@faultyFunc::decs@[EndOfFile(noSyntaxAnno)]
      return errs, { modul = modul; source = "Auto-generated"; dependencies = minimalDeps}
    }

type MALGenerator =
  static member Program() = {
    new Arbitrary<program<typ<Position>, Position>>() with
      override this.Generator = Helpers.genProgram
      override this.Shrinker _ = Seq.empty
  }
  static member FaultyProgram() = {
    new Arbitrary<AnalyserTypes.typErr list * program<typ<Position>, Position>>() with
      override this.Generator = FailureGenerators.genFaultyProg
      override this.Shrinker _ = Seq.empty
  }

ignore <| Arb.register<MALGenerator>()