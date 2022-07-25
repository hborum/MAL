namespace itu.dk.MAL

open SyntaxUtil
open NameGenerator

module ToIntermediate =
  open AST
  open Structure
  open ASTUtil
  open Analysis
  open System.Numerics
  open Monads
  open CompileConstants
  open Parser

  type funtyp = IFunc | Map | Other
  type name = string

  type varPlacement =
    | Local
    | ExecutorState
    | BuiltIn
    | Module

  type VectorType =
    | Empty
    | Repeat
    | TrueVector

  // Tranlation idea list of records -> TypeSpan
  //                 list of anything -> array
  // Sometimes we need an intermediate array of record.
  // Therefore we have both types
  and ILtyp =
    | TDouble
    | TPair   of ILtyp * ILtyp
    | TEnum   of string
    | TInt
    | TBool
    | TStr
    | TRec      of pureTag list
    | TTypeSpan of ILtyp      // Translates to C# TypeSpan<T>
    | TArray    of ILtyp      // Translates to C# T[]
    | TList     of ILtyp      // Translates to C# List<T>
    | TVector   of ILtyp * VectorType
    | TMap      of ILtyp * ILtyp * bool // isReadable
    | TFun      of ILfunTyp
    | TOption   of ILtyp
    | TCsObj    of string  // Sometimes we create an intermediate C# object, we use this type for these
    | TUnit
    | TModule
  and ILfunTyp =
    | IFun
    | BIFun    of ILtyp list * ILtyp
    | SysFun   of ILtyp * ILtyp
    | ParFun

  and ILexpr =
    | CstD   of double
    | CstI   of int
    | CstB   of bool
    | CstEnum of string
    | Lambda  of string * annoILexpr
    | Pair    of annoILexpr * annoILexpr
    | Filter  of annoILexpr * ILtyp
    | ILnull
    | UList  of annoILexpr list
    | CstS   of string
    | Init   of ILtyp * annoILexpr list      // Initializes given type using args
    | Var    of name
    | Access of annoILexpr * name
    | Index  of annoILexpr * annoILexpr
    | Binop  of binOp * annoILexpr * annoILexpr
    | Call   of funtyp * annoILexpr * annoILexpr list
    | ESome  of annoILexpr
    | ENone  of ILtyp
    | TypeOf of ILtyp // This is used to generate eg: typeof(Risk)
  and annoILexpr = ILtyp * ILexpr

  let simpleTypes = [TInt; TBool; TStr; TDouble; TUnit]
  let isSimpleType t = List.contains t simpleTypes
  let isUnionType = function TRec tags -> tags.Length > 1 | _ -> false

  let ilInt i = TInt, CstI i

  type subType = string
  type Destination = (string * (int -> annoILexpr) * ILexpr * bool) option


  [<CustomEquality; NoComparison>]
  type ILstmt =
    | For        of name * (name * annoILexpr) list * ILstmt * Operation option
    | VectorLoop of name * (name * annoILexpr) list * ILstmt * Destination * ILstmt
    | Foreach    of name * name * name * annoILexpr * ILstmt * Operation option //for loops over key-value collection
    | Assign     of annoILexpr * annoILexpr
    | Let        of name * annoILexpr * bool //bool indicates true let or not (mutable)
    | Match      of annoILexpr * (ILtyp * name * ILstmt) list
    | Declare    of ILtyp * name
    | SkipIfNot  of annoILexpr
    | BreakIfNot of annoILexpr
    | Expression of annoILexpr
    | DoCSharp   of name
    | Block      of ILstmt seq
    | If         of annoILexpr * ILstmt * ILstmt

      //static member mycompare (x, y) =
      //    match x, y with
      //    | For(_, n1), For(_, n2) -> compare n1 n2
      //    | _ -> 0 // or 1 depending on which is list...

      override x.Equals(yobj) =
            match yobj with
            | :? ILstmt as y -> (x = y)
            | _ -> false

      override x.GetHashCode() = hash (x)

      //interface System.IComparable with
      //   member x.CompareTo yobj =
      //      match yobj with
      //      | :? ILstmt  as y -> ILstmt.mycompare(x, y)
      //      | _ -> invalidArg "yobj" "cannot compare value of different types"

  and Operation = ILstmt list * ILstmt list
  type ReduceOperator = name -> name -> annoILexpr -> annoILexpr -> Operation * ILtyp

  let rec isImmutable e =
     match e with
     | AST.EMissing       _ -> failwith "impossible"
     | AST.CDouble _ | AST.CBool _
     | AST.CStr    _ | AST.CEnum _
     | AST.ENone   _           -> true
     | AST.Var     _ | AST.Proj  _
     | AST.Filter  _ | AST.Let   _
     | AST.If      _ | AST.Match _
     | AST.Map     _ | AST.FCall _
     | AST.CreateRec _         -> false
     | AST.Pair  (_,_,e1,e2)   -> isImmutable e1 && isImmutable e2
     | AST.BinOp (_,_,_,e1,e2) -> isImmutable e1 && isImmutable e2
     | AST.EPar       (_,_,e1) -> isImmutable e1
     | AST.ESome (_,_,e1)      -> isImmutable e1
     | AST.List  (_,_,es,_)      -> List.forall (isImmutable << snd) es

  let isCompositeAndImmutable<'aS,'eS> (e : expr<'aS,'eS>) =
    match e with
    | AST.EMissing  _ | AST.CDouble   _
    | AST.CBool     _ | AST.CEnum     _
    | AST.Var       _ | AST.Proj      _
    | AST.Filter    _ | AST.Let       _
    | AST.If        _ | AST.Match     _
    | AST.Map       _ | AST.FCall     _
    | AST.ENone     _ | AST.CreateRec _
    | AST.BinOp     _       -> false
    | AST.CStr      _       -> true
    | AST.Pair  (_,_,e1,e2) -> isImmutable e1 && isImmutable e2
    | AST.EPar  (_,_,e1)    -> isImmutable e1
    | AST.ESome (_,_,e1)    -> isImmutable e1
    | AST.List  (_,_,es,_)    -> List.forall (isImmutable << snd) es

  let combineTags tags = List.reduce (fun t1 t2 -> t1+"_"+t2) <| List.sort tags

  let incVar x =
    Assign( (TInt, Var x)
            , (TInt, Binop( binOp.Plus
                          , (TInt, Var x)
                          , (TInt, CstI 1)
                          ))
            )

  let resize t e sz =
    t, Call( funtyp.Other
        , ( TFun ( BIFun([TInt], t))
          , Access( e
                  , "Resize" )
          )
        , [(TInt , Var sz)]
        )

  let counti = Access((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"), "counti")
  let call_counti e =
    Call( Other
        , (TFun(BIFun([fst e], TInt)), counti)
        , [e] )

  let accessAdd typ nm = Access((typ, Var nm), "Add")
  let call_add arrTyp index colNm elExpr =
    Expression ( TUnit
               , Call( Other
                     , (TUnit, accessAdd arrTyp colNm)
                     , [index; elExpr]))

  let call_add_dict arrTyp index colNm elExpr =
    let ktyp,vtyp = match fst elExpr with TPair(kt, vt) -> kt,vt | _ -> failwith "Only pairs are allowed in dictionaries"
    let key = ktyp,Access(elExpr, "Item1")
    let value = vtyp,Access(elExpr, "Item2")
    Expression ( TUnit
               , Call( Other
                     , (TUnit, accessAdd arrTyp colNm)
                     , [key; value]))

  let callIsNull e =
     (TBool, Call(Other, (TBool, Access((TRec(["BuiltIn"]), Var "BuiltIns<TFunction>"), var_isNull)), [e] ))

  let collectOperation counterName destName iter body =
    let size = call_counti iter
    let typ = TArray(fst body)
    let init = [Let(destName, (typ, Init(typ, [TInt, size])), false)]
    let operation = [call_add typ (TInt, Var counterName) destName body]
    (init, operation), typ, None

  let collectFilterOperation filterCounter destName iter body =
    let size = call_counti iter
    let typ = TArray(fst body)
    let init = [Let(destName, (typ, Init(typ, [TInt, size])), false)]
    let operation = [call_add typ (TInt,Binop(Plus,(TInt, Var filterCounter), (TInt, CstI -1))) destName body]
    (init, operation), typ, None

  let maxOfListOperation _ destName _ body =
    let typ = fst body
    let dest = typ, Var destName
    let init = [Let(destName, (TDouble, CstD 0.0), false)]
    let operation = [If((TDouble, Binop(GT,body, dest)), Assign(dest, body), Block [])  ]
    (init, operation), typ, None

  let sumOperation _ destName _ body =
    let typ = fst body
    let dest = typ, Var destName
    let init = [Let(destName, (TDouble, CstD 0.0), false)]
    let operation = [Assign(dest,(typ, Binop(Plus, dest, body)))]
    (init, operation), typ, None

  //let createMapOperation _ destName _ body =
  //  let tupleTyp = fst body
  //  let typ =
  //    match fst body with
  //    | TPair(t1,t2) -> TMap(t1,t2, true)
  //    | _ -> failwith "impossible"
  //  let dest = typ, Var destName
  //  let init = Let(destName, (typ, Init(typ, [])), false)
  //  let nm = wishName "tuple"
  //  let operation = [Let(nm, body, false) ; Expression((typ, Binop(Plus, dest, body)))]
  //  (init, operation), typ

  let allOperation _ destName _ body =
    let typ = fst body
    let dest = typ, Var destName
    let init = [Let(destName, (TBool, CstB true), false)]
    let operation = [ Assign(dest, (body))
                    ; BreakIfNot dest
                    ]
    (init, operation), typ, None

  let anyOperation _ destName _ body =
    let typ = fst body
    let dest = typ, Var destName
    let init = [Let(destName, (TBool, CstB false), false)]
    let operation = [ Assign(dest, (body))
                    ; BreakIfNot (TBool, Binop(Eq, dest, (TBool, CstB false)))
                    ]
    (init, operation), typ, None

  let firstOrNoneOperation _ destName _ body =
    let typ = fst body
    let dest = typ, Var destName
    let init = [Let(destName, (TOption typ, ENone typ), false)]
    let operation = [ Assign(dest, (body))
                    ; BreakIfNot (TBool, CstB false)
                    ]
    (init, operation), TOption typ, None

  let isEmptyOperation _ destName _ body =
    let typ = fst body
    let dest = typ, Var destName
    let init = Let(destName, (TBool, CstB true), false)
    let operation = [ Assign(dest, (TBool, CstB true))
                    ; BreakIfNot dest
                    ]
    (init, operation), typ, None

  let countOperation _ destName _ body =
    let dest = TDouble, Var destName
    let init = [Let(destName, (TDouble, CstD 0.0), false)]
    let operation = [ Assign(dest, (TInt, Binop(Plus, dest, (TInt, CstD 1.0)))) ]
    (init, operation), TDouble, None

  let avgOperation _ destName iter body =
    let typ = fst body
    let size = call_counti iter
    let dest = typ, Var destName
    let inverseSize = wishName "inverseSize"
    let init = [ Let(destName, (typ, Init(typ, [])), false) ; Let(inverseSize, (typ, Binop(Div, (TInt, CstD 1.0), (TInt, size))), false) ]
    let operation = [Assign(dest,(typ, Binop(Plus, dest, (typ, Binop(Mult, body, (TInt, Var inverseSize))))))]
    (init, operation), typ, None

  let uniqueOperation _ destName iter body =
    let typ = fst body
    let setTyp = TCsObj (sprintf "HashSet<%s>" "double" ) //to fix
    let destTyp = TArray typ
    let setName = wishName "hashSet"
    let init = [ Let(setName, (setTyp, Init(setTyp, [])), false) ]
    let postOp = Let(destName, (destTyp, Call(funtyp.Other, (TFun IFun, Access((setTyp, Var setName),"ToArray")),[]) ), false)
    (init, [Expression(destTyp, Call(funtyp.Other, (TFun IFun, Access((setTyp, Var setName),"Add")),[body]))] ), destTyp, Some postOp

  let mkMapOperation filterCounter destName iter body =
    match body with
    | _,Pair(e1,e2) ->
      let size = call_counti iter
      let kt,vt = (fst e1, fst e2)
      let typ = TMap(kt, vt, true)
      let init = [Let(destName, (typ, Init(typ, [])), false)]
      let operation = [Expression ( TUnit
                        , Call( Other
                              , (TUnit, accessAdd typ destName)
                              , [e1; e2]))]
      (init, operation), TMap(kt, vt, false), None
    | _ ->
      let size = call_counti iter
      let kt,vt = match fst body with TPair(kt,vt) -> kt,vt | _ -> failwith "unexpected"
      let typ = TMap(kt, vt, true)
      let init = [Let(destName, (typ, Init(typ, [TInt, size])), false)]
      let tupleName = wishName "tuple"
      let operation = [Let(tupleName, body, false) ;  call_add_dict typ (TInt, Binop(Plus,(TInt, Var filterCounter), (TInt, CstI -1))) destName (fst body, Var tupleName)]
      (init, operation), TMap(kt, vt, false), None

  /// <summary>
  ///  Returns the some and none branch of the cases in a match-statement on an option.
  /// </summary>
  let getSomeNoneBranch cases =
    let (_,_,e_none) = List.find (fun (_,pattern,_) -> comparePatterns pattern ExpNone) cases
                                                                                          //void can be anything
    let (_,pat,e_some) = List.find (fun (_,pattern,_) -> comparePatterns pattern (ExpSome TVoid)) cases
    let x = patternToName pat
    (e_none, e_some, x)


  let rec isPrefixOf l1 l2 =
    if List.isEmpty l1
    then true
    else
      match (l1, l2) with
      | (x::l1s, y::l2s) ->
        if x=y
        then isPrefixOf l1s l2s
        else false
      | _ -> false

  let isPrefixOfStrict l1 l2 =
    List.length l1 < List.length l2
    && isPrefixOf l1 l2

  let rec fullname acc = function
    | Var x -> x::acc
    | Access ((_,e),n) ->
      let s = n::acc
      fullname s e
    | _ -> acc

  let exprContainsVar name =
    let rec inner e =
      match e with
      | CstD _ | CstB _
      | CstI _ | CstS _
      | ILnull
      | CstEnum _ | ENone _
      | TypeOf _             -> false
      | Pair ((_,e1),(_,e2))
      | Index((_,e1),(_,e2))
      | Binop (_,(_,e1)
              , (_, e2))     -> inner e1 || inner e2
      | Var x                -> isPrefixOf name [x]
      | ESome ((_,e1))       -> inner e1
      | Init (_, es)         -> List.exists inner (List.map snd es)
      | Call (_, (_,e), aes) -> inner e || List.exists (snd >> inner) aes
      | UList elems          -> List.exists (snd >> inner) elems
      | Filter((_,e1),_)     -> inner e1
      | Access ((_,e), n) ->
        let s = fullname [n] e
        isPrefixOf name s
    inner

  let renameVar old after =
    let chkName x = if x = old then after else x
    let rec renameExpr (anno,expr) =
      let expr' =
        match expr with
        | CstD(_) | CstB(_)
        | CstS(_) | CstI(_)
        | ILnull | CstEnum _
        | TypeOf _
        | ENone _           -> expr
        | Var x             -> Var <| chkName x
        | Lambda (i,e)      -> Lambda (i,if i = old then e else renameExpr e)
        | ESome ae          -> ESome <| renameExpr ae
        | Filter(e1,typ)    -> Filter(renameExpr e1, typ)
        | Init(typ, args)   -> Init(typ, List.map renameExpr args)
        | Access(aExpr, n)  -> Access(renameExpr aExpr, n)
        | Pair(e1, e2)      -> Pair(renameExpr e1, renameExpr e2)
        | Binop(op, e1, e2) -> Binop(op, renameExpr e1, renameExpr e2)
        | Call(ft, e, args) -> Call(ft, renameExpr e, List.map renameExpr args)
        | Index(e1, e2)     -> Index(renameExpr e1, renameExpr e2)
        | UList(elems)      -> UList(List.map renameExpr elems)
      (anno, expr')
    let chkDest = Option.map (fun (s, ae, e, f) -> (s, renameExpr ae, snd <| renameExpr (TUnit, e), f))
    let rec inner = function
      | For(countName, iters, s, d) ->
        let iters' = List.map (fun (x, e) -> (chkName x, renameExpr e)) iters
        For(countName, iters', inner s,  Option.map (fun (i,op) -> (List.map inner i, List.map inner op)) d)
      | Foreach(countName, key, value, source, body, dest) ->
        Foreach( countName
               , chkName key
               , chkName value
               , renameExpr source
               , inner body
               , Option.map (fun (i,op) -> (List.map inner i, List.map inner op)) dest
               )
      | Assign(e, aExpr) ->
        Assign(renameExpr e, renameExpr aExpr)
      | VectorLoop(counter, iters, s, d, w) ->
        let iters' = List.map (fun (x, e) -> (x, renameExpr e)) iters
        VectorLoop(counter, iters', inner s, Option.map (fun (s, ae, e, f) -> (s, (fun i -> renameExpr (ae i)), snd <| renameExpr (TUnit, e), f)) d, inner w)
      | Declare(ts, x)      -> Declare(ts, chkName x)
      | Match(aExpr, cases) -> Match(renameExpr aExpr, List.map (fun (t,n,s) -> t,n,inner s) cases)
      | Let(x, aExpr, b)    -> Let(chkName x, renameExpr aExpr, b)
      | Expression(aExpr)   -> Expression(renameExpr aExpr)
      | Block stmts         -> Block <| Seq.map inner stmts
      | If(aExpr, t, f)     -> If(renameExpr aExpr, inner t, inner f)
      | SkipIfNot(aExpr)    -> SkipIfNot <| renameExpr aExpr
      | BreakIfNot(aExpr)    -> BreakIfNot <| renameExpr aExpr
      | DoCSharp(extIdent)  -> DoCSharp(extIdent)
    inner

  let rec replaceCondILExpr cond map start =
    let recurse = replaceCondILExpr cond map
    match start with
    | e when cond e -> map e
    | t,ILexpr.ESome (ae)             -> t,ESome (recurse ae)
    | t,ILexpr.Access (ae, nm)        -> t,Access(recurse ae, nm)
    | t,ILexpr.Init (t', args)        -> t,Init(t', List.map recurse args)
    | t,ILexpr.Pair (e1, e2)          -> t,Pair(recurse e1, recurse e2)
    | t,ILexpr.Index (s, i)           -> t,Index(recurse s, recurse i)
    | t,ILexpr.UList (elems)          -> t,ILexpr.UList(List.map recurse elems)
    | t,ILexpr.Binop (kind, ae1, ae2) -> t,Binop(kind, recurse ae1, recurse ae2)
    | t,ILexpr.Call (t', fname, args) -> t,Call(t', recurse fname, List.map recurse args)
    | t,ILexpr.Filter(e1, typ)        -> t,Filter(recurse e1, typ)
    | t,ILexpr.CstD _ | t,ILexpr.CstB _
    | t,ILexpr.CstS _ | t,ILexpr.CstI _
    | t,ILexpr.Var _ | t,ILnull
    | t,ILexpr.TypeOf _
    | t,ILexpr.CstEnum _ | t,ILexpr.ENone _ -> start

  let rec convertType typ =
    match typ with
    | AST.TAct (args)       -> TFun(BIFun(List.map convertType args, TUnit))
    | AST.TDouble(_)        -> TDouble
    | AST.TReserve(_)       -> TDouble
    | AST.ASTTInt           -> TInt
    | AST.TBool(_)          -> TBool
    | AST.TEnum(_,s)        -> TEnum(s)
    | AST.TStr(_)           -> TStr
    | AST.TRec(_,tags)      -> TRec(tags)
    | AST.TFun(tf)          -> TFun(convertFunTyp tf)
    | AST.TVoid             -> TUnit
    | AST.TModule(_)        -> TModule
    | TGeneric(syn,GenericPair,[_,t1 ; _,t2]) -> TPair(convertType t1, convertType t2)
    | TGeneric(syn,GenericList,[_,t]) ->
      match t with
      | AST.TRec (_,["Transfers"]) | AST.TRec (_,["Expenses"])  -> TArray(convertType t)
      //| AST.TRec (_,["ThreeStateResult"]) | AST.TRec (_,["OneStateResult"])  -> TArray(convertType t)
      | AST.TRec _ -> TTypeSpan(convertType t)
      | _          -> TArray(convertType t)
    | TGeneric(syn,GenericMap,[_,t1 ; _,t2]) -> TMap(convertType t1,convertType t2, false)
    | TGeneric(syn,GenericSysFun,[_,t1 ; _,t2]) -> TFun(SysFun(convertType t1,convertType t2))
    | TGeneric(syn,GenericOption,[_,t1 ]) -> TOption(convertType t1)
    | AST.TErr(_)
    | AST.TMissing(_)
    | TGeneric(_,GenericList,_)
    | TGeneric(_,GenericMap,_)
    | TGeneric(_,GenericPair,_)
    | TGeneric(_,GenericOption,_)
    | TGeneric(_,GenericSysFun,_)
    | TGeneric(_,GenericUnknown _,_) -> failwith "impossible"
  and convertFunTyp tf =
    match tf with
    | AST.IFun(_)            -> IFun
    | AST.BIFun(ts, t)       -> BIFun(List.map convertType ts, convertType t)
    | AST.ParametricFun(f_t) -> ParFun

  let rec allSubtypes inh tag =
      match Map.tryFind tag inh with
      | Some subs -> List.collect (allSubtypes inh) subs
      | None -> [tag]

  let convertVar env x =
    match ASTUtil.lookup env x with
    | Local -> Var x
    | ExecutorState ->
      Access((TRec(["ExecutorState"]), Var "state"), x)
    | BuiltIn when x = "log" -> Access((TUnit, Var "Math"), "Log")
    | BuiltIn when x = "pow" -> Access((TUnit, Var "Math"), "Pow")
    | BuiltIn when x = "min" -> Access((TUnit, Var "Math"), "Min")
    | BuiltIn when x = "max" -> Access((TUnit, Var "Math"), "Max")
    | BuiltIn ->
      Access((TRec(["BuiltIn"]), Var "BuiltIns<TFunction>"), x)
    | Module -> Var <| "Module____" + x

  let convertEnum s =
    match s with
    | _ when s = kw_LumpedBio          -> "LumpedState.Biometric"
    | _ when s = kw_LumpedFree         -> "LumpedState.FreePolicy"
    | _ when s = kw_LumpedSBio         -> "LumpedStateWithSurrender.Biometric"
    | _ when s = kw_LumpedSFree        -> "LumpedStateWithSurrender.FreePolicy"
    | _ when s = kw_LumpedSSurr        -> "LumpedStateWithSurrender.Surrender"
    | _ when s = kw_StateActive        -> "State.Active"
    | _ when s = kw_StateDead          -> "State.Dead"
    | _ when s = kw_StateDisabled      -> "State.Disabled"
    | _ when s = kw_FreePolicyNone     -> "FreePolicyRule.None"
    | _ when s = kw_FreePolicyScaled   -> "FreePolicyRule.Scaled"
    | _ when s = kw_FreePolicyRemoved  -> "FreePolicyRule.Removed"
    | _ -> failwith "unexpected"

  let simpleConvert env e =
    let e' =
      match e with
      | AST.Var (_,_,(_,x))   -> convertVar env x
      | AST.CDouble (_, _, d) -> CstD d
      | AST.CBool (_, _, b)   -> CstB b
      | AST.CStr (_, _, s)    -> CstS s
      | AST.CEnum(_,_,s)      -> CstEnum <| convertEnum s
      | AST.ENone(_,_,t)      -> ENone <| convertType t
      | _ -> failwith "unexpected"
    let typ = eAnnoOf e
    (convertType typ, e')

  /// <summary>
  /// Takes an AST-variable triple and a list of IL-statements.
  /// If the type of the AST-variable is 'mixed record', copies the IL-statements into a match-case branch for each type of the record.
  /// Otherwise it returns the unmodified IL-statements.
  /// </summary>
  /// <param name="triple">An AST-variable formatted as (syntaxAnno, name, type). </param>
  /// <param name="stmts">The list of IL-statements where the variable can occur. </param>
  let mkTypeDiscriminator ((_, x, types) as triple) stmts =
      match types with
      | AST.TRec(_, tags) when tags.Length > 1 ->
          let mapper tag = let matchName = getName () in ILtyp.TRec[tag], matchName, Block << Seq.rev <| Seq.map (renameVar x matchName) stmts
          [Match((convertType types, Var x), List.map mapper tags)]
      | _ -> stmts

  type IEnvironment<'eA> =
    abstract variables : environment<varPlacement>
    abstract analyser  : Analyser<'eA>
    abstract compilerOptions : CompilerOptions

  type ILGenEnvironment<'eA>(vars, analyser, options) =
    interface IEnvironment<'eA> with
      member _.variables  = vars
      member _.analyser = analyser
      member _.compilerOptions = options
    abstract member extend : string * varPlacement -> ILGenEnvironment<'eA>
    default _.extend(x, loc) = new ILGenEnvironment<'eA>(extEnv vars x loc, analyser, options)
    abstract member extendMany : (string * varPlacement) list -> ILGenEnvironment<'eA>
    default _.extendMany(vars') = new ILGenEnvironment<'eA>(vars'@vars, analyser, options)
    member _.variables  = vars
    member _.analyser = analyser
    member _.compilerOptions = options
  ///<summary>
  ///The monadic environment used during vector loop generation.
  ///'env' is the same environment used for normal IL generation.
  ///'vectorVars' is a list of variables known to be vectors.
  ///'iterationVars' is a list of iteration variables along with their
  ///source collection in annotated IL-expression form.
  ///'iterationCounter' is the name of the iteration variable of the corresponding C# loop.
  ///</summary>
  type VectorEnvironment<'eA>(ILenv : ILGenEnvironment<'eA>, vectorVars, iterationVars, iterationCounter) =
    inherit ILGenEnvironment<'eA>(ILenv.variables, ILenv.analyser, ILenv.compilerOptions)
    override _.extend(x, loc) = new VectorEnvironment<'eA>(ILenv.extend(x, loc), vectorVars, iterationVars, iterationCounter) :> ILGenEnvironment<'eA>
    member this.extendSame(x, loc) = this.extend(x, loc) :?> VectorEnvironment<'eA>
    override _.extendMany(vars') = new VectorEnvironment<'eA>(ILenv.extendMany vars', vectorVars, iterationVars, iterationCounter) :> ILGenEnvironment<'eA>
    member this.extendManySame(vars') = this.extendMany(vars') :?> VectorEnvironment<'eA>
    member _.extendVector v = new VectorEnvironment<'eA>(ILenv, v::vectorVars, iterationVars, iterationCounter)
    member _.vectorVars = vectorVars
    member _.iterationVars = iterationVars
    member _.iterationCounter = iterationCounter

  type ConstantVectors = OrderedMap<annoILexpr, name>
  ///<summary>
  ///The 'ReusableArrayInformation' (or RAI) is used to keep track of how many arrays of a specific type is used at once.
  ///This allows us to reuse arrays as pseudo-vectors, when the lifetime of it's original usage is over.
  ///</summary>
  type ReusableArrayInformation = {
    arraysInUse : int;
    maxConcurrentArrays : int;
    arrayPrefix : string
  }
  ///<summary>
  ///The monadic state used during IL generation.
  ///constantVectors is a map from constant IL-expressions to the names of vectors containing the expression.
  ///filterCaches maps a pair of a state collection and a set of filtered-types, to a cached filter-result and statements to generate it.
  ///typeToArray maps collection types to information about the reusable arrays for that type.
  ///</summary>
  type ILGenState = {
    constantVectors : ConstantVectors
    filterCaches : Map<name * Set<pureTag>, annoILexpr * ILstmt list>
    typeToArray : Map<ILtyp, ReusableArrayInformation>
    globalConstants : (name * annoILexpr) list
  }

  let arrName () = wishName "reusableArray"
  let newArrInfo () = {arraysInUse = 0; maxConcurrentArrays = 0; arrayPrefix = arrName ()}
  let emptyState =
    { constantVectors = []
    ; filterCaches    = Map.empty
    ; typeToArray     = Map.empty
    ; globalConstants = []
    }
  ///<summary>
  ///Increases the count of arraysInUse for type 'typ' by 1, and potentially updates maxConcurrentArrays.
  ///</summary>
  let incArrays typ (state : ILGenState) =
    let arrInfo = state.typeToArray.[typ]
    let newCount = arrInfo.arraysInUse + 1
    {state with
      typeToArray = Map.add typ
        {arrInfo with
          arraysInUse = newCount;
          maxConcurrentArrays = max arrInfo.maxConcurrentArrays newCount
        }
        state.typeToArray
    }

  ///<summary>
  ///Adds a new expression/vectorName pair to the state.
  ///</summary>
  let addConstantVector constant vector (state :ILGenState) =
    {state with constantVectors = (constant, vector)::state.constantVectors}

  let cacheFilter name expr genStmts (state : ILGenState) =
    {state with filterCaches = Map.add name (expr, genStmts) state.filterCaches}

  let addGlobalConstant expr =
    ILBuilder {
      let! (state : ILGenState) = getState
      match List.tryFind (fun (_,ae) -> ae = expr) state.globalConstants with
      | Some (n,_) ->
        return Var n
      | _ ->
        let typeColName = wishName "global_const_typeArray"
        do! putState <| {state with globalConstants = (typeColName, expr) :: state.globalConstants}
        return Var typeColName
    }

  ///<summary>
  ///Retrieves the ReusableArrayInformation for type 'typ'.
  ///If no information is available, creates a fresh RAI using <see cref="newArrInfo()"/> and updates the state.
  ///</summary>
  let getArrInfo typ = ILBuilder {
    let! (s : ILGenState) = getState
    match Map.tryFind typ s.typeToArray with
    | None ->
      let arrInfo = newArrInfo ()
      do! putState ({s with typeToArray = Map.add typ arrInfo s.typeToArray})
      return arrInfo
    | Some(arrInfo) ->
      return arrInfo
  }
  ///<summary>
  ///Returns the name of the next available array for type 'typ'.
  ///</summary>
  let requestArray typ =
    ILBuilder {
      let! arrInfo = getArrInfo typ
      do! modifyState <| incArrays typ
      return sprintf "%s_%d" arrInfo.arrayPrefix arrInfo.arraysInUse
    }

  let resetType (rai : ReusableArrayInformation) = {rai with arraysInUse = 0}
  let resetArrays (s : ILGenState) = {s with typeToArray = Map.map (fun _ v -> resetType v) s.typeToArray}
  let resetArraysM env state = modifyState resetArrays env state

  /// <summary>
  /// Will create an IL-index of the form e[i], unless 'e' is an iteration variable.
  /// In that case it returns the expression e_source[loop_counter + i],
  /// where e_source and loop_counter depends on the environment and state of the ILBuilder.
  /// </summary>
  let indexVector i e = ILBuilder {
    let! (env : VectorEnvironment<'eA>) = ask
    let elem_typ, v_typ = match fst e with | TVector(elem_typ, v_typ) -> elem_typ, v_typ | _ -> failwith "only vectors should be indexed using this method"
    //let elem_exp =
    return
      match snd e with
      | Var n ->
        match List.tryFind (fst >> (=) n) env.iterationVars with
        | Some (_, coll) -> elem_typ,Index(coll, (TInt, Binop(Plus,(TInt, Var env.iterationCounter) ,(TInt, CstI i))))
        | None -> elem_typ,Index(e, (TInt, CstI i))
      | _ -> elem_typ,Index(e, (TInt, CstI i))
    //if (match elem_typ,v_typ with TBool,TrueVector -> true | _ -> false)
    //then return elem_typ, Binop(GT, elem_exp, (TInt, CstI 0))
    //else return elem_exp
  }

  ///<summary>
  ///enclose collects the write-data produced by 'generator', encloses it in an IL-Block and returns the block as the result.
  ///This operation removes the original write-data from 'generator', similiar to <see cref="capture"/>.
  ///</summary>
  let enclose generator : RSWMonad<'R, ILGenState, ILstmt,  ('T * ILstmt)> =
    fun env s -> let (a, s',ss) = generator env s in ((a, Block <| Seq.rev ss), s', Seq.empty)

  let extendILEnv (env : ILGenEnvironment<'eA>) x location = new ILGenEnvironment<'eA>(extEnv env.variables x location, env.analyser, env.compilerOptions)
  let withVar x location = local (fun (e : ILGenEnvironment<'eA>) -> new ILGenEnvironment<'eA>(extEnv e.variables x location, e.analyser, e.compilerOptions))
  let withVarV x location = local (fun (e : VectorEnvironment<'eA>) -> new VectorEnvironment<'eA>(extendILEnv e x location, e.vectorVars, e.iterationVars, e.iterationCounter))
  let withEnv e = local (fun _ -> e)
  let wrap (a,ss) = (a, [], ss)
  let unwrap (a,_,ss) = (a,ss)
  let moveCst e = ILBuilder {
    let! (s : ILGenState) = getState
    let typ = fst e
    match pureLookup s.constantVectors e with
    | Some (_,n) ->
      return fun i -> e
    | None ->
      let newName = wishName "vectorConstant"
      do! modifyState <| addConstantVector e newName
      return fun i -> e
  }

  let vector =  Var "Vector<double>"
  let vl = CstI Vector<double>.Count

  let initVector typ e = (TVector typ, Init(TVector typ, e))

  /// <summary>
  /// Returns an IL-vector and a loop to generate it.
  /// The loop creates a vector using a function that generates a non-vector expression, and inserting the expression into the vector.
  /// </summary>
  /// <param name="e_gen"> The function that generates the element-expression, once given a counter-name as input. </param>
  /// <param name="typ"> The IL-typ of the expression generated by e_gen.</param>
  let AVXWrap e_gen typ =
      ILBuilder {
        let! exp,_ = e_gen 0 |> capture
        match exp with
        | _,Index((TVector(_,_),Var _) as vector_src, _) ->
          return vector_src
        | _ ->
          let! arr = requestArray typ
          let assignToIndex i elem = Assign((typ, Index(((TVector (typ, Empty), Var arr)),(TInt, CstI i))), elem)
          let! elems = mapRSW (fun i -> ILBuilder {let! elem = e_gen i in return assignToIndex i elem}) [Vector<double>.Count-1..-1..0]
          do! emit elems
          return (TVector (typ, Empty), Var arr)
      }

  let transfer_vector source dest index =
    [Expression (TUnit, Call(Other, (TFun IFun, Access(source,"CopyTo")), [dest; index]))]


  ///<summary>
  /// Compiles e with using `inner` and generates a constant binding for the result
  ///</summary>
  let makeConst inner =
    (fun e env s ->
      ILBuilder {
        let (a,e),state,preStmt = inner e env s
        let enum_var = wishName "asConst"
        let preStmt = GeneralUtil.SeqCons (Let(enum_var, (a,e), true)) preStmt
        return! ((a, Var enum_var),state,preStmt)
      }
    )

  type FlexibleRSW<'eA, 'T when 'T :> IEnvironment<'eA>> = RSWMonad<'T, ILGenState, ILstmt, annoILexpr>
  type ExpressionGenerator<'eA, 'T when 'T :> IEnvironment<'eA>> = expr<typ<Position>, Position> -> FlexibleRSW<'eA, 'T>
  /// <summary>
  /// Generates IL-representations of iterated collections, along with adding the corresponding
  /// iteration variables to the provided environment.
  /// </summary>
  /// <param name="forins"> The AST collections being iterated over </param>
  /// <param name="genFunc"> A function used to convert the AST collections to IL </param>
  /// <param name="env"> The environment to be extended </param>
  let extendWithCollectionIterables<'eA, 'T when 'T :> ILGenEnvironment<'eA>> forins (genFunc : ExpressionGenerator<'eA, 'T>) (env :'T) =
    let folder (iters, innerEnv : ILGenEnvironment<'eA>, pres) (((_,name), list)) =
      let newEnv = innerEnv.extend(name, Local)
      ILBuilder {
        let! listName,pre = capture <| genFunc list
        return GeneralUtil.SeqCons (name, listName) iters, newEnv, Seq.append pre pres
      }
      |> withEnv (newEnv :?> 'T)
    let outerFolder (iters, innerEnv) forIn =
      match forIn with
      | Choice1Of2 list ->
        ILBuilder {
          let! (res, innerEnv, pres) = foldRWS folder ((Seq.empty, innerEnv, Seq.empty)) list
          return (Seq.rev res |> Seq.toList |> Choice1Of2, pres)::iters, innerEnv
        } |> withEnv innerEnv
      | Choice2Of2 ((_,(_,k),(_,v)), elist) ->
        ILBuilder {
          let! listName,pre = capture <| genFunc elist
          let newEnv = innerEnv.extend(k, Local).extend(v, Local)// extEnv (extEnv innerEnv k Local) v Local
          return (Choice2Of2 (k,v,listName), pre)::iters, newEnv
        } |> withEnv (innerEnv :?> 'T)
    List.map snd forins
      |> foldRWS outerFolder ([], env :> ILGenEnvironment<'eA>)
      |> fmap (fun (ivars, env) -> (ivars, env :?> 'T))
      |> local (fun (e : 'T) -> e :> ILGenEnvironment<'eA>)

  /// <summary>
  /// Generates a for-loop in IL-code, which iterates over the provided lists.
  /// </summary>
  /// <param name="iterCols"> The collections the loop should iterate over. </param>
  /// <param name="wOpt"> An optional where-clause to skip iterations. </param>
  /// <param name="e_body"> The original AST-expression of the loop. </param>
  /// <param name="ilConverter"> The function used to convert 'e_body' to an IL-expression. Should be exprConvert or expr_AVX </param>
  /// <param name="operation"> An optional ReduceOperator specifying what to do with the iteration result. Will default to array insertion. </param>"
  let loopIL<'eA, 'T when 'T :> IEnvironment<'eA>> iterCols wOpt e_body (ilConverter : ExpressionGenerator<'eA, 'T>) reduction : FlexibleRSW<'eA, 'T> =
      // When filtering filter_count keeps track of
      // the number of items added during the loop
      let filterCount = wishName "filter_count"
      let doFilter = Option.isSome wOpt
      let doReduce = Option.isSome reduction
      let (inner_iter, inner_pres) = Seq.head iterCols
      let outer_iters = Seq.tail iterCols

      // The name and type of the collection
      // the loop will fill
      let mutable accumType = TUnit // Only for initial value. DestType is always Array.
      let accumName = wishName "accum"
      //updateDestTyp assumes all reduction are of form T[] -> T
      //let updateDestTyp typ = if (reduction : ReduceOperator option).IsSome then typ else TArray(typ)
      // Builds the body of the loop, handles filtering
      let e_body_builder =
        ILBuilder {
          let! predicate =
            if doFilter
            then ILBuilder {let! skip = ilConverter (wOpt.Value |> snd) in return [incVar filterCount; SkipIfNot skip;]}
            else ILBuilder {return []}
          do! emit predicate
          let! e_body' = ilConverter e_body
          //accumType <- updateDestTyp <| fst e_body'
          return e_body'
        }
      let defaultOper = if wOpt.IsSome then collectFilterOperation else collectOperation
      let update = Option.defaultValue defaultOper reduction
      let constructLoop iter destName (body, pres : ILstmt seq) innerMost =
        let counterName = wishName "counter"
        let idxName = if doFilter && innerMost then filterCount else counterName
        let pres' = pres |> Seq.rev |> Block
        match iter with
        | Choice1Of2 inneriter ->
          let outerIter = Seq.head inneriter |> snd
          let oper,typ,postOp = update idxName destName outerIter body
          if innerMost then accumType <- typ else ()
          match postOp with
          | None -> ILstmt.For(counterName, inneriter, pres', Some oper)
          | Some postOp -> Block [ILstmt.For(counterName, inneriter, pres', Some oper) ; postOp]

        | Choice2Of2 (key, value, coll) ->
          let oper,typ,postOp = update idxName destName coll body
          if innerMost then accumType <- typ else ()
          match postOp with
          | None -> ILstmt.Foreach(counterName, key, value, coll, pres', Some oper)
          | Some postOp -> Block [ILstmt.Foreach(counterName, key, value, coll, pres', Some oper) ; postOp]


      let innermostLoop =
        ILBuilder {
          let! e_res = capture e_body_builder
          let mainstmt = constructLoop inner_iter accumName e_res true
          do! emit (mainstmt::(Seq.toList inner_pres))
        }

      let loopWrapper state (iters, pre_stmts) =
        let innerOutput                  = fst state
        let outerDestName, outerDestType = wishName "inner_dest", if doReduce then accumType else TArray(fst innerOutput)
        let outerDest                    = outerDestType, Var outerDestName
        let loop                         = constructLoop iters outerDestName state false
        outerDest, seq { yield loop; yield! pre_stmts}

      ILBuilder {
        let! _,loopStmts = capture innermostLoop

        let dest = accumType,Var accumName : annoILexpr
        let endDest, loop = Seq.fold loopWrapper (dest, loopStmts) outer_iters
        do! emit (if doFilter then [Let(filterCount, (TInt, CstI 0), false)] else [])
        let flatAccum =
          if not doReduce
          then // We only need to flatten if we are not reducing
            // Flatten result if neeeded (ineffecient to use SelectMany, end size should be known).
            let idLambda = TUnit, Var "i => i"
            let flatter intermediateDest =
                accumType,Call(Other, (accumType, Access(intermediateDest, "SelectMany")), [idLambda])
            let tmp = Seq.fold (fun d _ -> flatter d) endDest (Seq.tail iterCols)
            if Seq.length iterCols > 1 // Entire length not needed
            then accumType,Call(Other, (accumType, Access(tmp, "ToArray")), [])
            else tmp
          else endDest

        // The final result where the accumulator may be wrapped
        // in a typespan
        let resType, convertRes, doNothing =
          match accumType with
          | TArray(TRec ["Transfers"]) -> TArray(TRec ["Transfers"]), true, true
          | TArray(TRec ["Expenses"]) -> TArray(TRec ["Expenses"]), true, true
          | TArray(TRec tags) -> TTypeSpan(TRec tags), true, false
          | _ -> accumType, false, false

        let! wrappedAccum =
          ILBuilder {
            let! (env : 'T) = ask
            if not convertRes
            then
              if doFilter && not doReduce
              then return resize accumType flatAccum filterCount
              else return flatAccum
            else
              let leafTags =
                match resType with
                | TArray(TRec tags)
                | TTypeSpan(TRec tags) ->
                  Seq.collect (leafTypesOf env.analyser.DataStructure.subTypes) tags
                | _ -> failwith "impossible"
              let args = Seq.map (fun tag -> TCsObj "Type",TypeOf <| TRec [tag]) leafTags
              let argsType = TArray (TCsObj "Type")
              let args = argsType, UList <| Seq.toList args

              let! typeCol = addGlobalConstant args

              if doNothing
              then return flatAccum
              else
                if doFilter && not doReduce //These should be removed
                then return resType, Init(resType, [flatAccum] )
                else return resType, Init(resType, [flatAccum] )
          }

        do! emit (loop)
        return wrappedAccum
      }

  type shouldConvert =
    | NoReason
    | Reason
    | No

  let joinReasons r1 r2 =
    if r1 = No || r2 = No
    then No
    else if r1 = Reason || r2 = Reason
    then Reason
    else NoReason

  let rec shouldSimdConvert e =
    match eAnnoOf e with
    | AST.TFun (AST.ParametricFun _ ) -> No
    | _ ->
      match e with
      | AST.BinOp (_,_,op,e1,e2) ->
        match op with
        | Div -> No
        | _   -> joinReasons Reason (joinReasons (shouldSimdConvert e1) (shouldSimdConvert e2))
      | AST.Var _ | AST.CStr _
      | AST.CBool _ | AST.CDouble _ -> NoReason
      | AST.CreateRec (_,_,_,args) ->
        List.fold (fun r (_,e) -> joinReasons r (shouldSimdConvert e)) NoReason args
      | AST.EMissing _
      | AST.ENone _             -> NoReason
      | AST.CEnum _             -> NoReason
      | AST.Pair (_,sA,e1,e2)   -> joinReasons <| shouldSimdConvert e1 <| shouldSimdConvert e2
      | AST.EPar(_,_,e) | AST.ESome(_,_,e)  -> shouldSimdConvert e
      | AST.List(_,_,elems,_) -> List.map (snd >> shouldSimdConvert) elems |> List.reduce joinReasons
      | AST.FCall(_,_,ef, args) -> joinReasons <| shouldSimdConvert ef <| List.fold (fun r (_,e) -> joinReasons r (shouldSimdConvert e)) NoReason args
      | AST.Filter(_,_,e,_)     -> shouldSimdConvert e
      | AST.If(_,_,e1,e2,e3)    -> joinReasons <| shouldSimdConvert e1 <| (joinReasons <| shouldSimdConvert e2 <| shouldSimdConvert e3)
      | AST.Let(_,_,lets,e)     -> joinReasons <| shouldSimdConvert e <| List.fold (fun r (_,_,e) -> joinReasons r (shouldSimdConvert e)) NoReason lets
      | AST.Map(_,_,forins, wOpt, body) ->
        List.fold
          (fun r (_,ch) ->
            let regular ies = List.fold (fun r (_,e) -> joinReasons r (shouldSimdConvert e)) r ies
            let single (_,e) = joinReasons r (shouldSimdConvert e)
            Choices.joinChoice regular single ch
          ) NoReason forins
      | AST.Match(_,_,swtch, cases) ->
        List.fold (fun r (_,_,e) -> joinReasons r <| shouldSimdConvert e) (shouldSimdConvert swtch) cases
      | AST.Proj(_,_,e,_) -> shouldSimdConvert e

  let releaseVectors<'eA> : RSWMonad<ILGenEnvironment<'eA>, ILGenState, ILstmt, unit> = ILBuilder {
    let! state = getState
    let mapper (k, v) = Let(v, initVector (fst k, Repeat) [k], false)
    do! List.rev state.constantVectors |> List.map mapper |> emit
    do! putState {state with constantVectors = []}
  }

  let emitVectorStatements statement_generator = mapRSW_ statement_generator [0..Vector<double>.Count-1]

  /// <summary>
  /// Forces input to TVector and avoid compile warning.
  /// </summary>
  let asTVector = function
    | TVector (et, vt) -> et,vt
    | _ -> failwith "Impossible expected TVector"

  let releaseArrays (state:ILGenState) =
    let mkArray name typ i = Let(sprintf "%s_%d" name i, (TVector(typ, Empty),Init(TVector(typ, Empty),[])), false)
    let arrs =
      seq {
        for kvp in state.typeToArray do
          for i in 0..(kvp.Value.maxConcurrentArrays-1) -> mkArray kvp.Value.arrayPrefix kvp.Key i
      }
    arrs

  let getASTVarName = function AST.Var(_,_,(_,x)) -> Some x | _ -> None

  let rec isOutputProj superTypes outputMap typ p =
    match typ with
    | TRec([tag]) ->
      if Map.containsKey tag outputMap
         && Option.isSome <| Map.tryFindKey (fun ps _ -> List.last ps = p) outputMap.[tag]
      then Some tag
      else
         match Map.tryFind tag superTypes with
         | Some (Some supTag) -> isOutputProj superTypes outputMap (TRec[supTag]) p
         | _ -> None
    | _ -> None

  let handleProj defaultGen exp =
    ILBuilder {
      let! (env : ILGenEnvironment<'eA>) = ask
      let subTypes = env.analyser.DataStructure.subTypes

      // This should be replaced with a subType Tjek
      let policyTags = "Policy"::subTypes.["Policy"]
      let inputArg = TUnit,Var("input")
      let resArg   = TUnit,Var("result")
      let isPolicyTag x = List.contains x policyTags
      let isGroupTag x = tagIsSubOf env.analyser.DataStructure.superTypes x "Group"
      let isGlobal x = match x with TRec(["Global"]) -> true | _ -> false
      let simpleInputLocation = function TRec([x]) -> x="ReserveGroup" || x="Equity" || isPolicyTag x | TRec(x) -> List.forall isPolicyTag x | _ ->  false
      let simpleResultLocation = function
        | TRec([x]) ->  x="Equity" || isPolicyTag x || isGroupTag x
        | TRec(x) -> List.forall isGroupTag x || List.forall isPolicyTag x
        | _ -> false

      let typeToInputLocation = function
        | TRec(x) when List.forall isPolicyTag x -> "PolicyIdParameters"
        | TRec(["Equity"]) -> "EquityIdParameters"
        | TRec(["ReserveGroup"]) -> "GroupWithReserveIdParameters"
        | t -> failwith <| sprintf "type %A does not contain Input" t
      let typeToResultLocation = function
        | TRec(x) when List.forall isPolicyTag x -> "PolicyIdPeriodResult"
        | TRec(x) when List.forall isGroupTag x -> "GroupIdPeriodResult"
        | TRec(["Equity"]) -> "EquityIdPeriodResult"
        | t -> failwith <| sprintf "type %A does not contain Input" t

      let outputMap = env.analyser.DataStructure.outputMap

      match exp with
      //| AST.Proj(typ,_,exp, (_,p)) when isOutput (eAnnoOf exp |> convertType) p ->
      //  return eAnnoOf exp |> convertType, CstD 42.0
      | AST.Proj(typ,_,exp, (_,"Input")) when eAnnoOf exp |> convertType |> simpleInputLocation ->
        let inputtyp = convertType typ
        let expTyp =  eAnnoOf exp |> convertType
        let inputmap = inputtyp, Access(inputArg, typeToInputLocation expTyp)
        let! obj = defaultGen exp
        let objectName = expTyp, Access(obj, "Name")
        return inputtyp, Index(inputmap, objectName)
      | AST.Proj(typ,_,AST.Proj(_,_,e,(_,"Input")),(_,globField)) when eAnnoOf e |> convertType |> isGlobal ->
        let t = convertType typ
        return t,
          match globField with
          | "FreePolicyRules" -> Access(inputArg, "CashFlowNameFreePolicyRule")
          | "BiometricScenarioTimes"
          | "EconomicScenarioTimes" -> Call(Other,(t,Access((t, Access(inputArg, globField)),"ToArray")), [])
          | "CalculationTime" -> Call(Other,(t, Access((t,Access((TRec(["IProjectionConfiguration"]), Var "config"), "CalculationDate")),"ToOADate")), [])
          | _ -> Access(inputArg, globField)
      | AST.Proj(typ,_,e,(_,"Result")) when eAnnoOf e |> convertType |> simpleResultLocation ->
          let restyp = convertType typ
          let expTyp =  eAnnoOf e |> convertType
          let resmap = restyp, Access(resArg, typeToResultLocation expTyp)
          let! obj = defaultGen e
          let objectName = expTyp, Access(obj, "Name")
          return restyp, Index(resmap, objectName)
      | AST.Proj(typ,_,e,(_,"PeriodTechnicalExpenses"))
          when isSubOfTyp env.analyser.DataStructure.superTypes (eAnnoOf e) (nos_TRec["Result"])
               || isSubOfTyp env.analyser.DataStructure.superTypes (eAnnoOf e) (nos_TRec["LumpedStatePeriodResult"])  ->
          let restyp = convertType typ
          let pteName = wishName "periodTechincalExpenses"
          let expenseTyp = TRec(["Expenses"])
          let pteVar = expenseTyp, Var pteName
          let! obj = defaultGen e
          let pte = restyp, Access(obj, "PeriodTechnicalExpenses")
          let pteInit = Let(pteName, (expenseTyp, Init(expenseTyp, [])), false)
          let transfer = Expression(TUnit, Call(Other, (TFun IFun, Var "__transferExpense"), [pte; pteVar]))
          do! emit [transfer; pteInit]
          return pteVar
      | AST.Proj(typ,_,e,(_,x)) ->
          let! e' = defaultGen e
          return convertType typ,(Access(e', x))
      | e -> return! defaultGen e //will not happen, as we only call on projs
    }

  let csharp_invoke nspace (func, typ) args =
    Call(Other, (typ, Access(nspace, func)), args)

  let vector_logic v1 v2 bop =
    let nspace = TUnit, Var "Vector"
    let operandtyp = TVector (TBool, TrueVector)
    let ftyp = TFun <| BIFun ([operandtyp; operandtyp], operandtyp)
    operandtyp, csharp_invoke nspace (bop, ftyp) [v1;v2]
  /// <summary>
  /// Converts an AST-expression to a vectorized IL-expression, in the same manner as <see cref="exprConvert">exprConvert</see>.
  /// </summary>
  /// <param name="expr"> The AST expression to be converted. </param>
  /// <param name="env"> A collection of lists keeping tabs on which variables are vectors and iteration counters </param>
  /// <param name="state"> A map of loop-constants to vector names. Should be empty initially. </param>
  let rec expr_AVX expr (env : VectorEnvironment<'eA>) state : (int -> annoILexpr) * ILGenState * ILstmt seq =
    let rec mapReduce forIns wOpt e_body reduction =
      ILBuilder {
        let! env = ask
        let listGen list = fun _ -> exprConvert list env
        let! (lists, innerEnv) = extendWithCollectionIterables forIns listGen env
        return! withEnv innerEnv <| loopIL lists wOpt e_body exprConvert reduction
      }
    and inner expr : RSWMonad<VectorEnvironment<'eA>, ILGenState, ILstmt, int -> annoILexpr> =
      let typ = eAnnoOf expr |> convertType
      let enforce_vector (typ , e) =
        let et,vt = asTVector typ
        match vt with
        | Empty -> initVector (et, TrueVector) ([typ,e])
        | _ -> typ,e
      match expr with
      | AST.CDouble (_, _, f) -> moveCst (TDouble, CstD f)
      | AST.CStr (_, _, s) -> moveCst (TStr, CstS s)
      | AST.CBool(_, _, b) -> moveCst (TBool, CstB b)
      | AST.ENone(typ,_,_) -> moveCst (convertType typ, ILnull)
      | AST.CEnum(typ,_,s) -> moveCst (convertType typ, CstEnum <| convertEnum s)
      | AST.ESome(_,_,e1)  -> inner e1
      | AST.List(typ,_,elems,_) ->
        ILBuilder {
          let! es = mapRSW (snd >> inner) elems
          return fun i -> convertType typ, UList <| Seq.toList (Seq.map ((|>) i) es)
        }
      | AST.Var (typ, _, (_,v))  ->
        ILBuilder {
          let! (vector_env : VectorEnvironment<'eA>) = ask
          let t = convertType typ
          if List.contains v vector_env.vectorVars
          then return fun i -> t,Index((TVector (t, TrueVector),Var v), ilInt i)
          else
          let iterCounter = (TInt, Var env.iterationCounter)
          match List.tryFind (fst >> (=) v) env.iterationVars with
          // Good idea in theory, but poses issues in the current compilation scheme
          //| Some(_, collection) when t = TDouble ->
          //  return fun _ -> initVector (t, TrueVector) ([collection; iterCounter])
          | Some(_, collection) ->
            return fun i -> t,Var(sprintf "%s_%d" v i)//Index(collection, (TInt, Binop(Plus, iterCounter, ilInt i)))
          | _ ->
            return! moveCst (t, convertVar env.variables v)
        }
      | AST.BinOp(restyp, _, op, e1, e2) ->
        ILBuilder {
          let! e1' = inner e1
          let! e2' = inner e2
          //let restyp = convertType restyp
          match typ with
          | TDouble ->
            let! s = getState
            let lift e i = ILBuilder { return e i }
            let getConstantOrWrap e =
              match pureLookup s.constantVectors (e 0) with
              | Some(_, vectorconstant) -> ILBuilder { return (TVector (typ, Repeat), Var vectorconstant) }
              | None -> AVXWrap (lift e) TDouble
            let! e1'' = getConstantOrWrap e1'
            let! e2'' = getConstantOrWrap e2'//AVXWrap (lift e2') TDouble
            let v1 = enforce_vector e1''
            let v2 = enforce_vector e2''
            let vtyp = TVector (typ, TrueVector)
            let res_name = wishName "vector_op"
            do! emit [Let(res_name, (vtyp, Binop(op, v1, v2)), false)]
            return fun i -> typ, Index((vtyp, Var res_name), ilInt i)
          | _ ->
            return fun i -> typ, Binop(op, e1' i, e2' i)
          //| TBool ->
          //  return
          //    vector_logic v1 v2 <|
          //    match op with
          //    | Eq ->  "Equals"
          //    | LT ->  "LessThan"
          //    | LTE -> "LessThanOrEqual"
          //    | GT ->  "GreaterThan"
          //    | GTE -> "GreaterThanOrEqual"
          //    | LOR -> "BitwiseOr"
          //    | LAND ->"BitwiseAnd"
          //    | _ -> failwith "not a boolean operator"
        }
      | AST.Pair(_, _, e1,e2) ->
        ILBuilder {
          let! e1' = inner e1
          let! e2' = inner e2
          return fun i -> (typ, Pair(e1' i, e2' i))
        }
      | AST.Proj(_,_,exp, (_,id)) -> //TODO: Handle Input projs
        ILBuilder {
          let rec gatherProjs = function
            | AST.Proj(_,_,exp, (_,id)) as proj ->
                let inner_exp, proj_inner = gatherProjs exp
                inner_exp, ILBuilder { let! inner_exp = proj_inner in return fun i -> (eAnnoOf proj |> convertType, Access(inner_exp i, id))}
            | base_exp -> base_exp, inner base_exp
          let inner_exp, e_gen = gatherProjs expr
          //let e_gen i =
          //  ILBuilder {
          //    let! idx = indexVector i inner_exp
          //    let access = build idx
          //    return typ, snd access
          //  }
          let! env = ask
          let! s = getState
          if pureLookup s.constantVectors (exprConvert inner_exp env s |> (fun (f,_,_) -> f)) |> Option.isSome
          then
            let! e'' = exprConvert expr
            return! moveCst e''
          else
            return! e_gen
        }
      | AST.Filter(t,_, e, types) ->
        ILBuilder {
          let! e' = inner e
          return fun i -> typ, Filter(e' i, TRec <| List.map snd types)
        }
      | AST.Let(_,_, lets, expr) ->
        let lift e i = ILBuilder { return e i }
        ILBuilder {
          let liftLeft (_, (_,x), eval) =
            ILBuilder {
              let! eAName = inner eval
              let! eAName = AVXWrap (lift eAName) (convertType <| eAnnoOf eval)
              do! emit [Let(x, eAName, true)]
              let! env = ask
              return env.extendSame(x, Local).extendVector x
              //return ({env with variables = extEnv env.variables x Local; vectorVars = x::env.vectorVars})
            }
          if lets.Length = 0 then return! inner expr else
          let! new_env = updateMany <| List.map liftLeft lets
          return! withEnv new_env <| inner expr
        }
      | AST.If(_,_,e1, e2, e3) ->
        ILBuilder {
          let! e1' = inner e1
          let! e2' = inner e2
          let! e3' = inner e3
          let resname = getName ()
          //do! emit [Let(condname, e1', true)]
          //let condvar = fst e1', Var condname
          let e_gen i =
            ILBuilder {
              let idx1 = e1' i
              let idx2 = e2' i
              let idx3 = e3' i
              let res = sprintf "%s_%d" resname i

              do! emit [Declare(fst idx2, res)]
              do! emit [If(idx1, Assign((fst idx2, Var res), idx2), Assign((fst idx2, Var res), idx3))]
              //return (typ, Var res)
            }
          do! emitVectorStatements e_gen
          //let! res = AVXWrap e_gen typ
          return fun i -> (typ, Var <| sprintf "%s_%d" resname i)
        }
      | AST.Match(_, _, e, cases) ->
        ILBuilder {
          let! e' = inner e
          let tmp = wishName "tmp"
          match (eAnnoOf e) with
          | TGeneric(syn,GenericOption,[_,t]) ->
            let e_gen i = //make use of index i?
              ILBuilder {
                let (e_none, e_some, name_some) = getSomeNoneBranch cases
                let! _,noneBlock =
                  enclose <| ILBuilder {
                    let! e_none' = exprConvert e_none
                    do! emit [Assign((typ,Var tmp),e_none')]
                  }
                let! _,someBlock =
                  capture <| ILBuilder {
                    let! e_some' = exprConvert e_some |> withVarV name_some Local
                    do! emit [Assign((typ,Var tmp),e_some')]
                  }
                let access =
                  if isNullable t
                  then Access(e' i, "Value")
                  else snd (e' i)
                let addVar = Let(name_some, (convertType t, access), true) :: (Seq.toList <| Seq.rev someBlock)
                let s_some' = Block addVar
                do! emit [If(callIsNull (e' i), noneBlock, s_some'); Let(tmp,(typ, Var "null"), false)]
                return (typ, Var tmp)

              }
            let! res = AVXWrap e_gen typ
            return fun i -> typ, Index(res, ilInt i)
          | _ -> // Must be tags
            let e_gen i =
              let mapper (_, pattern, exp) =
                let name = patternToName pattern
                let tag = patternToTag pattern
                let caseTyp = TRec [tag]
                ILBuilder {
                  let! exp',stmts' = exprConvert exp |> withVarV name Local |> capture
                  return caseTyp, name, Block <| (Seq.append  (Seq.rev stmts') (Seq.singleton (Assign((typ,Var tmp),exp'))))
                }
              ILBuilder {
                let! ilCases = mapRSW mapper cases
                let idx = e' i
                do! emit [Match(idx, Seq.toList ilCases); Let(tmp,(typ, Var "null"), false)]
                return (typ, Var tmp)
              }
            let! res = AVXWrap e_gen typ
            return fun i -> typ, Index(res, ilInt i)
        }
      | AST.FCall(_, _, func, args) ->
        ILBuilder {
          let! env = ask
          let compOpts = env.compilerOptions
          let simpleName = getASTVarName func
          match simpleName,args with
          //| Some "all", [(_,AST.Map (_, _, forIns, wOpt, e_body))] when compOpts.deforest ->
          //  return! mapReduce forIns wOpt e_body <| Some allOperation
          //| Some "isEmpty", [(_,AST.Map (_, _, forIns, wOpt, e_body))] when compOpts.deforest ->
          //    return! mapReduce forIns wOpt e_body <| Some allOperation
          //| Some "count", [(_,AST.Map (_, _, forIns, wOpt, e_body))] when compOpts.deforest ->
          //  return! mapReduce forIns wOpt e_body <| Some countOperation
          //| Some "sum", [(_,AST.Map (_, _, forIns, wOpt, e_body))] when compOpts.deforest ->
          //  return! mapReduce forIns wOpt e_body <| Some sumOperation
          //| Some "avg", [(_,AST.Map (_, _, forIns, wOpt, e_body))] when compOpts.deforest ->
          //  return! mapReduce forIns wOpt e_body <| Some avgOperation
          //| Some "createMap", [(_,AST.Map (_, _, forIns, wOpt, e_body))] when compOpts.deforest ->
          //  return! mapReduce forIns wOpt e_body <| Some mkMapOperation
          | _ ->
            let! func_exp = inner func
            let! arg_es = mapRSW (snd >> inner) args
            let ftyp =
              match ASTUtil.eAnnoOf func with
              | AST.TFun(AST.IFun(_)) -> IFunc
              | TGeneric(_,GenericMap,_) -> Map
              | _ -> Other
            let e_gen i =
              ILBuilder {
                let idx = func_exp i
                let arg_idxs = Seq.map ((|>) i) arg_es
                return (typ, Call(ftyp, idx, Seq.toList arg_idxs))
              }
            let! res = AVXWrap e_gen typ
            return fun i -> typ, Index(res, ilInt i)
          }
      | AST.Map(eA, _, forins, where, body) ->
        ILBuilder{
          let! (env : VectorEnvironment<'eA>) = ask
          let listGen list = fun _ -> makeConst exprConvert list env
          let! (lists, innerEnv) = extendWithCollectionIterables<'eA, VectorEnvironment<'eA>> forins listGen env
          let (_, prestmts) = GeneralUtil.SeqUnZip lists
          do! emit <| Seq.concat prestmts
          let! s = getState
          let e_gen i =
            let lists =
              List.map (
                fun (c,pre) ->
                let reg =
                  (fun (f) ->
                    let innerMap =
                      List.map (fun (n, e) ->
                        let iTyp,_ = asTVector <| fst e
                        (n, (iTyp, Index(e, (TInt, CstI i))))
                      ) f
                    innerMap)
                let kvp =
                  (fun ((k,v,ed)) ->
                    let dTyp,_ = asTVector <| fst ed
                    let ed' = (dTyp, Index(ed, (TInt, CstI i)))
                    (k,v,ed'))
                Choices.choiceMap reg kvp c, pre)
                lists
            let cond = function
              | _,Var x ->
                List.contains x env.vectorVars ||
                List.exists (fst >> (=) x) env.iterationVars
              | _ -> false
            let map e = let res,_,_ = indexVector i (TVector(fst e, Empty), snd e) env s in res
            let recurser e = ILBuilder {let! e' = exprConvert e in return replaceCondILExpr cond map e'}
            loopIL<'eA, VectorEnvironment<'eA>> lists where body recurser None |> withEnv innerEnv
          let! res = AVXWrap e_gen typ
          return fun i -> typ, Index(res, ilInt i)
        }
      | AST.CreateRec(eA, _, objName, fields) ->
        ILBuilder {
          let ids, exps = List.unzip fields
          let! ilExps = mapRSW inner exps
          let e_gen i =
            let tmp = wishName "tmp"
            let decl = Let(tmp, (typ, Init(typ, [])),false)
            ILBuilder {
              let ilExps = Seq.map ((|>) i) ilExps
              let fieldExps = Seq.zip ids ilExps
              do! emit [decl]
              do! emit <| Seq.map (fun ((_,x),e) -> Assign((TUnit, Access((TUnit, Var tmp), x)), e)) fieldExps
              return (typ, Var tmp)
            }
          let! res = AVXWrap e_gen typ
          return fun i -> typ, Index(res, ilInt i)
        }
      | AST.EPar(_, _, e) -> inner e
      | EMissing _ -> failwith "impossible"
    inner expr env state

  /// <summary>
  /// Converts an AST expression to an equivalent IL-expression, where the statements returned preceed the expression.
  /// The statements are returned in reverse order.
  /// </summary>
  and exprConvert e : RSWMonad<ILGenEnvironment<'eA>, ILGenState, ILstmt, annoILexpr> =
    let annotate = convertType << eAnnoOf
    let rec prep innerExp eA stmtGenerator =
      ILBuilder {
        let varname = getName ()
        let! innerVar = inner false innerExp
        let typ = convertType eA
        do! emit [Declare (typ, varname)]
        let var = (typ, Var varname)
        do! stmtGenerator innerVar var
        return var
      }
    and mapReduce forIns wOpt e_body reduction =
      ILBuilder {
        let! env = ask
        let listGen = makeConst (inner true)
        let! (lists, innerEnv) = extendWithCollectionIterables<'eA, ILGenEnvironment<'eA>> forIns listGen env
        return! withEnv innerEnv <| loopIL<'eA, ILGenEnvironment<'eA>> lists wOpt e_body (inner true) reduction
      }
    and inner allowConstantEmition (exp : AST.expr<typ<Position>,Position>)
      : RSWMonad<ILGenEnvironment<'eA>, ILGenState, ILstmt, annoILexpr> =
      let inner' = inner allowConstantEmition
      let annoTyp = annotate exp
      match exp with
      | _ when allowConstantEmition && isCompositeAndImmutable exp ->
        ILBuilder {
          let! e' = inner false exp
          let! e' = addGlobalConstant e'
          return annoTyp, e'
        }
      | AST.EPar (_, _, e) -> inner' e
      | AST.Proj (_, _, e,(_,x)) -> handleProj exprConvert exp
        //ILBuilder {
        //  let! e' = inner' e
        //  return annoTyp,(Access(e', x))
        //}
      | AST.ESome (_,_,e) ->
        ILBuilder {
          let! e' = inner' e
          return annoTyp,(ESome e')
        }
      | AST.Filter (eA, _, e1, filters) ->
        ILBuilder {
          let! e1' = inner' e1
          return annoTyp, Filter(e1', TRec <| List.map snd filters)
        }
      | AST.BinOp (eA, _, op, e1, e2) ->
        ILBuilder {
          let! e1' = inner' e1
          let! e2' = inner' e2
          return (annoTyp, Binop(op, e1', e2'))
        }
      | AST.Pair (eA, _, e1, e2) ->
        ILBuilder {
          let! e1' = inner' e1
          let! e2' = inner' e2
          return (annoTyp, Pair(e1', e2'))
        }
      | AST.FCall(eA, _, e1, args) ->
        let simpleName = getASTVarName e1
        ILBuilder {
          let! (env : ILGenEnvironment<'eA>) = ask
          let compOpts = (env :> IEnvironment<'eA>).compilerOptions
          match simpleName,args with
          | Some "all", [(_,AST.Map (_, _, forIns, wOpt, e_body))] when compOpts.deforest ->
            return! mapReduce forIns wOpt e_body <| Some allOperation
          | Some "any", [(_,AST.Map (_, _, forIns, wOpt, e_body))] when compOpts.deforest ->
            return! mapReduce forIns wOpt e_body <| Some anyOperation
          | Some "count", [(_,AST.Map (_, _, forIns, wOpt, e_body))] when compOpts.deforest ->
            return! mapReduce forIns wOpt e_body <| Some countOperation
          | Some "firstOrNone", [(_,AST.Map (_, _, forIns, wOpt, e_body))] when compOpts.deforest ->
            return! mapReduce forIns wOpt e_body <| Some firstOrNoneOperation
          | Some "maxOfList", [(_,AST.Map (_, _, forIns, wOpt, e_body))] when compOpts.deforest ->
            return! mapReduce forIns wOpt e_body <| Some maxOfListOperation
          | Some "sum", [(_,AST.Map (_, _, forIns, wOpt, e_body))] when compOpts.deforest ->
            return! mapReduce forIns wOpt e_body <| Some sumOperation
          | Some "avg", [(_,AST.Map (_, _, forIns, wOpt, e_body))] when compOpts.deforest ->
            return! mapReduce forIns wOpt e_body <| Some avgOperation
          | Some "unique", [(_,AST.Map (_, _, [forIns], wOpt, e_body))] when compOpts.deforest -> // Todo
            return! mapReduce [forIns] wOpt e_body <| Some uniqueOperation
          | Some "createMap", [(_,AST.Map (_, _, forIns, wOpt, e_body))] when compOpts.deforest ->
              return! mapReduce forIns wOpt e_body <| Some mkMapOperation
          | Some "sum", [(_,AST.List(listTyp,_,xs,_))] when compOpts.deforest ->
            return!
              ILBuilder {
                let sumName = wishName "sum"
                do! emit [Let(sumName, (TDouble, CstD 0.0), false)]

                let mapper (_, expr) =
                    ILBuilder {
                      let! arg = inner' expr
                      do! emit [ Assign ((TDouble, Var sumName), (TDouble, Binop(Plus, (TDouble, Var sumName), arg) )) ]
                    }
                do! mapRSW_ mapper xs
                return TDouble, Var sumName
              }
          | Some "createMap", [(_,AST.List(listTyp,_,xs,_))] when compOpts.deforest ->
            return!
              ILBuilder {
                let typ' = convertType listTyp
                let kt,vt = match typ' with TArray(TPair(kt,vt)) -> kt,vt | _ -> failwith <| sprintf "unexpected: %A" typ'
                let typ = TMap(kt, vt, true)
                let dictName = wishName "quickDict"
                do! emit [Let(dictName, (typ, Init(typ, [])), false)]

                let mapper (_, expr) =
                  match expr with
                  | AST.Pair (_,_, e1, e2) ->
                    ILBuilder {
                      let! e1' = inner' e1
                      let! e2' = inner' e2
                      do! emit [ Expression ( TUnit
                               , Call( Other
                                     , (TUnit, accessAdd typ dictName)
                                     , [e1'; e2']))]
                    }
                  | _ ->
                    ILBuilder {
                      let! arg = inner' expr
                      let tupleName = wishName "tuple"
                      let key = kt,Access((fst arg,Var tupleName), "Item1")
                      let value = vt,Access((fst arg,Var tupleName), "Item2")
                      do! emit [ Expression ( TUnit
                               , Call( Other
                                     , (TUnit, accessAdd typ dictName)
                                     , [key; value]))
                               ; Let(tupleName, arg, false)]
                    }
                do! mapRSW_ mapper xs
                return TMap(kt, vt, false), Var dictName
              }
          | _ ->
            let! e1' = inner' e1
            let typ =
                match ASTUtil.eAnnoOf e1 with
                | AST.TFun(AST.IFun(_)) -> IFunc
                | TGeneric(_,GenericMap,_) -> Map
                | _ -> Other
            let! argExprs = mapRSW (snd >> inner') args
            return annoTyp, Call(typ, e1', Seq.toList argExprs)
          }
      | AST.List(typ,_,elems,_) ->
        ILBuilder {
          let! es = mapRSW (snd >> inner') elems
          return convertType typ, UList (Seq.toList es)
        }
      | AST.If(eA,_, cond, t, f) ->
        let ifGen condVar ifName =
          ILBuilder {
              let! _,trueBlock =
                  enclose <| ILBuilder {
                      let! trueVar = inner' t
                      do! emit [Assign (ifName, trueVar)]
                  }
              let! _,falseBlock =
                  enclose <| ILBuilder {
                      let! falseVar = inner' f
                      do! emit [Assign (ifName, falseVar)]
                  }
              do! emit [If(condVar, trueBlock, falseBlock)]
          }
        prep cond eA ifGen
      | AST.Match(eA, _, pattern, cases) ->
        let switchGen patternVar switchName =
          match eAnnoOf pattern with
          | TGeneric(syn,GenericOption,[_,t1]) ->
            ILBuilder {
              let (e_none, e_some, name_some) = getSomeNoneBranch cases
              let! _,noneBlock =
                enclose <| ILBuilder {
                   let! noneBranch = inner' e_none
                   do! emit [Assign (switchName, noneBranch)]
                }
              let! _,someBlock =
                enclose <| ILBuilder {
                   let access =
                     if isNullable t1
                     then Access(patternVar, "Value")
                     else snd patternVar
                   do! emit [Let (name_some, (convertType t1, access), true)]
                   let! noneBranch = withVar name_some Local <| inner' e_some
                   do! emit [Assign (switchName, noneBranch)]
                }
              do! emit [If( callIsNull patternVar, noneBlock, someBlock)]
            }
          | _ -> //Must be tags
            let evalAndAssign (_, pattern, exp) =
              let x = patternToName pattern
              let tag = patternToTag pattern
              let caseBlock = enclose <| ILBuilder {
                let! caseName = withVar x Local <| inner' exp
                do! emit [Assign (switchName, caseName)]
              }
              fmap (fun block -> (TRec [tag], x, snd block)) caseBlock
            ILBuilder {
                let! caseBlocks = mapRSW (evalAndAssign) cases
                do! emit [Match(patternVar, Seq.toList caseBlocks)]
            }
        prep pattern eA switchGen
      | AST.Let(_, _, lbs, e1) ->
        let rec gen binds =
          match binds with
          | [] -> inner' e1
          | (_, (_,x), eval)::rest ->
              ILBuilder {
                  let! eName = inner' eval
                  do! emit [Let(x, eName, true)]
                  return! withVar x Local (gen rest)
              }
        gen lbs
      | AST.Map(eA, syn, forIns, wOpt, e_body) ->
        //when compOpts.vectorize && (shouldSimdConvert e_body = Reason)
        //&& (match forIns.Head with (_, Choice1Of2 _) -> true | _ -> false)
        //->
        ILBuilder {
          let! env = ask
          if not <|
              (env.compilerOptions.vectorize &&
               shouldSimdConvert e_body = Reason &&
               wOpt.IsNone &&
               (match forIns.Head with (_, Choice1Of2 _) -> true | _ -> false)) //temporary fix to not vectorize dict-traversals
          then return! mapReduce forIns wOpt e_body None
          else
          let outer,rest = GeneralUtil.headTail forIns
          let! env = ask
          let! (lists, innerEnv) = extendWithCollectionIterables<'eA, ILGenEnvironment<'eA>> [outer] (makeConst inner') env
          let vect, stmts = match lists.Head with Choice1Of2 vect,stmts -> vect,stmts | _ -> failwith "impossible"
          let size = call_counti <| (List.head >> snd) vect
          let res_var = wishName "vector_result"
          let dest e = Some(res_var, e, size, false)
          let iters = vect
          let counterName = wishName "iter"
          let env = new VectorEnvironment<'eA>(innerEnv, [], iters, counterName)
          if rest.IsEmpty
          then
            do! emit stmts
            let! e', loop =
              expr_AVX e_body
              |> withEnv env
              |> enclose
            do! releaseVectors
            let! ew, loopw =
              inner' e_body
              |> withEnv innerEnv
              |> enclose
            let newnames = List.map (fun (n,e) -> (getName(), n)) iters
            let op,_,_ = collectOperation counterName res_var (snd iters.Head) ew
            let wrapup = For(counterName, iters, loopw, Some op)
            let wrapup = List.fold (fun wup (newn, oldn) -> renameVar oldn newn wup) wrapup newnames
            do! emit [VectorLoop(counterName, iters, loop, dest e', wrapup)]
            do! resetArraysM
            return convertType eA, Var res_var
          else
            do! emit stmts
            let! e', loop =
              expr_AVX (AST.Map(eA, syn, rest, None, e_body))
              |> withEnv env
              |> enclose
            //let! e' = AVXWrap (lift e') (match convertType eA with TArray t | TTypeSpan t -> t | _ -> failwith "no")
            do! releaseVectors
            let! ew, loopw =
              inner' (AST.Map(eA, syn, rest, None, e_body))
              |> withEnv innerEnv
              |> enclose
            let (destType, res) = e' 0
            let idLambda = TUnit, Var "i => i"
            let flatter intermediateDest =
                destType,Call(Other, (destType, Access(intermediateDest, "SelectMany")), [idLambda])
            let newnames = List.map (fun (n,e) -> (getName(), n)) iters
            let op,_,_ =  collectOperation counterName res_var (snd iters.Head) ew
            let wrapup = For("", iters, loopw, Some op)
            let wrapup = List.fold (fun wup (newn, oldn) -> renameVar oldn newn wup) wrapup newnames
            do! emit [VectorLoop(counterName, iters, loop, dest e', wrapup)]
            do! resetArraysM
            return convertType eA, Call(Other, (destType, Access(flatter (destType, Var res_var), "ToArray")), [])
        }
      //| AST.Map(eA, _, forIns, wOpt, e_body) ->
      //  match forIns with
      //  | [] -> failwith "Impossible"
      //  | _ -> mapReduce forIns wOpt e_body None
          //ILBuilder {
          //  let! env = ask
          //  let! (lists, innerEnv) = extendWithCollectionIterables forIns (makeConst inner') env
          //  return! withEnv innerEnv <| loopIL analyser lists wOpt e_body inner'
          //}
      | CreateRec (eA,_,(_,nm),fields) ->
        ILBuilder {
            let recName = getName ()
            let t = TRec [nm]
            let recExpr = (t, Var recName)
            do! emit [Let(recName, (t, Init(t, [])), false)]
            let mapper ((_,ident), expr) = ILBuilder {
              let! arg = inner' expr
              do! emit [Assign((TUnit, Access(recExpr, ident)), arg)]
            }
            do! mapRSW_ mapper fields
            return recExpr
        }
      | AST.Var _
      | AST.CDouble _
      | AST.CBool _
      | AST.CStr _
      | AST.ENone _
      | AST.CEnum _->
        ILBuilder {
          let! env = ask
          return simpleConvert env.variables exp
        }
      | AST.EMissing _ -> failwith "Impossible"
    inner true e



  let obtainTypeInfo (analyser : Analyser<'eA>) t =  // probably redundant a redundant decleration
    match t with
    | TRec [tag] ->
      Some <| (t, analyser.DataStructure.totalMap.[tag])
    | _ -> None

  /// <summary>
  /// Constructs an ´Access´ expression from a variable ´var´ and a list of projections ´projs´.
  /// </summary>
  let constructAccess (analyser : Analyser<'eA>) t var projs =
      Seq.fold (fun e (_,_,(_,p)) -> TUnit, Access(e, p)) (t, var) projs

  /// <summary>
  /// Constructs an ´Access´ expression from a variable ´var´ and a list of projections ´projs´.
  /// </summary>
  let constructAssignAccess inInit (analyser : Analyser<'eA>) t var projs eAName =
    if false
    then
      let lastTag =
        Seq.fold (fun tagInfo (_,_,(_,p)) ->
          match tagInfo with
          | None -> None
          | Some (typ,tagInfo') ->
            obtainTypeInfo analyser (convertType <| tagInfo'.[p])
          ) (obtainTypeInfo analyser t) (Seq.take (Seq.length projs - 1) projs)
      let (_,_,(_,p)) = Seq.last projs
      match lastTag with
      | Some (typ,_) ->
         match isOutputProj analyser.DataStructure.superTypes analyser.DataStructure.outputMap typ p with
         | Some tag ->
           let outField tag = TUnit,Access((TUnit, Var "state"),sprintf "out_%s_%s" tag p)
           let projNumber = TUnit,Access((TUnit, Var "state"),sprintf "___projNumber")
           let outputEntity = constructAccess analyser t var (Seq.take (Seq.length projs - 1) projs)


           match typ with
           | TRec(["Global"])
              ->
                  Expression(TUnit, Call(Other, (TFun IFun, Access((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"),"___AddToArr")),
                     [outField "Global" ; projNumber; eAName]))
           | TRec([_])
              ->
                 let baseAssign = if  Map.containsKey tag BaseProgram.expectedFields && List.exists (fun (f,_) -> f = p) BaseProgram.expectedFields.[tag]
                                  then [Assign(constructAccess analyser t var projs, eAName)]
                                  else []
                 Block
                   (Expression(TUnit, Call(Other, (TFun IFun, Access((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"),"___AddToArr")),
                     [outField tag ; (TUnit, Access(outputEntity, sprintf "%s_id" tag)); projNumber; eAName])) :: baseAssign)
         | _ -> Assign(constructAccess analyser t var projs, eAName)
      | _ -> Assign(constructAccess analyser t var projs, eAName)
    else Assign(constructAccess analyser t var projs, eAName)

  /// <summary>
  /// Converts a single AST statement to an equivalent list of IL-statements. The list is in reverse order.
  /// </summary>
  let rec stmtConvert stmt inInit: RSWMonad<ILGenEnvironment<'eA>, ILGenState, ILstmt, ILGenEnvironment<'eA>> =
    //let genExp e env state = exprConvert analyser vectorize e env state
    //let inner = stmtConvert analyser vectorize
    match stmt with
    | SLet (_, (_,x), e) ->
      ILBuilder {
        let! eAName = exprConvert e
        do! emit [Let(x, eAName, true)]
        let! env = ask
        return env.extend(x, Local)
      }
    | SDo (_, ((t,(_,nm)),projs), args) ->
      ILBuilder {
        let! env = ask
        let accesses = constructAccess env.analyser (convertType t) (convertVar env.variables nm) projs
        let! eargs = mapRSW (snd >> exprConvert) <| List.rev args
        do! emit [Expression(TUnit,Call(funtyp.Other, accesses, Seq.toList eargs))]
        return! ask
      }
    | SDoCSharp (_, (_,extIdent)) ->
      ILBuilder {
        do! emit [DoCSharp extIdent]
        return! ask
      }
    | SAss (_,((x_t, (_,x)), projs),e) ->
      ILBuilder {
        let! eAName = exprConvert e
        let t = eAnnoOf e |> convertType
        let! env = ask
        let var = convertVar env.variables x
        let a = constructAssignAccess inInit env.analyser (convertType x_t) var projs eAName
        do! emit [a]
        return env
      }
    | SUpdate (_, (_, iter), cond, stmt) ->
      let envGen,e,stmtGen =
        match iter with
        | Choice1Of2 [(_,x), e] ->
          (fun env -> extEnv env x Local),
          e,
          fun (eAName, body) -> For(getName (), [x, eAName], body, None)
        | Choice2Of2 ((_,(_,k),(_,v)), e) ->
          (fun env -> extEnv (extEnv env k Local) v Local),
          e,
          fun (eAName, body) -> Foreach(getName (), k,v, eAName, body, None)
        | _ -> failwith "impossible"
      ILBuilder {
        let! eAName = exprConvert e
        let! env = ask
        let vars = envGen env.variables
        let env = new ILGenEnvironment<'eA>(vars, env.analyser, env.compilerOptions)
        let! _,startStmts =
          match cond with
            | Some (_, exp) ->
              ILBuilder {
                let! clauseEName = exprConvert exp
                do! emit [SkipIfNot(clauseEName)]
              }
            | None -> retRSW ()
          |> withEnv env
          |> enclose
        let! env,stmts = enclose <| withEnv env (stmtConvert stmt inInit)
        let loopBody = Block [startStmts; stmts]
        let nm = wishName "asConst"
        do! emit [Let(nm, eAName, true)]
        do! emit [stmtGen ((fst eAName, Var nm), loopBody)]
        return env
      }
    | SOverwrite(_, assProjs,e) ->
      stmtConvert (SAss(noAssignSyntax, assProjs, e)) inInit
    | STransfer(_, ((t1, (_,x1)), projs1),e,((t2, (_,x2)), projs2)) ->
      ILBuilder {
        let! eAName = exprConvert e
        let t = eAnnoOf e |> convertType
        let t1 = convertType t1
        let t2 = convertType t2
        let! env = ask
        let var1 = convertVar env.variables x1
        let var2 = convertVar env.variables x2
        let accesses1 = constructAccess env.analyser t1 var1 projs1
        let accesses2 = constructAccess env.analyser t2 var2 projs2
        do! emit [ Assign(accesses1, (t1, Binop(Minus, accesses1, eAName)))
                 ; Assign(accesses2, (t2, Binop(Plus, accesses2, eAName)))]
        return env
      }
    | SBlock (_,stmts) ->
      ILBuilder {
        let! env = ask
        let! env, ilStmts = enclose <| foldRWS (fun env stmt -> withEnv env (stmtConvert stmt inInit)) env stmts
        do! emit [ilStmts]
        return env
      }
    | SIf(_, cond, trueBranch, fBranchOpt) ->
      ILBuilder {
        let! condExp = exprConvert cond
        let! env = ask
        let! _,trueStmt = enclose <| stmtConvert trueBranch inInit
        let! _,falseStmt =
          match fBranchOpt with
          | Some (_, falseBranch) -> enclose <| stmtConvert falseBranch inInit
          | None -> ILBuilder { return env, Block Seq.empty }
        do! emit [If(condExp, trueStmt, falseStmt)]
        return env
      }
    | SMatch(_, exp, cases) ->
      ILBuilder {
        let! mExp = exprConvert exp
        let! env = ask
        do!
          match eAnnoOf exp with
          | TGeneric(syn,GenericOption,[_,t1]) ->
            ILBuilder {
              let (s_none, s_some, name_some) = getSomeNoneBranch cases
              let! _,noneBlock = enclose <| stmtConvert s_none inInit
              let! _,someBlock =
                enclose <| ILBuilder {
                   let access =
                     if isNullable t1
                     then Access(mExp, "Value")
                     else snd mExp
                   do! emit [Let (name_some, (convertType t1, access), true)]
                   return! withVar name_some Local <| stmtConvert s_some inInit
                }
              do! emit [If(callIsNull mExp, noneBlock, someBlock)]
            }
          | _ -> //Must be tags
            let convertCase (_, pattern, stm) =
              let x = patternToName pattern
              let tag = patternToTag pattern
              let caseBlock = stmtConvert stm inInit |> withVar x Local |> enclose
              fmap (fun block -> (TRec [tag], x, snd block)) caseBlock
            ILBuilder {
                let! caseBlocks = mapRSW (convertCase) cases
                do! emit [Match(mExp, Seq.toList caseBlocks)]
            }
        return env
      }
    | SkippedStmt (_)      -> failwith "impossible"

  let genILStmt stmt inInit (environment : ILGenEnvironment<'eA>) state =
    let _,s',ss = stmtConvert stmt inInit environment state
    Seq.append (releaseArrays s') ss, (s'.filterCaches , s'.globalConstants)

  let funConvert decBody (environment : ILGenEnvironment<'eA>) state =
      let a,s',ss = exprConvert decBody environment state
      let newRes = getName ()
      let resVar = fst a, Var newRes
      let assign = Assign(resVar,a)
      Some (resVar),
        seq {
          yield assign
          yield! ss
          yield Declare(fst a, newRes)
          yield! releaseArrays s'
        }, (s'.filterCaches , s'.globalConstants)

  let actConvert decBody inInit (environment : ILGenEnvironment<'eA>) state =
      let _,s',ss = stmtConvert decBody inInit environment state
      None, Seq.append ss (releaseArrays s'), (s'.filterCaches , s'.globalConstants)