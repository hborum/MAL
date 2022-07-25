namespace itu.dk.MAL

module AST =
  // Message, name, pos
  type errorPos = string * string * int

  type Severity =
      | Error = 1
      | Warning = 2
      | Information = 3
      | Hint = 4
      | InternalError = 5 //Used to report errors related to the analyses

  type ErrorMessage = {
    message: string
    index: int
    length: int
    origin: string  // Where does the error originate from
    prettyOrigin: string // The origin shown to the user
    severity: Severity

    quickfixMsg : string
  }

  type colonSyntax<'sA>       = { kwColon : 'sA}
  type outputAsSyntax<'sA>    = { kwOutput : 'sA}
  type projSyntax<'sA>        = { kwDot : 'sA }
  type letSyntax<'sA>         = { kwIn : 'sA; kwEnd : 'sA }
  type ifSyntax<'sA>          = { kwIf : 'sA; kwThen : 'sA; kwElse : 'sA }
  type matchSyntax<'sA>       = { kwMatch : 'sA; kwWith : 'sA; kwEnd : 'sA }
  type forComputeSyntax<'sA>  = { loopName : string ; kwWith : 'sA; kwEnd : 'sA; }
  type fCall<'sA>             = { kwLPar : 'sA; kwRPar : 'sA }
  type aCall<'sA>             = { kwDo : 'sA; kwLPar : 'sA; kwRPar : 'sA }
  type aCallCS<'sA>           = { kwDoCS : 'sA }
  type constantSyntax<'sA>    = { kwStart: 'sA; kwEnd: 'sA; str : string}
  type letBindingSyntax<'sA>  = { kwLet : 'sA; kwEq : 'sA }
  type matchCaseSyntax<'sA>   = { kwBar : 'sA; kwArrow : 'sA }
  type forInSyntax<'sA>       = { kwFor : 'sA; kwIn : 'sA ; commas : 'sA list}
  type whereSyntax<'sA>       = { kwWhere : 'sA }
  type assignSyntax<'sA>      = { kwEq : 'sA }
  type parSyntax<'sA>         = { kwPStart : 'sA; kwPEnd : 'sA }
  type listSyntax<'sA>        = { kwSep : option<'sA> }
  type createRecSyntax<'sA>   = { kwNew : 'sA ; LBrack : 'sA; RBrack : 'sA ; commas : 'sA list ; equals : 'sA list}
  type noneSyntax<'sA>        = { kwNone: 'sA; colon : 'sA}
  type uListSyntax<'sA>       = { kwLBrck : 'sA; kwRBrck : 'sA}
  type sifSyntax<'sA>         = { kwIf : 'sA; lPar : 'sA; rPar : 'sA; lBrace : 'sA; rBrace : 'sA; }
  type selseSyntax<'sA>       = { kwElse : 'sA; lBrace : 'sA; rBrace : 'sA; }
  type switchCaseSyntax<'sA>  = { kwCase : 'sA; kwColon : 'sA; }
  type switchSyntax<'sA>      = { kwSwitch : 'sA; lPar : 'sA; rPar : 'sA; lBrace : 'sA; rBrace : 'sA; }

  type transferSyntax<'sA>    = { LArrow : 'sA ; RArrow : 'sA}
  type overwriteSyntax<'sA>   = { Arrow : 'sA}
  type modulSyntax<'sA>       = { kwModule : 'sA }

  type tag<'sA> = 'sA * string
  type pureTag = string
  type identifier<'sA> = constantSyntax<'sA> * string

  let nm_only (_,_,nm) = nm

  type pureIdentifier = string

  type storeId = string

  type cardinality =
    | ToOne
    | ToMany

  type assoc =
    | Left
    | Right

  type binOp =
    | Plus
    | Minus
    | Mult
    | Div
    | Mod
    | Eq | LT | LTE | GT | GTE
    | LOR | LAND

  type Position =
    | FullPos of int * int // FullStart * Start
    | Missing of int // FullStart
    | NoPos

  let fullStart = function
    | FullPos (i,_) -> i
    | Missing i -> i
    | NoPos -> failwith "Impossible"

  type assignProj<'exprAnno, 'syntaxAnno> = ('exprAnno * identifier<'syntaxAnno>) * (('exprAnno * projSyntax<'syntaxAnno> * identifier<'syntaxAnno>) list)

  type relInfo = (pureTag * cardinality) list

  type genericSyntax<'syntaxAnno> = { kwName : 'syntaxAnno; kwLA : 'syntaxAnno; kwRA : 'syntaxAnno }
  type binaryGenericSyntax<'syntaxAnno>  = { kwName : 'syntaxAnno; kwLA : 'syntaxAnno; kwComma : 'syntaxAnno; kwRA : 'syntaxAnno }

  type genericIdentifier =
    | GenericList
    | GenericMap
    | GenericOption
    | GenericPair
    | GenericSysFun // Func<T,U>
    | GenericUnknown of string

  [<CustomEquality; CustomComparison>]
  type typ<'sA> =
    | TErr
    | TGeneric of genericSyntax<'sA> * genericIdentifier * (listSyntax<'sA> * typ<'sA>) list
    | TMissing of 'sA
    | TDouble  of 'sA
    | ASTTInt
    | TReserve of 'sA
    | TBool    of 'sA
    | TEnum    of 'sA * string
    | TStr     of 'sA
    | TRec     of 'sA * pureTag list
    | TFun     of funTyp<'sA>
    | TAct     of typ<'sA> list
    | TModule  of string * (Map<string, typ<'sA>>)
    | TVoid
    interface System.IComparable with
      member this.CompareTo other = (sprintf "%A" this).CompareTo(sprintf "%A" other)
    override this.Equals other =
      match other with
      | :? typ<'sA> as otherType ->
        match (this,otherType) with
        | TDouble (_), TDouble (_)       -> true
        | TReserve (_), TReserve (_)     -> true
        | TBool (_), TBool (_)           -> true
        | TVoid (_), TVoid (_)           -> true
        | TStr (_), TStr (_)             -> true
        | TEnum(_,s1) , TEnum(_,s2)      -> s1 = s2
        | TRec (_, tag1), TRec (_, tag2) -> List.forall (fun s -> List.contains s tag1) tag2 && List.forall (fun s -> List.contains s tag2) tag1
        | TGeneric (_, GenericList, [_,t1]), TGeneric (_, GenericList, [_,t2])
        | TGeneric (_, GenericOption, [_,t1]), TGeneric (_, GenericOption, [_,t2]) -> t1 = t2
        | TGeneric (_, GenericMap, [_,t11;_,t12]), TGeneric (_, GenericMap, [_,t21;_,t22])
        | TGeneric (_, GenericPair, [_,t11;_,t12]), TGeneric (_, GenericPair, [_,t21;_,t22])
        | TGeneric (_, GenericSysFun, [_,t11;_,t12]), TGeneric (_, GenericSysFun, [_,t21;_,t22])
            -> t11 = t21 && t12 = t22
        | TFun (f1), TFun (f2) ->
          match f1, f2 with
          | IFun(_),IFun(_) -> true
          | BIFun(ts1,t1),BIFun(ts2,t2) -> t1 = t2 && ts1 = ts2
          | _,_ -> false // How to compare parametric fun?
        | TAct(ts1), TAct(ts2) -> ts1 = ts2
        | TErr, _ -> true
        | _, TErr -> true
        | t1, t2 -> false
      | _ -> false
    // The fact that TErr should be equal to any type makes hashing troublesome.
    // Hashing is mostly used in collections, perhaps assume that TErr should not be present
    // in collections such as Map or HashSet?.
    override this.GetHashCode () =
      match this with
      | TErr -> 0
      | TGeneric (_, ident, typs) ->
        let idHash =
          match ident with
          | GenericUnknown s -> s.GetHashCode()
          | GenericList -> 1
          | GenericMap -> 2
          | GenericOption -> 3
          | GenericPair -> 4
          | GenericSysFun -> 5
        let elemHash = List.sumBy (fun (_,t) -> t.GetHashCode()) typs
        idHash * elemHash
      | TMissing _ -> 6
      | TDouble  _ -> 7
      | TReserve _ -> 8
      | TBool    _ -> 9
      | TEnum    (_,s) -> s.GetHashCode()*10
      | TStr     _ -> 11
      | TRec     (_, tags) -> tags.GetHashCode()*12
      | TFun     ft ->
        let typHash =
          match ft with
          | IFun _ -> 13
          | BIFun (ts,t) -> ts.GetHashCode() + t.GetHashCode()
          | ParametricFun _ -> 14
        typHash * 15
      | TAct  targs -> targs.GetHashCode()
      | TModule  (name, contents) -> name.GetHashCode() + contents.GetHashCode()
      | TVoid    -> 16


  and funTyp<'syntaxAnno> =
    | IFun     of 'syntaxAnno                                      (* IFunction *)
    | BIFun    of typ<'syntaxAnno> list * typ<'syntaxAnno>         (* Declared and builtin functions *)
    | ParametricFun of (typ<'syntaxAnno> list -> Monads.Result<typ<'syntaxAnno>, string>)

  type outputFormat =
    | PresentValueEnd
    | PresentValueMid
    | CashFlow
    | MissingFormat

  type outputMode =
    | Debug
    | Average

  type outputInfo<'sA> = typ<'sA> * outputFormat * Position * outputMode list

  type subTypeMap = Map<pureTag,pureTag list>
  type superTypeMap = Map<pureTag,pureTag option>

  type tagMap<'sA> = Map<pureTag, Map<pureIdentifier, typ<'sA>>>
  type fieldNameInfo<'sA> = Map<pureTag, Map<pureIdentifier, constantSyntax<'sA>>>
  type extsMap<'sA> = Map<pureTag, Map<pureIdentifier, typ<'sA>>>
  // outputMap maps from a tag to a map of all output-fields of the tag.
  // the second map conists of a non-empty list of identifiers creating a field path to the output value.
  type outputMap<'sA> = Map<pureTag, Map<pureIdentifier list, outputInfo<'sA>>>

  type dataStructure<'sA> =
    { positions    : Map<pureTag, constantSyntax<'sA>>
    ; subTypes     : subTypeMap
    ; superTypes   : superTypeMap
    ; initTagMap   : tagMap<'sA>  // Has Result removed on base entity tags
    ; manageTagMap : tagMap<'sA>  // Has Input removed on base entity tags
    // Seemingly used for completion items, todo investigate
    ; combinedTagMap : tagMap<'sA> // Has Result and Input removed on base entity tags.
    ; totalMap : tagMap<'sA> // Contains all fields of a tag
    ; extMap : tagMap<'sA> // Contains all the non-expected fields of a record.
                           // Used by Interpreter so we can manually create fields for all expected fields
                           // and automatically generate the remaining fields
    ; outputMap : outputMap<'sA>
    ; interfaceMap : (Map<string, Map<pureIdentifier, typ<'sA>> * Map<pureIdentifier, typ<'sA>>>) option
        // These are the fields required by a module.
        // fst arg is requires, snd is provides
        // This is empty on main modules
    ; fieldNameInfo : fieldNameInfo<'sA>
    }

  type matchPattern<'sA> =
    | PatNone of 'sA
    | PatSome of 'sA * identifier<'sA>
    | PatTag  of tag<'sA> * identifier<'sA>
    | PatMissing of 'sA

  let patternToName = function
    | PatNone _ -> failwith "impossible"
    | PatSome (_,(_,name)) -> name
    | PatTag  (_,(_,name)) -> name
    | PatMissing _ -> failwith "impossible"

  let bindingInPattern = function
    | PatNone _ -> None
    | PatSome (_,name) -> Some name
    | PatTag  (_,name) -> Some name
    | PatMissing _ -> None

  let patternToTag = function
    | PatNone _ -> failwith "impossible"
    | PatSome _ -> failwith "impossible"
    | PatTag  ((_,tag),_) -> tag
    | PatMissing _ -> failwith "impossible"

  type expectedPattern<'sA> =
    | ExpNone
    | ExpSome of typ<'sA>
    | ExpTag of string

  let comparePatterns matPat expPat =
    match matPat, expPat with
    | PatNone(_) , ExpNone -> true
    | PatSome(_) , ExpSome t -> true
    | PatTag((_,tag),_), ExpTag(tag') -> tag = tag'
    | _ , _ -> false

  type expr<'eA, 'sA> =
    | EMissing   of 'eA * 'sA
    | CDouble    of 'eA * constantSyntax<'sA>       * double
    | CBool      of 'eA * constantSyntax<'sA>       * bool
    | CEnum      of 'eA * constantSyntax<'sA>       * string
    | Pair       of 'eA * binaryGenericSyntax<'sA>  * expr<'eA, 'sA>         * expr<'eA, 'sA>
    | CStr       of 'eA * constantSyntax<'sA>       * string
    | Var        of 'eA * constantSyntax<'sA>       * identifier<'sA>
    | Proj       of 'eA * projSyntax<'sA>           * expr<'eA, 'sA>             * identifier<'sA>
    | Filter     of 'eA * colonSyntax<'sA>          * expr<'eA, 'sA>             * (tag<'sA> list)
    | Let        of 'eA * letSyntax<'sA>            * letBinding<'eA, 'sA> list  * expr<'eA, 'sA>
    | If         of 'eA * ifSyntax<'sA>             * expr<'eA, 'sA>             * expr<'eA, 'sA> * expr<'eA, 'sA>
    | Match      of 'eA * matchSyntax<'sA>          * expr<'eA, 'sA>             * matchCase<'eA, 'sA> list
    | Map        of 'eA * forComputeSyntax<'sA>     * forIn<'eA, 'sA> list       * Option<where<'eA, 'sA>> * expr<'eA, 'sA>
    | FCall      of 'eA * fCall<'sA>                * expr<'eA, 'sA>             * (listSyntax<'sA> * expr<'eA, 'sA>) list
    | BinOp      of 'eA * 'sA                       * binOp * expr<'eA, 'sA>     * expr<'eA, 'sA>
    | EPar       of 'eA * parSyntax<'sA>            * expr<'eA, 'sA>
    | ENone      of 'eA * noneSyntax<'sA>           * typ<'sA>
    | ESome      of 'eA * 'sA                       * expr<'eA, 'sA>
    | CreateRec  of 'eA * createRecSyntax<'sA>      * identifier<'sA> * (identifier<'sA> * expr<'eA, 'sA>) list
    | List       of 'eA * uListSyntax<'sA>          * (listSyntax<'sA> * expr<'eA, 'sA>) list * Option<typ<Position>>
  and letBinding<'eA, 'sA> = letBindingSyntax<'sA> * identifier<'sA> * expr<'eA, 'sA>
  and matchCase<'eA, 'sA> = matchCaseSyntax<'sA> * matchPattern<'sA> * expr<'eA, 'sA>

  and keyValuePair<'sA> = binaryGenericSyntax<'sA>  * identifier<'sA> * identifier<'sA>
                                                  (* Zip on lists                        ,   Dictionary  *)
  and forIn<'eA, 'sA> = forInSyntax<'sA> * Choice<(identifier<'sA> * expr<'eA, 'sA>) list, (keyValuePair<'sA> * expr<'eA, 'sA>)>
  and where<'eA, 'sA> = whereSyntax<'sA> * expr<'eA, 'sA>

  // Consider to add Block and remove nested stmt lists
  type stmt<'eA, 'sA> =
    | SkippedStmt of 'sA
    // Update only iterates over forIn of one pair at a time
    | SUpdate    of forComputeSyntax<'sA> * forIn<'eA, 'sA> * Option<where<'eA, 'sA>> * stmt<'eA, 'sA>
    | SAss       of assignSyntax<'sA> * assignProj<'eA, 'sA> * expr<'eA, 'sA>
    | SLet       of letBinding<'eA, 'sA>
    | SDo        of aCall<'sA> * assignProj<'eA, 'sA> * (listSyntax<'sA> * expr<'eA, 'sA>) list
    | SDoCSharp  of aCallCS<'sA> * identifier<'sA>
    | STransfer  of transferSyntax<'sA>  * assignProj<'eA, 'sA> * expr<'eA, 'sA> * assignProj<'eA, 'sA>
    | SOverwrite of overwriteSyntax<'sA> * assignProj<'eA, 'sA> * expr<'eA, 'sA>
    | SBlock     of 'sA * stmt<'eA, 'sA> list
    | SIf        of sifSyntax<'sA> * expr<'eA, 'sA> * stmt<'eA, 'sA> * (selseSyntax<'sA> * stmt<'eA, 'sA>) option
    | SMatch     of switchSyntax<'sA> * expr<'eA, 'sA> * switchCase<'eA, 'sA> list
  and switchCase<'eA, 'sA> = switchCaseSyntax<'sA> * matchPattern<'sA> * stmt<'eA, 'sA>

  type funDecSyntax<'sA>    = { kwFun : 'sA; kwLPar: 'sA; kwRPar: 'sA; kwEq : 'sA}
  type actDecSyntax<'sA>    = { kwAct : 'sA; kwLPar: 'sA; kwRPar: 'sA; kwWith: 'sA; kwEnd: 'sA}
  type blockSyntax<'sA>     = { kwBlockStart : 'sA; kwEnd : 'sA }
  type dataSyntax<'sA>      = { kwData : 'sA; kwEnd : 'sA ; kwExtends : 'sA option}
  type contractSyntax<'sA>  = { kwContract : 'sA
                              ; kwExtends : 'sA option
                              ; kwRequires : 'sA * 'sA * 'sA
                              ; kwProvides : 'sA * 'sA * 'sA
                              ; kwEnd : 'sA }

  type importSyntax<'sA>    = { kwImport : 'sA }
  type exportSyntax<'sA>    = { kwExport : 'sA ; kwLPar: 'sA; kwRPar: 'sA }
  type paramSyntax<'sA>     = { kwColon : 'sA; kwComma : 'sA option}

  type fieldInfo<'sA> = colonSyntax<'sA> * identifier<'sA> * typ<'sA>
  type paramInfo<'sA> = paramSyntax<'sA> * identifier<'sA> * typ<'sA>

  type dataDec<'eA,'sA> =
    { name : identifier<'sA>
    ; extends : string option
    ; fields : (fieldInfo<'sA> * outputInfo<'sA> option) list
    }

  type dec<'eA, 'sA> =
    | SkippedDec of 'sA * string
    | EndOfFile  of 'sA
    | FunDec     of funDecSyntax<'sA> * identifier<'sA> * paramInfo<'sA> list * expr<'eA, 'sA>
    | ActDec     of actDecSyntax<'sA> * identifier<'sA> * paramInfo<'sA> list * stmt<'eA, 'sA>
    | Data       of dataSyntax<'sA> * dataDec<'eA,'sA>
    | Contract   of contractSyntax<'sA> * identifier<'sA> * string option * fieldInfo<'sA> list * fieldInfo<'sA> list
    | Import     of importSyntax<'sA> * identifier<'sA>
    | DataImport of importSyntax<'sA> * identifier<'sA>
    | Export     of 'sA * dec<'eA, 'sA>

  and compValue =
    | CVDouble of double
    | CVBool  of bool
    | CVStr   of string
    | CVRef   of storeId

  // syntaxAnno
  let noSyntaxAnno          = NoPos
  let noConstSyntax         = { kwStart = noSyntaxAnno; kwEnd = noSyntaxAnno; str = "" }
  let noProjSyntax          = { kwDot = noSyntaxAnno}
  let noColonSyntax         = { kwColon = noSyntaxAnno }
  let noGenericSyntax       = { kwName = noSyntaxAnno; kwLA = noSyntaxAnno; kwRA = noSyntaxAnno}
  let noForComputeSyntax    = { loopName = ""; kwWith = noSyntaxAnno; kwEnd = noSyntaxAnno }
  let noForInSyntax         = { commas = [] ;kwFor = noSyntaxAnno; kwIn = noSyntaxAnno }
  let noMatchSyntax         = { kwMatch = noSyntaxAnno; kwWith = noSyntaxAnno; kwEnd = noSyntaxAnno }
  let noMatchCaseSyntax     = { kwBar = noSyntaxAnno; kwArrow = noSyntaxAnno }
  let noWhereSyntax         = { kwWhere = noSyntaxAnno}
  let noAssignSyntax        = { kwEq = noSyntaxAnno}
  let noListSep             = { kwSep = (None : option<Position>) }
  let noIfSyntax            = { kwIf = noSyntaxAnno; kwThen = noSyntaxAnno; kwElse = noSyntaxAnno}
  let noParSyntax           = { kwPStart = noSyntaxAnno; kwPEnd = noSyntaxAnno}
  let noBinaryGenericSyntax = { kwName = noSyntaxAnno; kwLA = noSyntaxAnno; kwComma = noSyntaxAnno; kwRA = noSyntaxAnno}
  let noNoneSyntax          = { kwNone = noSyntaxAnno; colon = noSyntaxAnno }
  let noUlistSyntax         = { kwLBrck = noSyntaxAnno; kwRBrck = noSyntaxAnno }
  let noLetBindingSynax     = { kwLet = noSyntaxAnno; kwEq = noSyntaxAnno }
  let noActDecSyntax        = { kwAct = noSyntaxAnno; kwLPar = noSyntaxAnno; kwRPar = noSyntaxAnno; kwWith = noSyntaxAnno; kwEnd = noSyntaxAnno }
  let noFunDecSyntax        = { kwFun = noSyntaxAnno; kwLPar = noSyntaxAnno; kwRPar = noSyntaxAnno; kwEq = noSyntaxAnno; }
  let noDataSyntax          = { kwData = noSyntaxAnno; kwEnd = noSyntaxAnno; kwExtends = None }
  let noFCallSyntax         = { kwLPar = noSyntaxAnno; kwRPar = noSyntaxAnno }
  let noACallSyntax         = { kwDo = noSyntaxAnno; kwLPar = noSyntaxAnno; kwRPar = noSyntaxAnno}
  let noCreateRecSyntax     = { kwNew = noSyntaxAnno; LBrack = noSyntaxAnno; RBrack = noSyntaxAnno; commas = []; equals = []}
  let noSwitchSyntax        = { kwSwitch = noSyntaxAnno; lPar = noSyntaxAnno; rPar = noSyntaxAnno; lBrace = noSyntaxAnno; rBrace = noSyntaxAnno}
  let noSwitchCaseSyntax    = { kwCase = noSyntaxAnno; kwColon = noSyntaxAnno}
  let noSifSyntax           = { kwIf = noSyntaxAnno; lPar = noSyntaxAnno; rPar = noSyntaxAnno; lBrace = noSyntaxAnno; rBrace = noSyntaxAnno; }
  let noSelseSyntax         = { kwElse = noSyntaxAnno; lBrace = noSyntaxAnno; rBrace = noSyntaxAnno}

  let nos_TDouble         = TDouble noSyntaxAnno
  let nos_TBool           = TBool noSyntaxAnno
  let nos_TReserve        = TReserve noSyntaxAnno
  let nos_TStr            = TStr noSyntaxAnno
  let nos_TOption t       = TGeneric (noGenericSyntax, GenericOption , [noListSep , t])
  let nos_TRec tag        = TRec (noSyntaxAnno, tag)
  let nos_TList t         = TGeneric (noGenericSyntax, GenericList , [noListSep , t])
  let nos_TMap t1 t2      = TGeneric (noGenericSyntax, GenericMap , [noListSep , t1 ; noListSep , t2])
  let nos_IFun            = TFun (IFun noSyntaxAnno)
  let nos_TSysFunc t1 t2  = TGeneric (noGenericSyntax, GenericSysFun , [noListSep , t1 ; noListSep , t2])
  let nos_TEnum s         = TEnum (noSyntaxAnno, s)
  let nos_TPair t1 t2     = TGeneric (noGenericSyntax, GenericPair , [noListSep , t1 ; noListSep , t2])

  let kw_none    = "None"
  let kw_some    = "Some"
  let kw_end     = "end"
  let kw_module  = "module"
  let kw_true    = "true"
  let kw_false   = "false"
  let kw_LumpedBio    = "LumpedStateBiometric"
  let kw_LumpedFree   = "LumpedStateFreePolicy"
  let kw_LumpedSBio   = "LumpedStateSurrenderBiometric"
  let kw_LumpedSFree  = "LumpedStateSurrenderFreePolicy"
  let kw_LumpedSSurr   = "LumpedStateSurrenderSurrender"
  let kw_StateActive   = "StateActive"
  let kw_StateDead   = "StateDead"
  let kw_StateDisabled   = "StateDisabled"
  let kw_FreePolicyNone   = "FreePolicyRuleNone"
  let kw_FreePolicyScaled   = "FreePolicyRuleScaled"
  let kw_FreePolicyRemoved   = "FreePolicyRuleRemoved"

  let lumpedState_Keywords = [kw_LumpedBio; kw_LumpedFree]
  let lumpedStateSurrender_Keywords = [kw_LumpedSBio; kw_LumpedSFree; kw_LumpedSSurr]
  let state_Keywords = [kw_StateActive; kw_StateDead; kw_StateDisabled]
  let freePolicy_Keywords = [kw_FreePolicyNone; kw_FreePolicyScaled; kw_FreePolicyRemoved]

  let kw_let        = "let"
  let kw_in         = "in"
  let kw_match      = "match"
  let kw_with       = "with"
  let kw_then       = "then"
  let kw_update     = "update"
  let kw_map        = "map"
  let kw_data       = "data"
  let kw_contract   = "contract"
  let kw_requires   = "requires"
  let kw_provides   = "provides"
  let kw_fun        = "fun"
  let kw_export     = "export"
  let kw_import     = "import"
  let kw_importData = "importData"
  let kw_action     = "action"
  let kw_do         = "do"
  let kw_doCSharp   = "doC#"
  let kw_where      = "where"
  let kw_if         = "if"
  let kw_else       = "else"
  let kw_switch     = "switch"
  let kw_case       = "case"
  let kw_new        = "new"
  let kw_extends    = "extends"
  let kw_output = "Output as"

  let kw_lumpedState = "LumpedState"
  let kw_lumpedStateWithSurrender = "LumpedStateWithSurrender"
  let kw_state = "State"
  let kw_freePolicyRule = "FreePolicyRule"


  let enumerations = [ kw_lumpedState ; kw_lumpedStateWithSurrender ; kw_state ; kw_freePolicyRule]

  let keywords =
    [ kw_end
    ; kw_true
    ; kw_false
    ; kw_let
    ; kw_in
    ; kw_match
    ; kw_with
    ; kw_then
    ; kw_update
    ; kw_map
    ; kw_data
    ; kw_fun
    ; kw_action
    ; kw_do
    ; kw_where
    ; kw_if
    ; kw_else
    ; kw_switch
    ; kw_case
    ; kw_new
    ; kw_extends
    ; kw_output
    ; kw_none
    ; kw_some
    ; kw_doCSharp
    ; kw_LumpedBio
    ; kw_LumpedFree
    ; kw_LumpedSBio
    ; kw_LumpedSFree
    ; kw_LumpedSSurr
    ; kw_StateActive
    ; kw_StateDead
    ; kw_StateDisabled
    ; kw_FreePolicyNone
    ; kw_FreePolicyScaled
    ; kw_FreePolicyRemoved
    ; kw_requires
    ; kw_provides
    ; kw_import
    ; kw_module
    ]