namespace itu.dk.MAL

module ASTUtil =
  open AST
  open Monads

  // Environment
  let pureLookup env key = List.tryFind <| (fun (k,v) -> k = key) <| env

  let lookup env key =
    match pureLookup env key with
    | Some (k, v) -> v
    | None   -> failwith <| sprintf "Could not find key: %A" key

  let lookupResult env key errBounds =
    match pureLookup env key with
    | Some (k, v) -> result { return v}
    | None   -> Result.Error (sprintf "Store lookup error: Could not find %s in %A" key (List.map fst env), errBounds)

  let lookupAnno env key =
      match pureLookup env key with
      | Some (k, v) -> Some v
      | None   -> None

  let extEnv env x v = (x, v)::env
  let appendEnv env app = app@env

  // AST
  let rec eAnnoOf expr =
    match expr with
    | EMissing (eA,_)       -> eA
    | CDouble  (eA,_,_)     -> eA
    | CBool    (eA,_,_)     -> eA
    | CEnum    (eA,_,_)     -> eA
    | Pair     (eA,_,_,_)   -> eA
    | CStr     (eA,_,_)     -> eA
    | ENone    (eA,_,_)     -> eA
    | ESome    (eA,_,_)     -> eA
    | Var      (eA,_,_)     -> eA
    | Proj     (eA,_,_,_)   -> eA
    | Filter   (eA,_,_,_)   -> eA
    | Let      (eA,_,_,_)   -> eA
    | If       (eA,_,_,_,_) -> eA
    | Match    (eA,_,_,_)   -> eA
    | Map      (eA,_,_,_,_) -> eA
    | List     (eA,_,_,_)   -> eA
    | FCall    (eA,_,_,_)   -> eA
    | BinOp    (eA,_,_,_,_) -> eA
    | EPar     (eA,_,_)     -> eA
    | CreateRec (eA,_,_,_)  -> eA

  and firstSLetBinding binding =
    match binding with
    | (a,_,_) -> a.kwLet
  and firstSForIn (aS,(_,_,x), e) = aS.kwFor

  let getBinding binding =
    match binding with
    | (_,x,e) -> (x,e)

  // GCP seems wrong
  let greatestCommonTyp (t1: typ<'sA>) (t2: typ<'sA>) : typ<'sA> =
    if t1 = t2
    then t1
    else
      match t1,t2 with
      | TRec (sA, tags1), TRec (_, tags2) -> TRec (sA, Set.toList (Set.union (Set.ofList tags1) (Set.ofList tags2)))
      | _ , _ -> TErr

  let rec getSuperTags (superMap : Map<string, string option>) (tag : string) =
    match Map.tryFind tag superMap with
    | None -> []
    | Some (None)       -> []
    | Some (Some super) -> super :: (getSuperTags superMap super)

  let rec firstCommonTag (tags1 : string list) (tags2 : string list) =
    match tags2 with
    | [] -> None
    | tag :: tags2' ->
      if (List.contains tag tags1)
      then Some tag
      else firstCommonTag tags1 tags2'


  ///<summary>
  /// Tries to find the greatest common type (GCT) between `t1` and `t2`.
  /// `unionTypes` indicates whether a uniontype is an accepted result.
  /// E.g. given `t1 = Risk` and `t2 = Expense` then GCT is `Expense_Risk` when unionTypes. Otherwise GCT is `Group`
  ///</summary>
  let tryGreatestComTyp (superMap : Map<string, string option>) (unionTypes : bool)  (t1: typ<'sA>) (t2: typ<'sA>) : typ<'sA> option =
    if t1 = t2
    then Some t1
    else
      match t1,t2 with
      | TRec (sA, tags1), TRec (_, tags2) ->
          let superTypes1 = getSuperTags superMap (List.head tags1)
          let superTypes2 = getSuperTags superMap (List.head tags2)
          match superTypes1, superTypes2 with
          | st1::_,st2::_ when st1 = st2 && unionTypes ->
            let t1',t2' = tags1.Head, tags2.Head
            Some (TRec (sA, [if t1' < t2' then sprintf "%s_%s" t1' t2' else sprintf "%s_%s" t2' t1']))
          | _ ->
            match firstCommonTag superTypes1 superTypes2 with
            | None     -> None
            | Some tag -> Some (TRec (sA, [tag]))
      | _ , _ -> None

  let isTag extsMap tag = Map.containsKey tag extsMap

  let rec tagIsSubOf (superTypes : superTypeMap) (tag1 : pureTag) (tag2 : pureTag) =
      tag1 = tag2 ||
      match Map.tryFind tag1 superTypes with
      | None -> false
      | Some None -> false
      | Some (Some super) -> tag2 = super || tagIsSubOf superTypes super tag2

  let rec isSubOfTyp (superTypeMap : superTypeMap) (t1: typ<'sA>) (t2: typ<'sA>) =
    if t1 = t2 then true
    else
      match (t1,t2) with
      | TRec (_,tags1),TRec (_,tags2) ->
        List.forall
          (fun tag1 ->
            List.exists
              (fun tag2 -> tag1 = tag2 || tagIsSubOf superTypeMap tag1 tag2)
              tags2
          ) tags1
      | TGeneric (_, GenericList, [_,t1]), TGeneric (_, GenericList, [_,t2]) -> isSubOfTyp superTypeMap t1 t2
      | _ -> false

  let superOf (inheritence : subTypeMap) t =
    Map.findKey (fun k v -> List.exists ((=) t) v) inheritence

  let tagIsSubType (inheritence : subTypeMap) t = Map.exists (fun _k v -> List.exists ((=) t) v) inheritence

  let superOfOrT (inheritence : subTypeMap) tag =
    if tagIsSubType inheritence tag
    then superOf inheritence tag
    else tag

  let rec leafTypesOf (inheritence : subTypeMap) tag =
    match Map.tryFind tag inheritence with
    | Some tags ->
      Seq.collect (leafTypesOf inheritence) tags
    | None -> Seq.singleton tag

  let rec subTypesOf (inheritence : subTypeMap) tag =
    let rec inner tag =
      match Map.tryFind tag inheritence with
      | Some tags ->
        (Seq.append (Seq.singleton tag) (Seq.collect inner tags))
      | None -> Seq.singleton tag

    match Map.tryFind tag inheritence with
    | Some tags -> Seq.collect inner tags
    | None -> Seq.empty


  let topologicalSort (graph: Map<string, Set<string>>) =
    let noInEdge graph k =
      not <|
            Map.exists
              (fun k' outEdges' -> Set.contains k outEdges'
              ) graph

    let L = []
    let S =
      List.filter
        (fun (k, outEdges) -> noInEdge graph k) <| Map.toList graph
    let rec sort graph S L =
      match S with
      | [] -> (List.map fst <| Map.toList graph, L)
      | (s, outEdges)::S' ->
        let graph = Map.remove s graph
        let S' =
          Set.fold (
            fun S' edge ->
              if (noInEdge graph edge)
              then let outEdges = (Map.find edge graph)
                   (edge, outEdges)::S'
              else S') S' outEdges
        sort graph S' ((s, outEdges)::L)
    sort graph S L

  let tagUnionI tags = String.concat "_" <| List.sort tags

  let rec exprContainsAnno test e =
    test <| eAnnoOf e ||
    match e with
    | EMissing (_,_)           -> false
    | CDouble  (_,_,_)         -> false
    | CBool    (_,_,_)         -> false
    | CStr     (_,_,_)         -> false
    | CEnum    (_,_,_)         -> false
    | Pair     (_,_,e1,e2)     -> exprContainsAnno test e1 || exprContainsAnno test e2
    | Var      (_,_,x)         -> false
    | ENone    (_)             -> false
    | ESome    (_,_,e1)        -> exprContainsAnno test e1
    | Proj     (_,_,e1,x)      -> exprContainsAnno test e1
    | Filter   (_,_,e1,_)      -> exprContainsAnno test e1
    | Let      (_,_,lbs,e1)    -> List.exists (fun (_,x,e1) -> exprContainsAnno test e1) lbs || exprContainsAnno test e1
    | If       (_,_,e1,e2,e3)  -> exprContainsAnno test e1 || exprContainsAnno test e2 || exprContainsAnno test e3
    | Match    (_,_,e1,mcs)    -> exprContainsAnno test e1 || List.exists (fun (_,_,e1) -> exprContainsAnno test e1) mcs
    | Map      (_,_,fis,wh,e1) ->
      let inForins =
        List.exists (fun (_,iters) ->
          let reg many = List.exists (fun (x,e1) -> exprContainsAnno test e1) many
          let kvp (_, e) = exprContainsAnno test e
          Choices.joinChoice reg kvp iters
        ) fis
      inForins
      || exprContainsAnno test e1
      || match wh with
          | None -> false
          | Some (_,e1) -> exprContainsAnno test e1
    | List     (_,_,es,_)      -> List.exists ( exprContainsAnno test << snd) es
    | FCall    (_,_,e1,args)   -> exprContainsAnno test e1 || List.exists ( exprContainsAnno test << snd) args
    | BinOp    (_,_,_,e1,e2)   -> exprContainsAnno test e1 || exprContainsAnno test e2
    | EPar     (_,_,e1)        -> exprContainsAnno test e1
    | CreateRec (_,_,_,fields) -> List.exists ((exprContainsAnno test) << snd) fields

  let rec stmtContainsAnno test stmt =
    match stmt with
    | SkippedStmt(_) -> false
    | SUpdate (_,(_,Choice1Of2 [_,e1]),wh,stmt)
    | SUpdate (_,(_,Choice2Of2 (_,e1)),wh,stmt) ->
      exprContainsAnno test e1
      || stmtContainsAnno test stmt
      || match wh with
          | None -> false
          | Some (_,e1) -> exprContainsAnno test e1
    | SUpdate (_,(_,_),wh,stmt)  -> failwith "impossible"
    | SAss (_,_,e1)              -> exprContainsAnno test e1
    | SLet (_,x,e1)              -> exprContainsAnno test e1
    | SDo (_, location, args)           -> false // Todo
    | SDoCSharp (_, _)           -> false // Todo
    | SOverwrite (_,_,e1)        -> exprContainsAnno test e1
    | STransfer (_,_,e1,_)       -> exprContainsAnno test e1
    | SIf(_, cond, t, fOpt)      ->
      exprContainsAnno test cond
      || stmtContainsAnno test t
      || match fOpt with Some(_, f) -> stmtContainsAnno test f | None -> false
    | SMatch(_, e, cases) -> exprContainsAnno test e || List.exists (fun (_,_,s) -> stmtContainsAnno test s) cases
    | SBlock (_,stmts)           -> List.exists (stmtContainsAnno test) stmts

  let rec decContainsAnno test dec =
    match dec with
    | SkippedDec(_,_)           -> false
    | EndOfFile(_)              -> false
    | FunDec (_,x,paramInfos,e) ->  exprContainsAnno test e
    | ActDec (_,x,paramInfos,stmt) -> stmtContainsAnno test stmt
    | Export (_,dec') -> decContainsAnno test dec'
    | Data(_,dataDecs)      -> false
    | Contract(_) -> false
    | Import(_) -> false

  let decEquivalent dec1 dec2 =
    match (dec1, dec2) with
    | SkippedDec(sA1,_),  SkippedDec(sA2,_)  -> sA1 = sA2
    | EndOfFile(sA1),     EndOfFile(sA2)     -> sA1 = sA2
    | FunDec (sA1,_,_,_), FunDec (sA2,_,_,_) -> sA1 = sA2
    | ActDec (sA1,_,_,_), ActDec (sA2,_,_,_) -> sA1 = sA2
    | Data(sA1,_),        Data(sA2,_)    -> sA1 = sA2
    | Export(sA1,_),      Export(sA2,_)    -> sA1 = sA2
    | Import(sA1,_),      Import(sA2,_)    -> sA1 = sA2
    | _ -> false

  let mergeTrees
        decs
        (decs1 : dec<typ<Position>,Position> list)
        (decs2 : dec<typ<Position>,Position> list) =
    let searchDecs dec (decs: dec<typ<Position>,Position> list) =
      List.tryFind (decEquivalent dec) decs
    List.collect
      (fun dec ->
        match searchDecs dec decs1,searchDecs dec decs2 with
        | Some (d1), Some (d2) ->
          if decContainsAnno (fun anno -> match anno with | TErr (_) -> true | _ -> false) d2
          then [d2]
          else [d1]
        | None, Some(d) -> [d]
        | Some(d), None -> [d]
        | None, None -> []
      ) decs

  let isNullable t =
    match t with
    | TErr | TVoid | TMissing _ -> failwith "Should never be considered whether this is nullable"
    | TDouble _ | TReserve _
    | TBool   _ | TEnum _ -> true
    | TGeneric _
    | TStr     _
    | TRec     _
    | TFun     _
    | TModule _
    | TAct     _ -> false

  let rec findActDec decs nm =
    match decs with
    | [] -> None
    | dec :: decs ->
      match dec with
      | ActDec (_, (_,nm'), _, stmt) when nm = nm' -> Some dec
      | Export (_, ActDec (_, (_,nm'), _, stmt)) when nm = nm' -> Some dec
      | _ -> findActDec decs nm

  let tryFindFunction decs f =
    List.tryFind
      (fun dec ->
        match dec with
        | FunDec (_, (_, f'), _, _) when f = f' -> true
        | ActDec (_, (_, f'), _, _) when f = f' -> true
        | Export (_, FunDec (_, (_, f'), _, _)) when f = f' -> true
        | Export (_, ActDec (_, (_, f'), _, _)) when f = f' -> true
        | _ -> false
      ) decs