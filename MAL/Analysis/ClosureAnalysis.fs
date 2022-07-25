namespace itu.dk.MAL

module ClosureAnalysis =

  open AST
  open Structure

  // Quite simple closure analysis that finds all function calls to user defined functions.
  // It is assumed that user defined functions can not exist in records, this should be guarenteed by the type system.

  let rec eClosureAnalysis (env : environment<Set<string>>) e  =
    match e with
    | EMissing _                        -> Set.empty
    | CDouble  _                        -> Set.empty
    | CBool    _                        -> Set.empty
    | CStr     _                        -> Set.empty
    | CEnum    _                        -> Set.empty
    | ENone    _                        -> Set.empty
    | ESome    (_,_,e')                 -> eClosureAnalysis env e'
    | Var      (_,_,(_,x))              ->
      match List.tryFind (fun (x',_) -> x=x') env with
      | None -> Set.empty
      | Some (_,fNames) -> fNames
    | Pair     (_,_, e1, e2)            -> Set.union <| eClosureAnalysis env e1 <| eClosureAnalysis env e2
    | Proj     (_, _, e, _)             -> eClosureAnalysis env e
    | Filter   (_, _, e, _)             -> eClosureAnalysis env e
    | Let      (_, _, lbs, eb)          ->
      let (env', ca) = List.fold (fun (accEnv, accCA) lb -> let e,c = lbClosureAnalysis accEnv lb in e, Set.union accCA c) (env, Set.empty) lbs//(eClosureAnalysis env eb) lbs
      eClosureAnalysis env' eb |> Set.union ca
    | If       (_,_,e1,e2,e3)           -> Set.union (eClosureAnalysis env e3) <| Set.union (eClosureAnalysis env e1) (eClosureAnalysis env e2)
    | Match    (_,_,e,mcs)              -> List.fold (fun accCA mc -> Set.union accCA <| mcClosureAnalysis env mc ) (eClosureAnalysis env e) mcs
    | Map      (_,_, forIns, where, e)  ->
      let env',cas = List.fold (fun (accEnv, accCA) mc -> let e,c = forInClosureAnalysis accEnv mc in e, Set.union accCA c) (env, Set.empty) forIns//(eClosureAnalysis env e) forIns
      Set.union cas (whereClosureAnalysis env' where) |>
      Set.union (eClosureAnalysis env' e)
    | List     (_,_,es,_)                 -> List.fold (fun accCA (_,mc) -> Set.union accCA <| eClosureAnalysis env mc ) Set.empty es
    | FCall    (_,_,e,es)               -> List.fold (fun accCA (_,mc) -> Set.union accCA <| eClosureAnalysis env mc ) (eClosureAnalysis env e) es
    | BinOp    (_,_,_,e1,e2)            -> Set.union (eClosureAnalysis env e1) (eClosureAnalysis env e2)
    | EPar     (_,_,e)                  -> eClosureAnalysis env e
    | CreateRec (_,_,_,fields)          -> List.fold (fun accCA (_,e) -> Set.union accCA <| eClosureAnalysis env e ) Set.empty fields
  and lbClosureAnalysis env lb =
    match lb with
    | (_,(_,x),e) -> let clos = eClosureAnalysis env e in (x,clos)::env, clos
  and mcClosureAnalysis env (_,pat,e) =
    match pat with
    | PatMissing _ | PatNone _ -> eClosureAnalysis env e
    | PatTag(_,(_,x)) | PatSome(_,(_,x)) ->  eClosureAnalysis ((x, Set.empty)::env) e
  and forInClosureAnalysis env ((_,args) : forIn<'a, 'b>) =
    let reg many =
      List.fold
        (fun (accEnv, accCA) ((_,x),e) -> let c = eClosureAnalysis env e in (x,c)::accEnv, Set.union accCA c) (env, Set.empty) many
    let kvp ((_,(_,k),(_,v)),e) = let clos = eClosureAnalysis env e in (k,clos)::(v,clos)::env, clos
    Choices.joinChoice reg kvp args

  and whereClosureAnalysis env whereO =
    match whereO with
    | None -> Set.empty
    | Some (_,e) -> eClosureAnalysis env e

  let rec stmtClosureAnalysis (env : environment<Set<string>>) stmt : environment<Set<string>> * Set<string>  =
    match stmt with
    | SkippedStmt _ -> (env, Set.empty)
    | SUpdate    (_, forIn, where, stmts) ->
      let env', caFor = forInClosureAnalysis env forIn
      let caWhere = whereClosureAnalysis env' where
      let (_, caStmt) = (stmtClosureAnalysis env' stmts)
      (env, Set.union (Set.union caFor caWhere) caStmt)
    | SAss    (_,_,e) -> (env, eClosureAnalysis env e)
    | SOverwrite (_,_,e) -> (env, eClosureAnalysis env e)
    | STransfer (_,_,e,_) -> (env, eClosureAnalysis env e)
    | SLet    lb -> lbClosureAnalysis env lb
    | SDo     (_, ((_,(_,act_nm)), locations), args) ->
      let initial =
        match locations with
        | [] ->
          match ASTUtil.pureLookup env act_nm with
          | None -> Set.empty
          | Some (_,fNames) -> fNames
        | _ -> Set.empty
      env , List.fold (fun accCA (_,mc) -> Set.union accCA <| eClosureAnalysis env mc ) initial args
    | SDoCSharp (_,_) -> env, Set.empty
    | SBlock (_,stmts) -> stmtsClosureAnalysis env stmts
    | SIf(_, e, t, fOpt) ->
      let caCond = eClosureAnalysis env e
      let _,caTrue = stmtClosureAnalysis env t
      let _,caFalse = Option.map (snd >> stmtClosureAnalysis env) fOpt |> Option.defaultValue ([], Set.empty)
      (env, Set.union (Set.union caCond caTrue) caFalse)
    | SMatch(_, e, cases) ->
      let ec = eClosureAnalysis env e
      env, List.fold (fun clos sCase -> Set.union clos <| snd (scClosureAnalysis env sCase)) ec cases
  and scClosureAnalysis (env : environment<Set<string>>) (_,pat,s) =
    match pat with
    | PatMissing _ | PatNone _ -> stmtClosureAnalysis env s
    | PatTag(_,(_,x)) | PatSome(_,(_,x)) ->  stmtClosureAnalysis ((x, Set.empty)::env) s
  and stmtsClosureAnalysis (env : environment<Set<string>>) (stmts : stmt<'a,'b> list) : environment<Set<string>> * Set<string>  =
      List.fold
        (fun (accEnv, accCa) stmt ->
           let (env', ca') = stmtClosureAnalysis accEnv stmt
           (env', Set.union accCa ca')) (env, Set.empty) stmts

  let buildInitEnv decs =
    let identityEnv =
      List.fold
        (fun mapEnv dec ->
          match dec with
          | Export(_,FunDec (_,(_,f),_,_)) -> Map.add f (Set.singleton f) mapEnv
          | Export(_,ActDec (_,(_,f),_,_)) -> Map.add f (Set.singleton f) mapEnv
          | FunDec (_,(_,f),_,_)           -> Map.add f (Set.singleton f) mapEnv
          | ActDec (_,(_,f),_,_)           -> Map.add f (Set.singleton f) mapEnv
          | _ -> mapEnv) Map.empty decs
    let usagesEnv =
      List.fold
        (fun (mapEnv: Map<string, Set<string>>) dec ->
          match dec with
          | Export(_,FunDec (_,(_,f),args,e))
          | FunDec (_,(_,f),args,e) ->
            let argEnv = List.map (fun (_,(_,arg),_) -> (arg, Set.empty)) args
            let ca = eClosureAnalysis (argEnv@(Map.toList identityEnv)) e
            Map.add f ca mapEnv
          | Export(_,ActDec(_,(_,f),args,stmt))
          | ActDec (_,(_,f),args,stmt) ->
            let argEnv = List.map (fun (_,(_,arg),_) -> (arg, Set.empty)) args
            let (_,ca) = stmtClosureAnalysis (argEnv@(Map.toList identityEnv)) stmt
            Map.add f ca mapEnv
          | _ -> mapEnv) identityEnv decs

    let rec reachable (mapEnv: Map<string, Set<string>>) visited s  =
      if Set.contains s visited
      then (Set.empty, visited)
      else
        Set.fold
          (fun (reach, visited) s' ->
            let (reach', visited') = reachable mapEnv visited s'
            (Set.union reach reach', visited')
          ) (mapEnv.[s], Set.add s visited) mapEnv.[s]

    let reachableEnv =
      Map.map
        (fun k v ->
          let (reach, _visit) = reachable usagesEnv Set.empty k
          reach
        ) usagesEnv

    let (recursiveFuns, reachableEnv) = ASTUtil.topologicalSort reachableEnv

    (recursiveFuns,
      List.map
        (fun (k, v) ->
          (k, Set.add k v)
        ) reachableEnv)

  type closureResult =
    { recursives : string list
    ; initFuns : string list
    ; manageFuns : string list
    ; unusedFuns : string list
    }
  let closureAnalysisDecs decs : closureResult =
    let (recursiveFuns, analysisEnv) = buildInitEnv decs

    let (_env, initClosures) =
      match ASTUtil.findActDec decs "init" with
      | Some (ActDec(_,_,_,stmt))
      | Some (Export (_, ActDec (_, (_,_), _, stmt))) -> stmtClosureAnalysis analysisEnv stmt
      | _ -> (analysisEnv, Set.empty)

    let (_env, updateClosures) =
      match ASTUtil.findActDec decs "manage" with
      | Some (ActDec(_,_,_,stmt))
      | Some (Export (_, ActDec (_, (_,_), _, stmt))) -> stmtClosureAnalysis analysisEnv stmt
      | _ -> (analysisEnv, Set.empty)

    let topologicalOrdered = List.map fst analysisEnv

    { recursives = recursiveFuns
    ; initFuns   = List.filter (fun v -> Set.contains v initClosures) topologicalOrdered
    ; manageFuns = List.filter (fun v -> Set.contains v updateClosures) topologicalOrdered
    ; unusedFuns  = List.filter (fun v -> not ((Set.contains v initClosures) || (Set.contains v updateClosures) || v = "init" || v = "manage")) topologicalOrdered
    }

