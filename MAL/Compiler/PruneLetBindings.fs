namespace itu.dk.MAL

/// See function `pruneDecs` for documentation on main idea of this mudule

module PruneLetBindings =
  open AST
  open ASTUtil
  let maybe = new itu.dk.MAL.Monads.MaybeBuilder()

  let final prefix cur postfix =
    List.rev prefix @ cur :: postfix

  /// <summary>
  /// Attempts to replace (x,e) in e_target. That is: e_target[e/x]
  /// </summary>
  let rec tryPruneExpr count (x,e) (e_target : expr<'eA, 'sA>) =
    match e_target with
    | EMissing   (_)
    | CDouble    (_)
    | CBool      (_)
    | CStr       (_)
    | ENone      (_)
    | CEnum      (_) -> Some (e_target, count)
    | Var        (sA,eA,(_,x1)) ->
      if x1 = x
      then if count = 0
           then Some (e, count + 1)
           else None
      else Some (e_target, count)
    | Pair       (sA,eA,e1, e2) ->
      maybe {
        let! e1', count' = tryPruneExpr count (x,e) e1
        let! e2', count' = tryPruneExpr count' (x,e) e2
        return Pair(sA,eA,e1', e2'), count'
      }
    | Proj (sA,eA,e1,x1) ->
      maybe {
        let! e1', count' = tryPruneExpr count (x,e) e1
        return Proj (sA,eA,e1',x1), count'
      }
    | Filter (sA,eA,e1,tag) ->
      maybe {
        let! e1', count' = tryPruneExpr count (x,e) e1
        return Filter(sA,eA,e1',tag), count'
      }
    | Let (sA,eA,lbs,e1) ->
      maybe {
        let! lbs',count' =
          List.fold
            (fun acc (sA,x1,e1) ->
              maybe {
                let! (acc, count') = acc
                let! (e1',count') = tryPruneExpr count' (x,e) e1
                return (sA,x1,e1') :: acc, count'
              }
            ) (Some ([], count)) lbs
        let! (e1',count') = tryPruneExpr count' (x,e) e1
        return Let(sA, eA, List.rev lbs', e1'), count'
      }
    | If (sA,eA,e1,e2,e3) ->
      maybe {
        let! (e1',count') = tryPruneExpr count (x,e) e1
        let! (e2',count'') = tryPruneExpr count' (x,e) e2 //Note we are branching so reusing count'
        let! (e3',count''') = tryPruneExpr count' (x,e) e3
        return If (sA, eA, e1', e2', e3'), max count'' count'''
      }
    | Match (sA,eA,e1,cases) ->
      maybe {
        let! (e1',count') = tryPruneExpr count (x,e) e1
        let! cases',count' =
          List.fold
            (fun acc (sA,pat,e1) ->
              maybe {
                let! (acc, count'') = acc
                let! (e1',count') = tryPruneExpr count' (x,e) e1 //Note we are branching so reusing count'
                return (sA,pat,e1') :: acc, max count'' count'
              }
            ) (Some ([], count)) cases
        return Match(sA, eA, e1', List.rev cases'), count'
      }
    | Map (sA, eA, forins, wOpt, e1) ->
      maybe {
        let! forins',count' =
          List.fold
            (fun acc forin ->
              maybe {
                let! (acc, count'') = acc
                let! (forin,count') = tryPruneForIn (count'' + 1) (x,e) forin
                return forin :: acc, count''
              }
            ) (Some ([], count)) forins
        let! wOpt', count' =
          match wOpt with
          | None -> Some (None, count')
          | Some (sA, e1) ->
            maybe{
              let! (e1',count') = tryPruneExpr count' (x,e) e1
              return (Some (sA, e1'), count')
            }
        let! (e1',count') = tryPruneExpr count' (x,e) e1
        return Map(sA, eA, List.rev forins', wOpt', e1'), count'
      }
    | FCall (sA,eA,e1,args) ->
      maybe {
         let! (e1',count') = tryPruneExpr count (x,e) e1
         let! args',count' =
           List.fold
             (fun acc (sA,e1) ->
               maybe {
                 let! (acc, count') = acc
                 let! (e1',count') = tryPruneExpr count' (x,e) e1
                 return (sA, e1') :: acc, count'
               }
             ) (Some ([], count')) args
         return FCall(sA, eA, e1', List.rev args'), count'
      }
    | BinOp (sA,eA,op,e1,e2) ->
      maybe {
        let! (e1',count') = tryPruneExpr count (x,e) e1
        let! (e2',count') = tryPruneExpr count' (x,e) e2
        return BinOp (sA,eA,op,e1',e2'), count'
      }
    | EPar (sA,eA,e1) ->
      maybe {
        let! (e1',count') = tryPruneExpr count (x,e) e1
        return EPar (sA,eA,e1'), count'
      }
    | ESome (sA,eA,e1) ->
      maybe {
        let! (e1',count') = tryPruneExpr count (x,e) e1
        return ESome (sA,eA,e1'), count'
      }
    | CreateRec (sA,eA,typeName,fields) ->
      maybe {
        let! fields',count' =
            List.fold
              (fun acc (sA,e1) ->
                maybe {
                  let! (acc, count') = acc
                  let! (e1',count') = tryPruneExpr count' (x,e) e1
                  return (sA, e1') :: acc, count'
                }
              ) (Some ([], count)) fields
        return CreateRec (sA,eA,typeName,List.rev fields'), count'
      }
    | List(sA,eA,elems,anno) ->
      maybe {
        let! elems',count' =
            List.fold
              (fun acc (sA,e1) ->
                maybe {
                  let! (acc, count') = acc
                  let! (e1',count') = tryPruneExpr count' (x,e) e1
                  return (sA, e1') :: acc, count'
                }
              ) (Some ([], count)) elems
        return List (sA,eA,List.rev elems',anno), count'
      }

  and tryPruneForIn count (x,e) (forIn : forIn<'eA, 'sA>) : (forIn<'eA, 'sA> * int) option =
    let sA, iters = forIn
    match iters with
    | Choice1Of2 (x_in_es) ->
      let rec recurse count x_in_es acc =
        match x_in_es with
        | [] -> Some ((sA, Choice1Of2 <| List.rev acc), count)
        | (x1,e1)::x_in_es' ->
          match tryPruneExpr count (x,e) e1 with
          | None -> None
          | Some(e1',count') -> recurse count' x_in_es' ((x1,e1') :: acc)
      recurse count x_in_es []
    | Choice2Of2 ((sA1, x1, x2), e1) ->
      match tryPruneExpr count (x,e) e1 with
      | None -> None
      | Some (e1', count') ->
        Some ((sA, Choice2Of2((sA1, x1, x2), e1')), count' )

  and tryPruneWOpt count (x,e) wOpt =
    match wOpt with
    | None -> Some (None, count)
    | Some (sA,e1) ->
        match tryPruneExpr count (x,e) e1 with
        | None -> None
        | Some (e1', count) -> Some <| (Some (sA, e1'), count)

  let isInAssign ((_,(_,x')),_) x =
    if x = x'
    then None
    else Some ()

  let rec tryPruneStmts count (x,e) (prefix : stmt<'a, 'b> list) (postfix : stmt<'a, 'b> list) : ((stmt<'a, 'b> list * int) option)=
    match postfix with
    | [] -> Some (List.rev prefix , count)
    | s::postfix' ->
      let contTryPrune count s = tryPruneStmts count (x,e) (s::prefix) postfix'
      match s with
      | SkippedStmt (_)                 -> contTryPrune count s
      | SDoCSharp   (_)                 -> contTryPrune count s
      | SUpdate     (sA,forIn,wOpt,s)   ->
        maybe {
          let! (forin',count') = tryPruneForIn count (x,e) forIn
          let! (wOpt', count') = tryPruneWOpt count' (x,e) wOpt
          let! (s', count') = tryPruneStmt (count') (x,e) s
          return! contTryPrune count' (SUpdate (sA, forin', wOpt', s'))
        }
      | SAss (sA,aP,e1) ->
        maybe {
          let! _ = isInAssign aP x
          let! (e1',count') = tryPruneExpr count (x,e) e1
          return! contTryPrune count' (SAss (sA,aP,e1'))
        }
      | SDo (sA, location, args) ->
        maybe {
          let! args',count' =
            List.fold
              (fun acc (sA_arg, e_arg) ->
                maybe {
                  let! (acc, count') = acc
                  let! (e1',count') = tryPruneExpr count' (x,e) e_arg
                  return (sA_arg, e1') :: acc, count'
                }
              ) (Some ([], count)) args
          return! contTryPrune count' (SDo (sA, location, List.rev args'))
        }
      | STransfer (sA, aP1, e1, aP2) ->
        maybe {
          let! _ = isInAssign aP1 x
          let! _ = isInAssign aP2 x
          let! e1', count' = tryPruneExpr count (x,e) e1
          return! contTryPrune count' (STransfer (sA, aP1, e1', aP2))
        }
      | SOverwrite  (sA, aP1, e1) ->
        maybe {
          let! _ = isInAssign aP1 x
          let! e1', count' = tryPruneExpr count (x,e) e1
          return! contTryPrune count' (SOverwrite (sA, aP1, e1'))
        }
      | SBlock      (sA, ss)  ->
        maybe {
          let! ss', count' = tryPruneStmts count (x,e) [] ss
          return! contTryPrune count' (SBlock (sA, ss'))
        }
      | SLet (sA, x1, e1) ->
        maybe {
          let! e1', count' = tryPruneExpr count (x,e) e1
          return! contTryPrune count' (SLet (sA, x1, e1'))
        }
      | SIf (sA, cond, t, fOpt) ->
        maybe {
          let! cond',count' = tryPruneExpr count (x,e) cond
          let! t',count'' = tryPruneStmt count' (x,e) t
          let! fOpt',count''' =
            match fOpt with
            | Some(sA', f) ->
              maybe { let! f',count = tryPruneStmt count' (x,e) f in return Some (sA', f'), max count count''}
            | None ->
              maybe { return None, count'' }
          return! contTryPrune count''' (SIf (sA, cond', t', fOpt'))
        }
      | SMatch(sA, exp, cases) ->
        maybe {
          let! exp',count' = tryPruneExpr count (x,e) exp
          let! cases',count' =
            List.fold
              (fun acc (sA,pat,s1) ->
                maybe {
                  let! (acc, count'') = acc
                  let! (s1',count') = tryPruneStmt count' (x,e) s1 //Note we are branching so reusing count'
                  return (sA,pat,s1') :: acc, max count'' count'
                }
              ) (Some ([], count)) cases
          return! contTryPrune count' (SMatch(sA, exp', cases'))
        }
  and tryPruneStmt count (x,e) s =
    let a = tryPruneStmts count (x,e) [] [s]
    Option.map (fun (ss,count) -> List.head ss, count) a


  let rec pruneExpr (e : expr<typ<'a>, 'sA>) : (expr<typ<'a>, 'sA> * bool) =
    match e with
    | EMissing   (_)
    | CDouble    (_)
    | CBool      (_)
    | CStr       (_)
    | ENone      (_)
    | CEnum      (_)
    | Var        (_) -> e, true
    | Pair       (sA,eA,e1,e2) ->
      let e1', p1 = pruneExpr e1
      let e2', p2 = pruneExpr e2
      Pair (sA,eA,e1',e2'), p1 && p2
    | ESome (sA, eA, e1) ->
      let e1', p1 = pruneExpr e1
      ESome (sA, eA, e1'), p1
    | Proj (sA,eA,e1,x1) ->
      let e1', p1 = pruneExpr e1
      Proj (sA,eA,e1',x1), p1
    | Filter (sA,eA,e1,tag) ->
      let e1', p1 = pruneExpr e1
      Filter (sA,eA,e1',tag), p1
    | Let (sA,eA,lbs,e1) ->
      let rec recurse lbs e1 =
        match lbs with
        | [] -> [], e1, true
        | (sA, (sA_x,x), e_lb) :: lbs' ->
          // We are attempting to substituting outer let-binding into both lbs' and e1
          let e_lb', p' = pruneExpr e_lb
          let tryThis =
            maybe {
              let! e1', count' = tryPruneExpr 0 (x, e_lb') e1
              let! lbs', e1',_ =
                List.fold
                  ( fun acc (sA_i, iter, e_lb_i) ->
                    maybe {
                      let! lbs, e1', count' = acc
                      let! e_lb_i, count' = tryPruneExpr count' (x, e_lb') e_lb_i
                      return ((sA_i, iter, e_lb_i) :: lbs, e1', count')
                    }
                  ) (Some ([], e1', count')) lbs'
              return (List.rev lbs', e1')
            }
          match p', tryThis with //p' indicates whether we should substitue at all due to side effects
          | false,_ | _,None -> // We could not substitue so we continue on tail
            let acc, e1', p1' = recurse lbs' e1
            (sA, (sA_x,x), e_lb) :: acc, e1', p' && p1'
          | true,Some (lbs', e1') -> // We continue on substituting the updates lbs
            let acc, e1', p1' = recurse lbs' e1'
            acc, e1', p1' // We can remove this binding level since we succesfully pruned it
      let lbs', e1', p1 = recurse lbs e1
      Let (sA,eA,lbs',e1'), p1
    | If (sA,eA,e1,e2,e3) ->
      let e1', p1 = pruneExpr e1
      let e2', p2 = pruneExpr e2
      let e3', p3 = pruneExpr e3
      If (sA,eA,e1',e2',e3'), p1 && p2 && p3
    | Match (sA,eA,e1,cases) ->
      let e1', p1 = pruneExpr e1
      let cases' =
        List.map
          (fun (sA, idemt, e1) ->
            let e1', p1' = pruneExpr e1
            (sA, idemt, e1'), p1'
          ) cases
      let cases', p' =
        List.foldBack
          (fun (case, p') (acc, p) -> case :: acc, p && p')
            cases' ([], p1)
      Match (sA,eA,e1',cases'), p'
    | Map (sA, eA, forins, wOpt, e1) ->
      let forins', pForIn =
        List.foldBack
          (fun (forin) (accFI, accP) ->
            let forIn, p = pruneForIn forin
            (forIn :: accFI, accP && p)
          ) (forins) ([], true)
      let wOpt', pOpt = pruneWOpt wOpt
      let e1', p1 = pruneExpr e1
      Map (sA, eA, forins', wOpt', e1'), pOpt && p1 && pForIn
    | FCall (sA,eA,e1,args) ->
      let e1', p1 = pruneExpr e1
      let args', pArgs =
        List.foldBack (fun (sA, e1) (accA, pA) ->
          let e1', p1 = pruneExpr e1
          (sA, e1') :: accA, p1 && pA && (match eAnnoOf e1 with | TVoid -> true | _ -> false)
          ) args ([], p1)
      FCall (sA,eA,e1',args'), pArgs
    | BinOp (sA,eA,op,e1,e2) ->
      let e1, p1 = pruneExpr e1
      let e2, p2 = pruneExpr e2
      BinOp (sA,eA,op,e1,e2), p1 && p2
    | EPar  (sA,eA,e1) ->
      let e1', p1 = pruneExpr e1
      EPar(sA, eA, e1'), p1
    | CreateRec (sA,eA,typeName,fields) ->
      let fields', p =
        List.foldBack
          (fun (sA, e1) (accE, accP) ->
            let e1', p1 = pruneExpr e1
            (sA, e1') :: accE, p1 && accP
          ) fields ([], true)
      CreateRec (sA,eA,typeName,fields'), p
    | List(sA,eA,elems,anno) ->
      let elems', p = List.foldBack (fun (sA, e1) (accE, accP)-> let e1', p1 = pruneExpr e1 in (sA, e1') :: accE, p1 && accP) elems ([], true)
      List(sA,eA,elems',anno), p

  and pruneForIn (iters : forIn<typ<'a>, 'sA>) : forIn<typ<'a>, 'sA> * bool =
    let sA, iters = iters
    let iters, p =
      match iters with
      | Choice1Of2 (forins) ->
        let forins = List.map (fun (sA, e) -> let e', p = pruneExpr e in (sA, e'),p ) forins
        let forins, p =
          List.foldBack
            (fun (case, p') (acc, p) -> case :: acc, p && p')
              forins ([], true)

        Choice1Of2 forins, p
      | Choice2Of2 (kvp, e) ->
        let e', p = pruneExpr e
        Choice2Of2 (kvp, e'),p
    (sA, iters), p

  and pruneWOpt wOpt =
    match wOpt with
    | Some (sA,e1) ->
      let e1', p1 = pruneExpr e1
      Some (sA, e1'), p1
    | None -> None, true

  /// <summary>
  /// Prunes a list of statements which have the same binding scope.
  /// Prefix are already pruned.
  /// Postfix is to-be pruned, if we encounter a let binding it must be replaced in the postfix.
  /// </summary>
  let rec pruneStmts (prefix : stmt<typ<'a>, 'b> list) (postfix : stmt<typ<'a>, 'b> list) =
    match postfix with
    | []            -> List.rev prefix
    | s :: postfix' ->
      let prefix, postfix' =
        match s with
        | SkippedStmt (_) -> s :: prefix, postfix'
        | SDoCSharp   (_) -> s :: prefix, postfix'
        | SUpdate     (sA,forIn,wOpt,s)   ->
          let forIn',_ = pruneForIn forIn
          let wOpt',_ = pruneWOpt wOpt
          SUpdate (sA, forIn', wOpt', pruneStmt s) :: prefix, postfix'
        | SAss        (sA,aP,e)           ->
          SAss        (sA,aP, fst <| pruneExpr e) :: prefix, postfix'

        | SDo         (sA, location, args) -> SDo         (sA, location, List.map (fun (sA, e) -> (sA, fst <| pruneExpr e)) args) :: prefix, postfix'
        | STransfer   (sA, aP1, e, aP2)   -> STransfer   (sA, aP1, fst <| pruneExpr e, aP2) :: prefix, postfix'
        | SOverwrite  (sA, aP1, e)        -> SOverwrite  (sA, aP1, fst <| pruneExpr e) :: prefix, postfix'
        | SBlock      (sA, ss)            -> SBlock      (sA, pruneStmts [] ss) :: prefix, postfix'
        | SIf         (sA, cond, t, fOpt) ->
          let cond',_ = pruneExpr cond
          let t' = pruneStmt t
          let fOpt' = Option.map (fun (sA, f) -> sA, pruneStmt f) fOpt
          SIf(sA, cond', t', fOpt') :: prefix, postfix'
        | SMatch(sA, exp, cases) ->
          let exp',_ = pruneExpr exp
          let cases' = List.map (fun (sA, idemt, s1) -> (sA, idemt, pruneStmt s1)) cases
          SMatch(sA, exp', cases') :: prefix, postfix'
        | SLet        (_, (_,x), e) ->
          let e', p = pruneExpr e
          match p, tryPruneStmts 0 (x,e') [] postfix' with
          | true,Some (postfix', _) -> prefix, postfix'
          | _,_ -> s :: prefix, postfix'

      pruneStmts prefix postfix'
  and pruneStmt s =
    let s' = pruneStmts [] [s]
    assert(List.length s' = 1)
    List.head s'

  let rec pruneDec (dec : dec<typ<'a>, 'b>) =
    match dec with
    | FunDec     (sA,f_name,ps,e) -> FunDec (sA,f_name,ps, fst <| pruneExpr e)
    | ActDec     (sA,f_name,ps,s) -> ActDec(sA,f_name,ps, pruneStmt s)
    | Export     (sA, dec)        -> Export (sA, pruneDec dec)
    | Data       (_)
    | SkippedDec (_)
    | EndOfFile  (_)
    | DataImport (_)
    | Import     (_)
    | Contract   (_) -> dec

  /// <summary>
  /// Removes all let bindings where the bound variable only
  /// occurs once in the body of the expression.
  ///
  /// Assumption: All variable names are unique
  ///
  /// </summary>
  let pruneDecs (decs : dec<typ<'a>, 'b> list) =
    List.map pruneDec decs
