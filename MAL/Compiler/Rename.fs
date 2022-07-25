namespace itu.dk.MAL

module Rename =
  open AST
  open GeneralUtil
  open ASTUtil

  let mutable count = 0;

  let rec renewVar var env =
    let var = if var = "@" then "gen" else var
    count <- count + 1
    let ident = sprintf "%s_%d" var count
    if List.exists (fun (k,_) -> k  = ident) env
    then renewVar var env
    else ident

  // Creates a new name for variable and adds it to environemnt
  let handleVar env x =
    let x' = renewVar x env
    (extEnv env x x', x')

  let rec unVarExpr env expr =
    match expr with
    | EMissing (_) | CDouble (_) | CBool (_) | CStr (_) | ENone _ | CEnum _ -> expr
    | Pair   (eA,sA, e1, e2) -> Pair (eA,sA, unVarExpr env e1, unVarExpr env e2)
    | Var    (eA, sA, (sA_x, x)) ->  Var (eA, sA, (sA_x, lookup env x))
    | ESome  (eA, sA, e1)        -> ESome  (eA, sA, unVarExpr env e1)
    | Proj   (eA, sA, e1, ident) -> Proj(eA, sA, unVarExpr env e1, ident)
    | Filter (eA, sA, e1, filters) -> Filter (eA, sA, unVarExpr env e1, filters)
    | Let    (eA, sA, lbs, e1) ->
      let (env', lbs') =
        List.fold
          (fun (env', acc) lb ->
            let env',lb' = unVarLb env' lb
            (env', lb'::acc)
          ) (env, []) lbs
      let e1' = unVarExpr env' e1
      Let (eA, sA, List.rev lbs', e1')
    | If (eA, sA, e1, e2, e3) ->
      If (eA, sA, unVarExpr env e1, unVarExpr env e2, unVarExpr env e3)
    | Match  (eA, sA, e1, mcs) ->
      let e1' = unVarExpr env e1
      let mcs' =
        List.map
          (fun (aS, matchPattern, mc_e) ->
            let env', matchPattern' =
              match matchPattern with
              | PatSome (t,(sA,x)) ->
                let env' , x' = handleVar env x
                env' , PatSome (t,(sA,x'))
              | PatTag (tag,(sA,x)) ->
                let env' , x' = handleVar env x
                env' , PatTag(tag,(sA,x'))
              | PatNone _ | PatMissing _ -> env, matchPattern
            let mc_e' = unVarExpr env' mc_e
            (aS, matchPattern', mc_e')
          ) mcs
      Match (eA, sA, e1', mcs')
    | Map (eA, sA, forIns, clause, e1) ->
      let env', forIns =
        List.fold
          (fun (env', acc) forIn ->
            let env', forIn' = unVarForIn env' forIn
            (env', forIn'::acc)
          ) (env, []) forIns
      let clause' = Option.map (unVarClause env') clause
      let e1' = unVarExpr env' e1
      Map (eA, sA, List.rev forIns, clause', e1')
    | List(eA, sA, es,anno) -> List(eA, sA, List.map (fun (aS, e) -> (aS,unVarExpr env e)) es,anno)
    | FCall  (eA, sA, e1, es) ->
      let e1' = unVarExpr env e1
      let es' = List.map (fun (aS, e) -> (aS,unVarExpr env e)) es
      FCall  (eA, sA, e1', es')
    | BinOp  (eA, sA, op, e1, e2) -> BinOp(eA, sA, op, unVarExpr env e1, unVarExpr env e2)
    | CreateRec (eA,sA,name,fields) -> CreateRec (eA,sA,name,List.map (fun (nm,e1) -> (nm, unVarExpr env e1)) fields)
    | EPar   (eA, sA, e1) -> EPar(eA, sA, unVarExpr env e1)

  and unVarForIn env forIn =
    let (sA, iters) = forIn
    match iters with
    | Choice1Of2 many ->
      let (env', forin') =
        List.fold (fun (accEnv, accForIn) ((sA_x,x), e) ->
          let e' = unVarExpr env e
          let (accEnv', x') = handleVar accEnv x
          accEnv', ((sA_x,x'), e') :: accForIn
        ) (env, []) many
      env', (sA, Choice1Of2 <| List.rev forin')
    | Choice2Of2 ((sA_kvp,(sA_k, k),(sA_v, v)), e) ->
      let e' = unVarExpr env e
      let (ae, k') = handleVar env k
      let (ae', v') = handleVar ae v
      ae', (sA, Choice2Of2((sA_kvp,(sA_k, k'),(sA_v, v')), e'))
  and unVarLb env (sA, (sA_x,x), e) =
    let e' = unVarExpr env e
    let (env', x') = handleVar env x
    env', (sA, (sA_x,x'), e')
  and unVarClause env (sA, e) = (sA, unVarExpr env e)

  let rec unVarStmt env stmt =
    match stmt with
    | SUpdate(sA,forIn, clause, stmt) ->
      let (env', forIn') = unVarForIn env forIn
      let clause' = Option.map (unVarClause env') clause
      let _,stmt' = unVarStmt env' stmt
      env, SUpdate(sA,forIn', clause', stmt')
    | SAss(aS, ((t, (sA_x, x)), projs), e) ->
      let x' = lookup env x
      let e' = unVarExpr env e
      env, SAss(aS, ((t, (sA_x,x')), projs), e')
    | SLet(lb) ->
      let env', lb' = unVarLb env lb
      env', SLet(lb')
    | SDo(sA, location, args) ->
      let args' = List.map (fun (sa, e) -> (sa, unVarExpr env e)) args
      env, SDo(sA, location, args')
    | SDoCSharp(sA, extIdent) ->
      env, SDoCSharp(sA, extIdent)
    | SBlock (sA, stmts) -> env, SBlock (sA, unVarStmts env stmts)
    | SOverwrite(sA, ((t, (sA_x, x)), projs), e) ->
      let x' = lookup env x
      let e' = unVarExpr env e
      env, SOverwrite(sA, ((t, (sA_x,x')), projs), e')
    | STransfer(sA, ((t1, (sA_x1, x1)), projs1), e, ((t2, (sA_x2, x2)), projs2)) ->
      let x1' = lookup env x1
      let x2' = lookup env x2
      let e' = unVarExpr env e
      env, STransfer(sA, ((t1, (sA_x1, x1')), projs1), e', ((t2, (sA_x2, x2')), projs2))
    | SIf(sA, cond, t, fOpt) ->
      let cond' = unVarExpr env cond
      let _,t' = unVarStmt env t
      let _,fOpt' =
        match fOpt with
        | Some (sA, f) -> let (env',f') = unVarStmt env f in env',Some (sA, f')
        | None -> env, None
      env, SIf(sA, cond', t', fOpt')
    | SMatch(sA, exp, cases) ->
      let exp' = unVarExpr env exp
      let caseRename (sA, t, s) =
        match t with
        | PatSome(sA', (sA_v, var)) ->
          let (env', var') = handleVar env var
          let _,s' = unVarStmt env' s
          (sA, PatSome(sA', (sA_v, var')), s')
        | PatTag(sA', (sA_v, var)) ->
          let (env', var') = handleVar env var
          let _,s' = unVarStmt env' s
          (sA, PatTag(sA', (sA_v, var')), s')
        | other ->
          let _,s' = unVarStmt env s
          (sA, other, s')
      let cases' = List.map caseRename cases
      env, SMatch(sA, exp', cases')
    | SkippedStmt _ -> (env, stmt)
  and unVarStmts env stmts =
    let (_, stmts') =
      List.fold
        (fun (env,acc) stmt ->
          let (env', stmt') = unVarStmt env stmt
          (env', stmt'::acc)
        ) (env, []) stmts
    List.rev stmts'

  let rec unVarDec env dec =
    match dec with
    | FunDec(sA,ident,args, e1) ->
      let (env', args') =
        List.fold
          (fun (env, acc) (sA_p,(sA_x,x),t) ->
            let env', x' = handleVar env x
            extEnv env x x', (sA_p,(sA_x,x'),t)::acc
          ) (env, []) args
      let e1' = unVarExpr env' e1
      FunDec(sA,ident, List.rev args', e1')
    | ActDec(sA, ident, args, stmt) ->
      let (env', args') =
        List.fold
          (fun (env, acc) (sA_p,(sA_x,x),t) ->
            let env', x' = handleVar env x
            extEnv env x x', (sA_p,(sA_x,x'),t)::acc
          ) (env, []) args
      let _,stmt' = unVarStmt env' stmt
      ActDec(sA, ident, args', stmt')
    | Export(sA, dec) -> Export(sA, unVarDec env dec)
    | extend -> extend
  let unVarDecs (env : (string * string) seq) decs =
    List.map (unVarDec <| Seq.toList env) decs