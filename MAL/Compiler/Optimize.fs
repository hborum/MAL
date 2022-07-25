namespace itu.dk.MAL

module Optimize =

  open ToIntermediate

  // Removed when changing to sequences
  //let optimize =
  //  let rec inner result = function
  //  | Declare(_,x)::a::Let(res, (t, Var x'), _)::rest when x=x' ->
  //    let newA = renameVar x res a
  //    Declare(t, res)::newA::rest |> inner result
  //  | Declare(_,x)::a::Assign((_,Var res), (_, Var x'))::rest when x=x' ->
  //     let newA = renameVar x res a
  //     newA::rest |> inner result
  //  | Let(x, (_, Init(ts, [])), b)::a::Let(res, (_, Var x'), _)::rest when x=x' ->
  //    let newA = renameVar x res a
  //    Let(res, (ts, Init(ts, [])), b)::newA::rest |> inner result
  //  | If((_, CstB b), t, f)::rest ->
  //    let winner = if b then t else f |> optSingle
  //    winner::rest |> inner result
  //  | If(b, t, f)::rest -> inner (If(b, optSingle t, optSingle f)::result) rest
  //  | (Block [single])::rest -> optSingle single::rest |> inner result
  //  | Match(e, cases)::rest -> inner (Match(e, List.map (fun (t,n,s) -> (t,n, optSingle s)) cases)::result) rest
  //  | For(counterName, iters, s, d)::rest -> inner (For(counterName, iters, optSingle s, d)::result) rest
  //  | Declare(_, x)::Assign((_,Var x'),ae)::rest when x=x'-> inner result (Let(x, ae, false)::rest)
  //  | Block stmts :: rest-> inner (Block (inner [] stmts) :: result) rest
  //  | x::rest -> inner (x::result) rest
  //  | [] -> List.rev result
  //  and optSingle s = List.head <| inner [] [s]
  //  inner []