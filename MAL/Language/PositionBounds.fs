namespace itu.dk.MAL

module PositionBounds =
  open AST

  type BoundSide =
    | Left
    | Right

  let posToIndex (pos_n_offset : Position * int) =
    snd pos_n_offset +
      match fst pos_n_offset with
      | FullPos (_, r) -> r
      | Missing (m)    -> m
      | NoPos          -> failwith "Missing position"

  let patternToPos (side : BoundSide) (pattern : matchPattern<Position>) : Position =
    let left, right =
      match pattern with
      | PatNone sA -> sA, sA
      | PatSome (sA,(sA',_)) -> sA, sA'.kwEnd
      | PatTag  ((sA,_),(sA',_)) -> sA, sA'.kwEnd
      | PatMissing sA -> sA, sA
    match side with
    | Left -> left
    | Right -> right

  let rec leftExprSAnno = function
    | EMissing (_,sA)             -> (sA, 0)
    | CDouble  (_,sA,_)           -> (sA.kwStart, 0)
    | CBool    (_,sA,_)           -> (sA.kwStart, 0)
    | CStr     (_,sA,_)           -> (sA.kwStart, 0)
    | CEnum    (_,sA,_)           -> (sA.kwStart, 0)
    | ENone    (_,sA,_)           -> (sA.kwNone, 0)
    | Var      (_,sA,_)           -> (sA.kwStart, 0)
    | Let      (_,sA,lbs,_)       ->
      let left = match lbs with | [] -> sA.kwIn | (sA,_,_)::_ -> sA.kwLet
      (left, 0)
    | Match    (_,sA,_,_)         -> (sA.kwMatch, 0)
    | Map      (_,sA,forins,whr,_) ->
      let left =
        match forins with
        | [] -> match whr with | Some (sAwth,_) -> sAwth.kwWhere | None -> sA.kwWith
        | (sA_forin,_) :: _ -> sA_forin.kwFor
      (left, 0)
    | CreateRec (_,sA,_,_)  -> (sA.kwNew, 0)
    | EPar     (_,sA,_)     -> (sA.kwPStart, 0)
    | Pair     (_,sA,_,_)   -> (sA.kwLA, 0)
    | If       (_,sA,_,_,e3)      -> (sA.kwIf, 0)
    | ESome    (_,sA,e)           -> (sA, 0)
    | Proj     (_,_,e,(sA,_))     -> (leftExprSAnno e)
    | Filter   (_,sA,e,_)         -> (leftExprSAnno e)     //Todo left
    | List     (_,sA,_,_)           -> (sA.kwLBrck, 0)
    | FCall    (_,sA,e,_)         -> (leftExprSAnno e)
    | BinOp    (_,sA,_,e1,e2)     -> (leftExprSAnno e1)

  let rec rightExprSAnno = function
    | EMissing (_,sA)             -> (sA, 0)
    | CDouble  (_,sA,_)           -> (sA.kwEnd, 0)
    | CBool    (_,sA,_)           -> (sA.kwEnd, 0)
    | CStr     (_,sA,_)           -> (sA.kwEnd, 0)
    | CEnum    (_,sA,_)           -> (sA.kwEnd, 0)
    | ENone    (_,sA,_)           -> (sA.colon, 0)
    | ESome    (_,sA,e)           -> rightExprSAnno e
    | Var      (_,sA,_)           -> (sA.kwEnd, 0)
    | Proj     (_,_,e,(sA,_))     -> (sA.kwEnd, 0)
    | Filter   (_,sA,e,_)         -> (sA.kwColon, 0)
    | Let      (_,sA,lbs,_)       -> (sA.kwEnd, 3)
    | If       (_,sA,_,_,e3)      -> (rightExprSAnno e3)
    | Match    (_,sA,_,_)         -> (sA.kwEnd, 3)
    | Map      (_,sA,forins,whr,_)-> (sA.kwEnd, 3)
    | List     (_,sA,_,_)           -> (sA.kwRBrck, 1)
    | FCall    (_,sA,e,_)         -> (sA.kwRPar, 1)
    | BinOp    (_,sA,_,e1,e2)     -> (rightExprSAnno e2)
    | CreateRec (_,sA,e1,fields)  -> (sA.RBrack, 1)
    | EPar     (_,sA,_)           -> (sA.kwPEnd, 1)
    | Pair     (_,sA,_,_)         -> (sA.kwRA, 1)

  // Should not have an exponential runningtime?
  let rec exprSAnno (side : BoundSide) =
    match side with
    | Left -> leftExprSAnno
    | Right -> rightExprSAnno

  let leftPos  (e : expr<'a, Position>) : Position = fst << exprSAnno BoundSide.Left <| e
  let rightPos (e : expr<'a, Position>) : Position = fst << exprSAnno BoundSide.Right <| e

  let rec leftStmtSAnno = function
    | SkippedStmt(sA)               -> sA, 0
    | SUpdate(sA,(sA',_),_,_)       -> (sA'.kwFor, 0)
    | SAss(sA,((_,(sA_id,_)),_),e)  -> (sA_id.kwStart, 0)
    | SLet(sA,_,e)                  -> (sA.kwLet, 0)
    | SDo(sA,e,_)                   -> (sA.kwDo, 0)
    | SDoCSharp(sA,_)               -> (sA.kwDoCS, 0)
    | SOverwrite(sA,((_,(sA_id,_)),_),e)
      -> (sA_id.kwStart, 0)
    | STransfer(sA,((_,(sA_id,_)),_),e, ((_,(sA_id2,_)),_)) //todo fix
      -> (sA_id.kwStart, 0)
    | SIf(sA, _, _, _)           -> sA.kwIf, 0
    | SMatch(sA, _, _)           -> sA.kwSwitch, 0
    | SBlock (sA,[])             -> (sA, 0)
    | SBlock (_,stmts)           -> leftStmtSAnno (List.head stmts)

  let rec rightStmtSAnno = function
    | SkippedStmt(sA)           -> sA, 0
    | SUpdate(sA,(sA',_),_,_)   -> (sA.kwEnd, 3)
    | SAss(sA,((_,(sA_id,_)),_),e)
                                -> (rightExprSAnno e)
    | SLet(sA,_,e)              -> (rightExprSAnno e)
    | SDo(sA,e,_)               -> (sA.kwRPar, 1)
    | SDoCSharp(sA,_)           -> (sA.kwDoCS, 6)
    | SOverwrite(sA,((_,(sA_id,_)),_),e)
      -> (rightExprSAnno e)
    | STransfer(sA,((_,(sA_id,_)),_),e, ((_,(sA_id2,_)),_)) //todo fix
      -> (sA_id2.kwStart, 0)
    | SIf(sA, _, _, None)        -> (sA.rBrace, 1)
    | SIf(_, _, _, Some(sA',_))  -> (sA'.rBrace,1)
    | SMatch(sA, _, _)           -> (sA.rBrace, 1)
    | SBlock (sA,[])             -> (sA,2)
    | SBlock (_,stmts)           -> (rightStmtSAnno (List.last stmts))

  let rec stmtSAnno (side : BoundSide) =
    match side with
    | Left -> leftStmtSAnno
    | Right -> rightStmtSAnno

  let leftDecSAnno = function
    | SkippedDec(sA,_)     -> sA , 0
    | EndOfFile(sA)        -> sA , 0
    | FunDec(sA,_,_,e)     -> (sA.kwFun, 0)
    | ActDec(sA,_,_,stmt)  -> (sA.kwAct, 0)
    | Data(sA,_)           -> (sA.kwData, 0)

  let rightDecSAnno = function
    | SkippedDec(sA,_)     -> sA , 0
    | EndOfFile(sA)        -> sA , 0
    | FunDec(sA,_,_,e)     -> (rightExprSAnno e)
    | ActDec(sA,_,_,stmt)  -> (rightStmtSAnno stmt)
    | Data(sA,_)           -> (sA.kwEnd, 3)

  let decSAnno (side : BoundSide) =
    match side with
    | Left -> leftDecSAnno
    | Right -> rightDecSAnno

  let rec exprBound (side : BoundSide) (expr : expr<'aE,Position>) =
    posToIndex <| exprSAnno side expr

  let exprBounds (expr : expr<'aE,Position>) = (exprBound Left expr, exprBound Right expr)

  let rec stmtBound (side : BoundSide) (stmt : stmt<'aE,Position>) =
    posToIndex <| stmtSAnno side stmt

  let stmtBounds (stmt : stmt<'aE,Position>) = (stmtBound Left stmt, stmtBound Right stmt)

  let rec decBound (side : BoundSide) (dec : dec<'aE,Position>) =
    posToIndex <| decSAnno side dec

  let decBounds (dec : dec<'aE,Position>) = (decBound Left dec, decBound Right dec)

  let rec posToLineCol (src : string) posI =
    let mutable counter = 0
    let mutable lineCount = 0
    let mutable col = 0
    while counter < posI do
      counter <- counter + 1
      if counter >= src.Length
      then ()
      else ()
      if src.[counter] = '\n'
      then lineCount <- lineCount + 1
           col <- 0
      else col <- col + 1
    lineCount, col

  let lineToBounds (src : string) targetLine =
    let mutable counter = 0
    let mutable lineCount = 0
    let mutable lower = 0
    let mutable higher = 0
    while lineCount <= targetLine do
      counter <- counter + 1
      if src.[counter] = '\n'
      then lineCount <- lineCount + 1
           if lineCount = targetLine
           then lower <- counter
           else
              if lineCount > targetLine
              then higher <- counter
              else ()
      else ()
    (lower, higher)


