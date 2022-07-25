namespace itu.dk.MAL

module ParserValidation =
    open AST
    open Structure

    let errorOrigin = "ParserValidation"
    let prettyErrorOrigin = "Parse Error"

    /// <summary>
    /// Returns error message if a declaration or statement is skipped.
    /// Returns None otherwise.
    /// </summary>
    let checkSkipped (pos: Position) (text: string) =
        match pos with
        | FullPos(_, start) ->
            let words = List.ofArray <| text.Split [|' '|]
            match words with
            | w::_ ->   Some {
                            message = sprintf "Did not expect the keyword '%s'" w
                            index = start
                            length = w.Length
                            origin = errorOrigin
                            prettyOrigin = prettyErrorOrigin
                            severity = Severity.Error
                            quickfixMsg = ""
                        }
            | [] -> Some {
                            message = "Internal error. Please report this error."
                            index = start
                            length = 1
                            origin = errorOrigin
                            prettyOrigin = prettyErrorOrigin
                            severity = Severity.InternalError
                            quickfixMsg = ""
                        } // Shouldn't happen

        | _ -> None

    /// <summary>
    /// Returns an error when `pos` is missing.
    /// Returns None otherwise
    /// </summary>
    let checkMissingPos (pos: Position) (expected: string) =
        match pos with
        | Missing(start) ->
            let noQuickFixKw = ["end";"=";"expression"]
            let noQuickFix = expected.Split([|''';' '|]) |> Seq.exists (fun x -> List.contains x noQuickFixKw)
            let qfMsg = if noQuickFix then "" else (sprintf "Add %s" expected)
            Some {
                message = sprintf "Expected %s" expected
                index = start
                length = 1
                origin = errorOrigin
                prettyOrigin = prettyErrorOrigin
                severity = Severity.Error
                quickfixMsg = qfMsg
            }
        | _ -> None

    /// <summary>
    /// Calls checkMissingPos (used when a keyword is missing)
    /// </summary>
    let checkMissingPosKw (pos: Position) (expected: string) : (ErrorMessage option) =
        checkMissingPos pos <| sprintf "the keyword '%s'" expected

    /// <summary>
    /// Calls checkMissingPos (used when a ??type declaration?? is missing)
    /// </summary>
    let checkMissingPosType (pos: Position) (expected: string) : (ErrorMessage option) =
        checkMissingPos pos <| sprintf "a %s" expected

    /// <summary>
    /// Returns an error when pattern is missing.
    /// Returns None otherwise
    /// </summary>
    let checkMissingPattern (pattern : matchPattern<Position>) =
      match pattern with
      | PatMissing sA ->
        Some {
          message = "Expected a valid match pattern. Either a tag, None, or Some. e.g. \"| Interest iGrp -> 2\""
          index = fullStart sA
          length = 1
          origin = errorOrigin
          prettyOrigin = prettyErrorOrigin
          severity = Severity.Error
          quickfixMsg = ""
            }
      | _ -> None

    let checkMissingPosOptional (pos: Position option) expected =
        match pos with
        | Some p -> checkMissingPosKw p expected
        | None -> None

    /// <summary>
    /// Generates error message `msg`
    /// Highlights error from `start` to `start` + `length`
    /// If applicaple, send QuickFix message `qfMsg`
    /// </summary>
    let generateError (pos: Position) (msg: string) (length: int) (qfMsg : string) =
        match pos with
        | Missing(start) | FullPos(_,start) ->
            Some {
                message = msg
                index = start
                length = length
                origin = errorOrigin
                prettyOrigin = prettyErrorOrigin
                severity = Severity.Error
                quickfixMsg = qfMsg
            }
        | NoPos ->
            Some {
                message = "Error produced with no position"
                index = 0
                length = 1
                origin = errorOrigin
                prettyOrigin = prettyErrorOrigin
                severity = Severity.InternalError
                quickfixMsg = ""
            }

    let getPos = function
     | FullPos(_,s) -> s
     | Missing s -> s
     | NoPos -> -1

    /// <summary>
    /// Adjusts a position by `n`.
    /// This solution is a little problematic - if we change the syntax, the adjustment by `n` may not be the prettiest anymore!
    /// </summary>
    let getPosAdjust (pos : Position) (n : int) =
        match pos with
        | FullPos(s1,s2) -> FullPos(s1,s2 + n)
        | Missing s -> Missing (s + n)
        | NoPos -> NoPos

    /// <summary>
    /// Checks whether a keyword is missing, for an entire structure from `structStart` to `structEnd`
    /// </summary>
    let checkMissingPosKwLength (structStart : Position)
                                (structEnd : Position)
                                (endKwLenght : int)
                                (pos: Position) // if pos is missing, then we are in trouble
                                (expected: string)  =
        let e = checkMissingPosKw pos expected
        if e.IsSome
        then generateError (getPosAdjust structStart 1) e.Value.message (getPos structEnd - getPos structStart + endKwLenght) e.Value.quickfixMsg
        else e

    let isMissingError e =
         match e with
         | EMissing(_) -> true
         | _ -> false

    let rec checkMissingPosForType (typ: typ<Position>) expected =
        match typ with
        | TMissing(s) -> [checkMissingPosType s expected]
        | TGeneric (s, _, []) -> // Todo, we need to add error of missing gen
            [ checkMissingPosKw s.kwName "Map"
            ; checkMissingPosKw s.kwLA "<"
            ; checkMissingPosKw s.kwRA ">"]
        | TGeneric (s, _, (_,ht)::tail) ->
            // changed to `checkMissingPosForType`, for a better error message
            let headErr = checkMissingPosForType ht "type name"
            let errs =
              List.collect
                (fun (si,ti) ->
                  let err_i =
                    match si.kwSep with
                    | None -> None
                    | Some sA -> checkMissingPosKw sA ","
                  err_i :: checkMissingPosForType ti "type name"
                ) tail
            headErr @
            errs @
            [ checkMissingPosKw s.kwName "Map"
            ; checkMissingPosKw s.kwLA "<"
            ; checkMissingPosKw s.kwRA ">"]
        | _ -> [None]

    let getFirstError (errs: ErrorMessage option list) =
        let realErrs = List.choose id errs
        match realErrs with
        | e::_ -> Some e
        | [] -> None

    let collectErrors errors =
        let rec run input =
            match input with
            | (Some e)::ls ->
                (Some e):: // If we find an error, skip all subsequent errors
                (run (List.skipWhile (fun err ->
                    match err with
                    | Some _ -> true
                    | None -> false
                ) ls))
            | None::ls -> run ls
            | [] -> []
        run errors

    // Validates expressions
    let rec PVal_expr (expr : expr<unit, Position>) =
        match expr with
        // Not a keyword! Not a type
        | EMissing(_, pos) -> [checkMissingPos pos <| sprintf "Expected an expression"]
        | BinOp(_, _, _, e1, e2) -> PVal_expr e1 @ PVal_expr e2
        | If(_, syntax, e1, e2, e3) ->
            // let checkMissingPosKw = checkMissingPosKwLength syntax.kwIf syntax.kwElse 0
            // TODO: Can these positions be improved?
            //       For one-liner if-then-else statements, it looks good to adjust the postition a little.
            //       For if-then-else statements with line skips between each keyword, it looks good to adjust even more (there are more characters to skip?).
            //       I will look into this!
            let kw_then_pos = getPosAdjust syntax.kwThen 1
            let kw_else_pos = getPosAdjust syntax.kwElse 1

            let checkMissingPosIf = checkMissingPosKwLength syntax.kwIf syntax.kwIf 2 syntax.kwIf kw_if
            let checkMissingPosThen = checkMissingPosKwLength kw_then_pos kw_then_pos 4 syntax.kwThen kw_then
            let checkMissingPosElse =
              //let's only include `else`-error if there is no `then`-error. Otherwise, the position of the error seems random + confusing.
              match checkMissingPosThen with
              | Some err -> None
              | None -> checkMissingPosKwLength kw_else_pos kw_else_pos 4 syntax.kwElse kw_else

            let syntaxErrs =
                checkMissingPosIf::
                PVal_expr e1@
                checkMissingPosThen::
                PVal_expr e2@
                checkMissingPosElse::
                PVal_expr e3
            syntaxErrs
        // TODO: There are duplicate 'variable not defined' errors for Map, but they are not generated here. Investigate!
        | Map(_, syntax1, forIns, filter, e2) ->
            let checkMissingPosKw = checkMissingPosKwLength syntax1.kwEnd syntax1.kwEnd 3
                                    //(fst forIns.Head).kwFor syntax1.kwEnd 3
            let forInErrs =
              List.collect (fun (syn, iters) ->
                let kw_in_pos = getPosAdjust syn.kwIn 1 //adjust syn.kwIn
                let errs1 = [
                  checkMissingPosKwLength syn.kwFor syn.kwFor 0 syn.kwFor kw_map
                  checkMissingPosKwLength kw_in_pos kw_in_pos 2 syn.kwIn kw_in
                ]
                let regular listIters =
                  List.collect (fun ((pos,_),e) ->
                    let miss = checkMissingPos pos.kwStart "variable name"
                    let exprErrs = PVal_expr e
                    miss :: exprErrs
                  ) listIters
                let kvp ((pos,_,_), e) =
                  let miss = checkMissingPos pos.kwName "("
                  let exprErrs = PVal_expr e
                  miss :: exprErrs
                let errs2 = Choices.joinChoice regular kvp iters
                errs1@errs2
              ) forIns
            let filterErrs = match filter with
                             | Some (wSyn, e3) ->
                                (checkMissingPosKw wSyn.kwWhere kw_where)::(PVal_expr e3)
                             | None -> []
            let computeErrs =
                checkMissingPosKw syntax1.kwWith kw_with::
                PVal_expr e2@
                [checkMissingPosKw syntax1.kwEnd (sprintf "%s for map-expression" kw_end)]
            let allErrs = forInErrs@filterErrs@computeErrs
            allErrs
        | Let(_, syntax, lets, e1) ->
            // TODO: generally for a missing `end` - can we improve the position?
            let kw_end_pos = getPosAdjust syntax.kwEnd -3
            let kw_in_pos  = getPosAdjust syntax.kwIn 1

            let checkMissingPosKw = checkMissingPosKwLength syntax.kwEnd syntax.kwEnd 3//((fun (f,_,_) -> f) lets.Head).kwLet syntax.kwEnd 3
            let letErrs = List.foldBack (fun (syn, (pos,_), e1) errs ->
                                let checkMissingPosKwLet = checkMissingPosKwLength syn.kwLet syn.kwLet 0 syn.kwLet kw_let
                                let kw_eq_pos  = getPosAdjust syn.kwEq 1
                                let checkMissingPosKwEq = checkMissingPosKwLength kw_eq_pos kw_eq_pos 1 syn.kwEq "="
                                let syntaxErrs = [
                                    checkMissingPosKwLet
                                    checkMissingPos pos.kwStart "variable name"
                                    checkMissingPosKwEq
                                ]
                                let exprErrs = PVal_expr e1
                                syntaxErrs@exprErrs@errs
                            ) lets []
            let syntaxErrs =
                checkMissingPosKwLength kw_in_pos kw_in_pos 2 syntax.kwIn kw_in::
                PVal_expr e1@
                [checkMissingPosKw syntax.kwEnd (sprintf "%s for let-expression" kw_end)]

            let allErrs = letErrs@syntaxErrs
            allErrs
        // EPar is already good!
        | EPar(_, syntax, e1) ->
            let checkMissingPosKw = checkMissingPosKwLength syntax.kwPStart syntax.kwPEnd 3
            let syntaxErrs =
                    checkMissingPosKw syntax.kwPStart "("::
                    PVal_expr e1@
                    [checkMissingPosKw syntax.kwPEnd ")"]
            syntaxErrs
        // TODO: More precise positions
        | Match(_, syntax, e1, cases) ->
            //let checkMissingPosKw = checkMissingPosKwLength syntax.kwMatch syntax.kwEnd 3
            let caseErrs =
                List.foldBack (fun (syn, matchPattern, e) errs ->
                    let syntaxErrs = [ checkMissingPattern matchPattern ]
                    let exprErrs = PVal_expr e
                    syntaxErrs@exprErrs@errs
                ) cases []
            let syntaxErrs =
                checkMissingPosKwLength syntax.kwMatch syntax.kwMatch 5 syntax.kwMatch kw_match::
                PVal_expr e1@
                checkMissingPosKwLength syntax.kwWith syntax.kwWith 4 syntax.kwWith kw_with::
                (
                if cases.Length = 0
                then generateError syntax.kwMatch "Expected '|' in match expression" (kw_match.Length) "Add '|'"
                else None)::
                caseErrs@
                [checkMissingPosKwLength syntax.kwEnd syntax.kwEnd 3 syntax.kwEnd (sprintf "%s for match expression" kw_end)]

            syntaxErrs
        // List is already good!
        | List(_, syntax, exprs,_) ->
            let checkMissingPosKw = checkMissingPosKwLength syntax.kwLBrck syntax.kwRBrck 1
            let exprErrs = List.foldBack (fun (s,e) errs ->
              let sepErr = checkMissingPosOptional s.kwSep ";"
              sepErr::(PVal_expr e)@errs) exprs []
            let syntaxErrs =
              checkMissingPosKw syntax.kwLBrck "["::
              exprErrs@
              [checkMissingPosKw syntax.kwRBrck "]"]

            syntaxErrs
        // TODO: The rest
        | FCall(_, syntax, e1, exprs) ->
            let checkMissingPosKw = checkMissingPosKwLength syntax.kwLPar syntax.kwRPar 1
            let exprErrs = List.foldBack (fun (s,e) errs ->
                let commaErr = checkMissingPosOptional s.kwSep ","
                commaErr::(PVal_expr e)@errs) exprs []
            let syntaxErrs =
                PVal_expr e1@
                checkMissingPosKw syntax.kwLPar "("::
                exprErrs@
                [checkMissingPosKw syntax.kwRPar ")"]

            syntaxErrs
        | Proj(_, syntax, e, (pos,x)) ->
            let checkMissingPosKw = checkMissingPosKwLength syntax.kwDot pos.kwEnd 3
            let syntaxErrs =
                PVal_expr e@
                checkMissingPosKw syntax.kwDot "."::
                [checkMissingPosKw pos.kwStart x]

            syntaxErrs
        | Filter(_, syntax, e, x) ->
            let checkMissingPosKw = checkMissingPosKwLength syntax.kwColon (fst x.Head) 1//not sure
            let syntaxErrs =
                PVal_expr e@
                checkMissingPosKw syntax.kwColon ":"::[]
                //[checkMissingPosKw (fst x) "filter"] tofix

            syntaxErrs
        | Pair (_, syntax, e1, e2) ->
            let checkMissingPosKw = checkMissingPosKwLength syntax.kwLA syntax.kwRA 1
            let syntaxErrs =
                    checkMissingPosKw syntax.kwLA "("::
                    PVal_expr e1@
                    PVal_expr e2@
                    [checkMissingPosKw syntax.kwRA ")"]
            syntaxErrs
        | CreateRec _ //Should CreateRec not validate field-values somehow?
        | CBool _ | CDouble _ | CStr _ | CEnum _
        | ENone _ | ESome _ | Var _ -> [None]

    ///<summary>
    /// Validates all projections `projs` contains a dot and a projection field.
    ///</summary>
    let PVal_projs projs =
      List.foldBack (fun (_,s,(pos,x)) errs ->
        let syntaxErrs = [
            checkMissingPosKw s.kwDot "."
            checkMissingPosKw pos.kwStart x
        ]
        syntaxErrs@errs) projs []

    let checkMissingPosSingleKw kw i = checkMissingPosKwLength kw kw i kw

    ///<summary>
    /// Validates all statements.
    ///</summary>
    let rec PVal_stmt s =
      match s with
      | SUpdate(syntax, (syntax2, iter) , where, stmt) ->
          let posStart,e =
            match iter with
            | Choice1Of2 [(pos,_),e] -> pos.kwStart,e
            | Choice2Of2 ((pos,_,_), e) -> pos.kwLA,e
            | _ -> failwith "impossible"
          let kw_end_pos = getPosAdjust syntax.kwEnd -3
          let checkMissingPosKw = checkMissingPosKwLength kw_end_pos syntax.kwEnd 3
          let kw_in_pos = getPosAdjust syntax2.kwIn 1
          let syntaxErrs =
              checkMissingPosKwLength syntax2.kwFor syntax2.kwFor 0 syntax2.kwFor kw_update::
              checkMissingPos posStart "variable"::
              checkMissingPosKwLength kw_in_pos kw_in_pos 2 syntax2.kwIn kw_in:: // pretty good!
              PVal_expr e@
              (match where with
              | Some (wSyn, e3) ->
                  (checkMissingPosKwLength wSyn.kwWhere wSyn.kwWhere 0 wSyn.kwWhere kw_where)::(PVal_expr e3)
              | None -> [])@
              //checkMissingPosKwLength syntax.kwWith syntax.kwWith 0 syntax.kwWith kw_with::
              PVal_stmt stmt@
              [checkMissingPosKw syntax.kwEnd (sprintf "%s for update-statement" kw_end)]
          let withErrs =
              match (checkMissingPosKwLength syntax2.kwIn syntax2.kwIn 0 syntax2.kwIn kw_in) with
              | Some err -> []
              | None -> [(checkMissingPosKwLength syntax.kwWith syntax.kwWith 0 syntax.kwWith kw_with)] //TODO: Cannot provoke this error?
          syntaxErrs @ withErrs
      | SAss(syntax,((_,(pos,_)),projs),e) ->
          // If nothing is here, maybe we could add a general "statement expected" error?
          let checkMissingPosKw = checkMissingPosKwLength pos.kwStart syntax.kwEq 1
          let syntaxErrs =
              checkMissingPos pos.kwStart "variable name"::
              PVal_projs projs@
              checkMissingPosKw syntax.kwEq "=":: //for missing 'end' in 'init'/'manage'/'update' block, this error is generated.
              PVal_expr e
          syntaxErrs
      | SLet(syntax,(pos,_),e) ->
          //let checkMissingPosKwEq = checkMissingPosKwLength syntax.kwLet pos.kwEnd 3
          let syntaxErrs =
              checkMissingPosKw syntax.kwLet kw_let::
              checkMissingPos pos.kwStart "variable name"::
              checkMissingPosKw syntax.kwEq "="::
              PVal_expr e
          syntaxErrs
      | SDo(syntax, (_,projs), args) ->
        let checkMissingPosKw = checkMissingPosKwLength syntax.kwDo syntax.kwRPar 1
        checkMissingPosKw syntax.kwDo kw_do ::
        PVal_projs projs
      | SDoCSharp(syntax, _) ->
        let checkMissingPosKw = checkMissingPosKwLength syntax.kwDoCS syntax.kwDoCS 1
        [ checkMissingPosKw syntax.kwDoCS kw_do ]

      | SOverwrite (syntax,((_,(pos,_)),projs),e) ->
        let checkMissingPosKw = checkMissingPosKwLength pos.kwStart syntax.Arrow 1
        let syntaxErrs =
            checkMissingPos pos.kwStart "variable name"::
            PVal_projs projs@
            checkMissingPosKw syntax.Arrow "="::
            PVal_expr e
        syntaxErrs
      | STransfer (syntax,((_,(pos1,_)),projs1),e,((_,(pos2,_)),projs2)) ->
        let checkMissingPosKw = checkMissingPosKwLength pos1.kwStart syntax.LArrow 1
        let syntaxErrs =
            checkMissingPos pos1.kwStart "variable name"::
            PVal_projs projs1@
            checkMissingPos pos2.kwStart "variable name"::
            PVal_projs projs2@
            checkMissingPosKw syntax.LArrow "->" ::
            checkMissingPosKw syntax.RArrow "->" ::
            PVal_expr e
        syntaxErrs
      | SkippedStmt(pos) ->
          [checkSkipped pos "Skipped Statement"]
      | SBlock(_,stmts) -> List.foldBack (fun st ss -> (PVal_stmt st)@ss) stmts []
      | SIf(syntax, cond, t, fOpt) ->
        let syntaxErrsIf =
            checkMissingPosSingleKw syntax.kwIf 2 "if"::
            checkMissingPosSingleKw syntax.lPar 1 "("::
            PVal_expr cond@
            checkMissingPosSingleKw syntax.rPar 1 ")"::
            checkMissingPosSingleKw syntax.lBrace 1 "{"::
            PVal_stmt t@
            [checkMissingPosSingleKw syntax.rBrace 1 "}"]
        let syntaxErrsElse =
          match fOpt with
          | Some(syntax, f) ->
            let checkMissingPosKw = checkMissingPosKwLength syntax.kwElse syntax.rBrace 1
            checkMissingPosSingleKw syntax.kwElse 4 "else"::
            checkMissingPosSingleKw syntax.lBrace 1 "{"::
            PVal_stmt f@
            [checkMissingPosSingleKw syntax.rBrace 1 "}"]
          | None -> []
        syntaxErrsIf@syntaxErrsElse
      | SMatch(syntax, exp, cases) ->
        let checkCase (syntax : switchCaseSyntax<_>, t, stm) =
          checkMissingPosSingleKw syntax.kwCase 4 "case"::
          checkMissingPattern t::
          checkMissingPosSingleKw syntax.kwColon 1 ":"::
          PVal_stmt stm
        checkMissingPosSingleKw syntax.kwSwitch 6 "switch"::
        checkMissingPosSingleKw syntax.lPar 1 "("::
        PVal_expr exp@
        checkMissingPosSingleKw syntax.rPar 1 ")"::
        checkMissingPosSingleKw syntax.lBrace 1 "{"::
        List.collect checkCase cases@
        [checkMissingPosSingleKw syntax.rBrace 1 "}"]

    let parseValidateDeclarations (ast : dec<unit,Position> list) : ErrorMessage list =
        let rec run (decs : dec<unit,Position> list) errs : ErrorMessage option list =
            match decs with
            | Import(syntax,_) :: ls -> run ls errs // todo
            | DataImport(syntax,_) :: ls -> run ls errs // todo
            | Contract(syntax,_,_,_,_) :: ls -> run ls errs // todo
            | Export(_) :: ls -> run ls errs // todo
            | FunDec(syntax, (pos,_), pars, expr)::ls ->
                let checkMissingPosKw = checkMissingPosKwLength syntax.kwFun syntax.kwEq 1
                let syntaxErrors =
                    checkMissingPosKw syntax.kwFun kw_fun::
                    checkMissingPosKw pos.kwStart "function name"::
                    checkMissingPosKw syntax.kwLPar "("::
                    (List.foldBack (fun (syn,(pos,_),t) errs ->
                    let synErrs =
                        [
                        checkMissingPosOptional syn.kwComma ","
                        checkMissingPosKw pos.kwStart "parameter name"
                        checkMissingPosKw syn.kwColon ":"
                        ]@(checkMissingPosForType t "type name")
                    let firstErr = getFirstError synErrs
                    firstErr::errs) pars [])@
                    checkMissingPosKw syntax.kwRPar ")"::
                    checkMissingPosKw syntax.kwEq "="::
                    PVal_expr expr

                run ls (errs@(collectErrors syntaxErrors))
            | Data(syntax,dataDec)::ls ->
                let kw_end_pos = getPosAdjust syntax.kwEnd -3
                let checkMissingPosKw = checkMissingPosKwLength kw_end_pos kw_end_pos 3
                let checkExtends =
                  match syntax.kwExtends with
                  | None -> [None]
                  | Some extPos -> let pos = getPosAdjust extPos 1
                                   in [checkMissingPosKwLength pos pos 6 pos "extends"]
                let checkDataDec =
                  List.collect
                    (fun ((_, (sA:constantSyntax<Position>, n), t), ouput) ->
                      (if not <| Parser.csProvider.IsValidIdentifier n
                      then generateError sA.kwStart "Cannot use keywords reserved in C#" n.Length ""
                      else None)::
                      checkMissingPosForType t "type declaration" // changed to PosForType for better error message
                    ) dataDec.fields
                let dataName = snd dataDec.name
                let dataErr =
                  if Parser.csProvider.IsValidIdentifier dataName then []
                  else [generateError ((fst (dataDec.name)).kwStart) "Cannot use keywords reserved in C#" dataName.Length ""]
                // check colonPos for each dec
                let checkColon =
                  List.collect
                    // PROBLEM: output information and colonSyntax are recorded in separate list items - this gives us double error message
                    (fun (((c : colonSyntax<Position>), t_sA, t), output) ->
                      // new error message here - the error message from checkMissingPos "Expected :" is maybe a little unspecific?
                      // if you prefer the consistent use of `checkMissingPos`, just outcomment the following line:
                      // [checkMissingPos c.kwColon ":"]
                      match c.kwColon with
                      | Missing(p)  -> [generateError c.kwColon "Colon missing after field name in data declaration" 3 ""]
                      | FullPos(p1,p2) -> [None]
                      | NoPos -> [None]
                    ) dataDec.fields
                let checkOutput =
                  List.collect
                    (fun ((_, (sA:constantSyntax<Position>, n), t), ouput) ->
                      match ouput with
                      | None -> [None]
                      | Some (_,_,outputPos,_) -> let pos = getPosAdjust outputPos 1
                                                  in [checkMissingPosKwLength pos pos 6 pos "Output as"]
                    ) dataDec.fields
                // check colonPos for each dec
                let checkColon =
                  List.collect
                    // PROBLEM: only shows error if there are no "output as" error
                    (fun (((c : colonSyntax<Position>), t_sA, t), output) ->
                      (if dataName = snd t_sA
                      then generateError (fst t_sA).kwStart "Field names cannot be the same as their enclosing data declaration." dataName.Length ""
                      else None)::
                      match c.kwColon, (getFirstError checkOutput) with
                      | Missing(p), None  -> [generateError c.kwColon "Colon missing after field name in data declaration" 3 ""]
                      | Missing(p), Some e  -> [None]
                      | FullPos(p1,p2), _ -> [None]
                      | NoPos, _ -> [None]
                    ) dataDec.fields
                let syntaxErrs =
                     [ checkMissingPosKw syntax.kwEnd kw_end
                     ]
                let extPos = match syntax.kwExtends with
                             | None -> NoPos
                             | Some a -> a
                let checkMissingName = match dataDec.extends with
                                       | Some "" -> [generateError extPos "Missing name after keyword 'extends'" (String.length kw_extends) ""]
                                       | None -> [None]
                                       | Some a -> [None]
                run ls (errs@dataErr@checkColon@checkMissingName@checkDataDec@checkOutput@collectErrors syntaxErrs)
            | SkippedDec(pos, str)::ls ->
                // Only create error for first skippedDec, then skip the subsequent SkippedDec to avoid multiple errors
                run (List.skipWhile (fun d ->
                    match d with
                    | SkippedDec _ -> true
                    | _ -> false
                ) ls) ((checkSkipped pos str)::errs)
            | ActDec(syntax, (pos,_), pars, stmt)::ls ->
              //let checkMissingPosKw = checkMissingPosKwLength syntax.kwAct syntax.kwEnd 3
              let kw_end_pos = getPosAdjust syntax.kwEnd -3
              let syntaxErrors =
                  checkMissingPosKwLength syntax.kwAct syntax.kwAct 2 syntax.kwAct kw_action::
                  checkMissingPosKwLength pos.kwStart pos.kwStart 2 pos.kwStart "action name"::
                  checkMissingPosKwLength syntax.kwLPar syntax.kwLPar 1 syntax.kwLPar "("::
                  (List.foldBack (fun (syn,(pos,_),t) errs ->
                  let synErrs =
                      [
                      checkMissingPosOptional syn.kwComma ","
                      checkMissingPosKwLength pos.kwStart pos.kwStart 2 pos.kwStart "parameter name"
                      checkMissingPosKwLength syn.kwColon syn.kwColon 1 syn.kwColon ":"
                      ]@(checkMissingPosForType t "type name")
                  let firstErr = getFirstError synErrs
                  firstErr::errs) pars [])@
                  checkMissingPosKwLength syntax.kwRPar syntax.kwRPar 1 syntax.kwRPar ")"::
                  checkMissingPosKwLength syntax.kwWith syntax.kwWith 4 syntax.kwWith "with"::
                  checkMissingPosKwLength kw_end_pos syntax.kwEnd 3 syntax.kwEnd "end"::
                  PVal_stmt stmt

              run ls (errs@(collectErrors syntaxErrors))

            | EndOfFile(_)::ls -> run ls errs
            | [] -> errs
        List.choose id <| (run ast [])

    let parseValidateModule ((modul , decs) as _mod : modul<unit,Position>) : ErrorMessage list =
      let modulDefErrors =
        match modul with
        | MainModul -> []
        | ImportModul(syntax, (ident, nm)) ->
          List.choose id [checkMissingPosKwLength syntax.kwModule syntax.kwModule 2 syntax.kwModule kw_module]
      let decsErrors = parseValidateDeclarations decs
      modulDefErrors @ decsErrors

    /// <summary>
    /// Validates that the ast does not contain any parsing errors.
    /// Either in the form of missing or skipped AST-notes
    /// The returned list of errors contains all errors
    /// The general idea is to first bind `checkMissingPosKw` to mark the entire block
    /// when we discover a missing keyword.
    /// Invariant:
    ///   If parseValidate returns an empty list then.
    ///   ´program´ contains no missing positions or missing nodes
    ///   It was possible to locate all imported modules of `program` and these do not contain missing positions or missing nodes.
    /// </summary>

    let rec parseValidateProgram (program : program<unit,Position>) : ErrorMessage list =
      let modulErrs = parseValidateModule program.modul
      let depsErr =
        List.collect (fun (_,_,dep) -> parseValidateProgram dep) program.dependencies

      let missingModules =
        List.choose
          ( fun dec ->
            match dec with
            | Import (_,(p,nm)) ->
              if List.exists (fun (_,(_,nm'),_) -> nm = nm') program.dependencies
              then None
              else
                Some
                  { message = ErrorMsgs.import_not_found nm
                  ; index = getPos p.kwStart
                  ; length = p.str.Length
                  ; origin = errorOrigin
                  ; prettyOrigin = prettyErrorOrigin
                  ; severity = Severity.Error
                  ; quickfixMsg = ""
                  }
            | _ -> None
          ) (snd program.modul)
      missingModules @ modulErrs @ depsErr
