namespace itu.dk.MAL

open ClosureAnalysis

module Typer =
  open AST
  open Structure
  open ASTUtil
  open Parser
  open Monads
  open Constants
  open Printer
  open PositionBounds
  open AnalyserTypes

  let addUnion un =
    if List.length un > 1
    then
      AnnoResult.addF (fun (tags,errs) -> (Set.add (List.sort un) tags, errs))
    else AnnoResult (fun annos -> ((), annos))


  let addErr err = AnnoResult.addF (fun (tags,errs) -> (tags, err :: errs))
  let addErrIfNot err b = AnnoResult.addAnnoWhenFalse (fun (tags,errs) -> (tags, err :: errs)) b
  let lookupAnno env x def err =
    match lookupAnno env x with
    | Some t -> annoResult { return t }
    | None -> annoResult {
      do! addErr err
      return def
    }

  type tag_union = string list
  type typeAnno = Set<tag_union> * typErr list

  let checkForTagUnion t =
    annoResult {
      match t with
      | TRec(_, tags) -> do! addUnion tags
                         return ()
      | _ -> return ()
    }
  let binOpResultType =
    function
    | Plus
    | Minus
    | Mult
    | Div
    | Mod -> nos_TDouble
    | Eq | LT | LTE | GT | GTE
    | LOR | LAND -> nos_TBool
  let binOpTypeInfo op =
    match op with
    | Plus ->
      ParametricFun (fun typs ->
      match typs with
      | [TStr(_) ; TStr(_)] -> Result.Ok nos_TStr
      | [TDouble(_) ; TDouble(_)] -> Result.Ok nos_TDouble
      | [typ1; typ2] -> Result.Error <| ErrorMsgs.err_badargs_binop "+" (printTyp typ1) (printTyp typ2)
      | _ -> Result.Error <| sprintf "+ expects a (Double, Double) or (String, String) got %A" typs
      )
    | Minus | Div | Mult | Mod -> BIFun ([nos_TDouble; nos_TDouble], nos_TDouble)
    | LT | LTE | GT | GTE -> BIFun ([nos_TDouble; nos_TDouble], nos_TBool)
    | LOR | LAND -> BIFun ([nos_TBool; nos_TBool], nos_TBool)
    | Eq -> ParametricFun (fun typs ->
      match typs with
      | [typ1;typ2] ->
        if typ1 = typ2
        then Result.Ok nos_TBool
        else Result.Error <| ErrorMsgs.err_badargs_binop "==" (printTyp typ1) (printTyp typ2)
      | _ -> Result.Error <| sprintf "= expects a (Any1, Any1) got %A" typs
    )

  let tagToFields (ta : tagMap<'sA>) (tag : pureTag) annos =
    annoResult {
      match Map.tryFind tag ta with
      | Some v -> return v
      | None -> do! AnnoResult.add annos
                return Map.empty
    }

  let verifyTagMatch typ exp ds : AnnotatedResult<typeAnno, expectedPattern<'a> list>=
    match typ with
    | TRec (_, tags) ->
      annoResult {
        let! patternTags =
          match tags with
          | tag::[] ->
            annoResult {
              if not <| Map.containsKey tag ds.subTypes
              then
                do! addErr (ErrorMsgs.warning_match_useless tag (leftPos exp) (rightPos exp))
                return []
              else
                return ds.subTypes.[tag]
            }
          | tags -> annoResult { return tags }
        return List.map ExpTag  patternTags
      }
    | TGeneric(_,GenericOption,[_,typ]) ->
      annoResult {
        return [ExpSome typ; ExpNone]
      }
    | _ ->
      annoResult {
        do! addErr <| ErrorMsgs.cannot_match (Printer.printTyp typ) (leftPos  exp) (rightPos exp)
        return []
      }

  let annotateCases typer env expr expectedPatterns cases =
    foldMA (
      fun (cases, expPats) (sA_case,matchPattern, case) ->
          annoResult {
            match GeneralUtil.findAndRemove expPats (comparePatterns matchPattern) with
            | None, _   ->
              do! addErr <| ErrorMsgs.wrong_match_branches (printPattern matchPattern) (leftPos expr) (rightPos expr) //todo improve error
              let! case' = typer env case
              return (sA_case,matchPattern, case')::cases, expPats
            | Some exPat, expPats' ->
              let env_case =
                match exPat, matchPattern with
                | ExpTag tag, PatTag (_,(_,name))    -> extEnv env name (TRec (noSyntaxAnno,[tag]))
                | ExpSome typ , PatSome (_,(_,name)) -> extEnv env name typ
                | ExpNone _ , PatNone _ -> env
                | _ , _ -> failwith "Impossible"
              let! case' = typer env_case case
              return (sA_case,matchPattern, case')::cases, expPats'
          }
        ) ([], expectedPatterns) cases

  /// <summary>
  /// ´deref´ handles implicit conversions taking place in expression projections.
  /// Concretely, a ´reserve´ behaves like a ´float´ in expressions
  /// </summary>
  let deref (t : typ<'sA>) =
    match t with
    | TReserve (sA) -> TDouble (sA)
    | _ -> t

  let rec typExpr
      (tagMap : tagMap<Position>)
      (ds : dataStructure<Position>)
      (env: environment<typ<Position>>)
      (e : expr<'aE, Position>)
      : AnnotatedResult<typeAnno, expr<typ<Position>, Position>> =
    let errBounds () = exprBounds e
    match e with
    | EMissing  (_, aS)       -> annoResult { return EMissing(TErr, aS) }
    | CDouble   (_, aS, i)    -> annoResult { return CDouble(TDouble noSyntaxAnno, aS, i)}
    | CBool     (_, aS, b)    -> annoResult { return CBool(TBool noSyntaxAnno, aS, b) }
    | CStr      (_, aS, str)  -> annoResult { return CStr(TStr noSyntaxAnno,aS,str) }
    | CEnum     (_,aS,s)       ->
      let enumName =
        if lumpedState_Keywords |> List.contains s
        then "LumpedState"
        else
          if lumpedStateSurrender_Keywords |> List.contains s
          then "LumpedStateWithSurrender"
          else
            if state_Keywords |> List.contains s
            then "State"
            else
              if freePolicy_Keywords |> List.contains s
              then "FreePolicyRule"
              else failwith "Impossible"
      annoResult { return CEnum(TEnum (noSyntaxAnno, enumName), aS,s) }
    | Pair      (_,sA, e1, e2) ->
      annoResult {
        let! e1' = typExpr tagMap ds env e1
        let! e2' = typExpr tagMap ds env e2
        let t1 = eAnnoOf e1'
        let t2 = eAnnoOf e2'
        return Pair(nos_TPair t1 t2, sA, e1', e2')
      }
    | ENone     (_, aS, t)    -> annoResult { return ENone(nos_TOption t, aS, t) }
    | ESome     (_, aS, e')    ->
      annoResult {
        let! e' = typExpr tagMap ds env e'
        let t = eAnnoOf e'
        return ESome(nos_TOption t, aS, e')
      }
    | CreateRec (_, aS, (tagPos, tag), fields) ->
      annoResult {
        let! fields' =
          mapMA
            (fun ((sA_n, n), e) ->
              (annoResult
                {
                let! e' = typExpr tagMap ds env e
                return (eAnnoOf e', ((sA_n, n), e'))
                })
            ) fields
        let! expectedFields, isError =
          if Map.containsKey tag tagMap
          then
            annoResult {return tagMap.[tag], false }
          else
            annoResult {
              do! addErr <| ErrorMsgs.unkown_data_definition tag tagPos.kwStart tagPos.kwEnd
              return Map.empty, true
            }
        do! iterMA (fun (nm, t) ->
              match List.tryFind (fun (_, ((nm',_),_)) -> nm = nm'.str) fields' with
              | None ->
                (annoResult
                {
                  do! addErr <| ErrorMsgs.create_record_missing_field (tag, nm, (Printer.printTyp t)) tagPos.kwStart tagPos.kwEnd
                  return ()
                })
              | Some (t',((nm',_),e)) ->
                (annoResult
                {
                  if not <| isSubOfTyp ds.superTypes t nos_TReserve
                  then
                    let ePos = exprBounds e
                    do! addErrIfNot
                          (ErrorMsgs.must_have_field (nm, Printer.printTyp t, Printer.printTyp t') nm'.kwStart (rightPos e))
                          (isSubOfTyp ds.superTypes t' t)
                    return ()
                  else
                    return ()
                })
            ) <| Map.toList expectedFields
        let extraFields = List.filter (fun (_,((_,nm'),_)) ->  not<| Map.containsKey nm' expectedFields) fields'
        do!
          iterMA
            (fun (_,((sA_nm,nm),e)) ->
              (annoResult
                {
                  let ePos = exprBounds e
                  do! addErr <| ErrorMsgs.data_does_not_contain (tag, nm) sA_nm.kwStart (rightPos e)
                })
            ) extraFields
        let retType = if isError then TErr else TRec (noSyntaxAnno, [tag])
        return CreateRec (retType, aS, (tagPos, tag), (List.map snd fields'))
      }
    | Var       (_, aS, (xPos, x)) ->
      annoResult {
        let! t = lookupAnno env x TErr (ErrorMsgs.var_is_undefined x xPos.kwStart xPos.kwEnd)
        return Var(t, aS, (xPos, x))
      }
    | Proj      (_, aS, e1, (xPos, x)) ->
      annoResult {
        let! e1_t = typExpr tagMap ds env e1
        let t1 = eAnnoOf e1_t
        let! resTyp = typProj tagMap ds env (t1, x, (xPos.kwStart, xPos.kwEnd))
        let resTyp = deref resTyp
        return Proj(resTyp, aS, e1_t, (xPos, x))
      }
    | Filter(_,sA, e1, tags) ->
      annoResult {
        let! e1_t = typExpr tagMap ds env e1
        let t1 = eAnnoOf(e1_t)
        let pureTags = List.map snd tags
        do! addUnion <| List.map (fun (_,t) -> t) tags
        // Could distinguish between record and non-record errors
        let! typ =
          annoResult {
            if List.forall (isTag tagMap) pureTags
            then
              let tagAsListType = nos_TList (nos_TRec pureTags)
              do!
                addErrIfNot
                  (ErrorMsgs.cant_filter_with (Printer.printTyp t1, pureTags) sA.kwColon (rightPos e))
                  (isSubOfTyp ds.superTypes tagAsListType t1)
              return tagAsListType
            else
              do! addErr <| ErrorMsgs.cant_filter_with (Printer.printTyp t1, pureTags) sA.kwColon (rightPos e)
              return TErr
          }
        return Filter(typ, sA, e1_t, tags)
      }
    | BinOp (_, sA, op, e1, e2) ->
      annoResult {
        let! (e1_t) = typExpr tagMap ds env e1
        let t1 = eAnnoOf(e1_t)
        let! (e2_t) = typExpr tagMap ds env e2
        let t2 = eAnnoOf(e2_t)
        let! (t1_ex,t2_ex, t_ret) =
          annoResult {
            match binOpTypeInfo op with
            | BIFun ([t1_ex;t2_ex], t_ret) -> return (t1_ex,t2_ex, t_ret)
            | ParametricFun (tf) ->
              match tf [t1; t2] with
              | Result.Ok res -> return t1, t2, res
              | Result.Error err ->
                do! addErr <| (BadArgs err, errBounds ())
                return t1, t2, binOpResultType op
            | _ -> return t1, t2, binOpResultType op
          }
        let binOp = Printer.printBinOp op
        do!
          addErrIfNot
            (ErrorMsgs.operator_not_compatible (binOp, Printer.printTyp t1, Printer.printTyp t2) (leftPos e1) (rightPos e2))
            (t1 = t1_ex && t2 = t2_ex)
        return BinOp (t_ret, sA, op, e1_t, e2_t)
      }
    | Let(_, sA, bindings, e_body) ->
      annoResult {
        let! (env', bindings_t) =
          foldMA (
            fun (env',bindings_t) lb ->
              annoResult {
                let! (env',newBinding_t) = typLetBinding tagMap ds env' lb
                return (env',newBinding_t::bindings_t)
              }
          ) (env,[]) bindings
        let! e_body_t = typExpr tagMap ds env' e_body
        let t_body = eAnnoOf e_body_t
        return Let(t_body, sA, List.rev bindings_t, e_body_t)
      }
    | If (_,sA, e1, e2, e3) ->
      annoResult {
        let! e1_t = typExpr tagMap ds env e1
        let t1 = eAnnoOf e1_t
        do!
          addErrIfNot
            (ErrorMsgs.if_expr_must_be_boolean (Printer.printTyp t1) (leftPos e1) (rightPos e1))
            (t1 = (TBool noSyntaxAnno))
        let! e2_t  = typExpr tagMap ds env e2
        let t2 = eAnnoOf e2_t
        let! e3_t  = typExpr tagMap ds env e3
        let t3 = eAnnoOf e3_t
        do!
          addErrIfNot
            (ErrorMsgs.if_expr_same_branches (Printer.printTyp t2, Printer.printTyp t3) (leftPos e3) (rightPos e3))
            (t2 = t3)
        return If (t2, sA, e1_t, e2_t, e3_t)
      }
    | Match (_, sA, e1, cases) ->
      annoResult {
        let! e1_t = typExpr tagMap ds env e1
        let t1 = eAnnoOf e1_t
        let! expectedPatterns = verifyTagMatch t1 e1 ds
        let exprTyper = typExpr tagMap ds
        let! cases', unMatched = annotateCases exprTyper env e1 expectedPatterns cases
        let cases' = List.rev cases'
        do! iterMA
              (fun pat ->
                annoResult{
                  do! addErr (ErrorMsgs.expected_branch_for (sprintf "%A" pat) (leftPos e1) (rightPos e1))
                }) unMatched
        let! typ =
          annoResult {
            if List.length cases' = 0
            then return TErr
            else
                let (_,_,e_case) = (List.head cases')
                let c1_t = eAnnoOf e_case
                do!
                  iterMA
                    (fun (_,_, e_case') ->
                      let c_t = eAnnoOf e_case'
                      addErrIfNot
                        (ErrorMsgs.different_branches (Printer.printTyp c1_t, Printer.printTyp c_t) (leftPos e1) (rightPos e1))
                        (c_t = c1_t)
                    ) (List.tail cases')
                return c1_t
          }
        return Match (typ, sA, e1_t, cases')
      }
    | Map(_,sA, forIns, e_filter, e_body) ->
      annoResult {
        let rec buildArgs accEnv forIns forIns_t =
          match forIns with
          | [] ->
            annoResult {
              let! e_filter_t =
                annoResult {
                  match e_filter with
                  | Some filter -> let! e_filter_t = typWhere tagMap ds accEnv filter
                                   return Some e_filter_t
                  | None -> return None
                }
              let! e_body_t = typExpr tagMap ds accEnv e_body
              let t_body = eAnnoOf e_body_t
              return (nos_TList t_body, forIns_t, e_filter_t, e_body_t)
            }
          | forIn::forIns' ->
            annoResult {
              let! (accEnv', forIn_t) = typForIn tagMap ds accEnv forIn false sA.loopName
              return! buildArgs accEnv' forIns' <| forIn_t::forIns_t
            }
        let! (t, forIns_t, e_filter_t, e_body_t) = buildArgs env forIns []
        return Map(t, sA, List.rev forIns_t, e_filter_t, e_body_t)
      }
    | List (_, sA, es,typAnno) ->
      annoResult {
        let! elems =
          mapMA (
            fun (sA, eArg) ->
              annoResult {
                let! eArg_t = typExpr tagMap ds env eArg
                let t_arg = eAnnoOf eArg_t
                return (sA, t_arg, eArg_t)
              }) es
        let t_elems = List.map (fun (_,typ,_) -> typ) <| elems
        let elems_t_sA = List.map (fun (sA,_,e_t) -> (sA,e_t)) elems
        let list_typ =
          match t_elems with
          | [] -> typAnno
          | t1::ts ->
            List.fold (fun commonTyp t -> Option.bind (tryGreatestComTyp ds.superTypes true t) commonTyp) (Some t1) ts
              |> Option.map (fun t -> nos_TList t)
        let expTyp =
          match typAnno with
          | Some t -> t
          | None -> TErr
        let headTyp =
          match t_elems with
          | thead::[] -> Printer.printTyp thead
          | _ -> ""

        do!
          (addErrIfNot
            (ErrorMsgs.list_must_be_of_type (printTyp expTyp) (leftPos e) (rightPos e))
            (match list_typ with
            | None -> true
            | Some s -> isSubOfTyp ds.superTypes s expTyp)
          )

        do!
          (addErrIfNot
          (ErrorMsgs.elements_must_be_the_same headTyp (leftPos e) (rightPos e))
          (Option.isSome list_typ))

        return List(Option.defaultValue TErr list_typ, sA, elems_t_sA, typAnno)
      }
    | FCall(_, sA, e1, e_args) ->
      annoResult {
        let! e1_t = typExpr tagMap ds env e1
        let t1 = eAnnoOf e1_t
        let! args =
          mapMA (
            fun (sA, eArg) ->
              annoResult {
                let! eArg_t = typExpr tagMap ds env eArg
                let t_arg = eAnnoOf eArg_t
                return (sA, t_arg, eArg_t)
              }) e_args
        let t_args = List.map (fun (_,typ,_) -> typ) args
        let e_args_t_sA = List.map (fun (sA,_,e_t) -> (sA,e_t)) args
        let! args_expected,result_type,errString =
            annoResult {
              match t1 with
              | TGeneric (_, GenericMap, [_,t1;_,t2])         -> return [t1],t2,Some "Map"
              | TFun (IFun(_))                                -> return [nos_TDouble],nos_TDouble,Some "Function"
              | TFun (BIFun (expectedArgs, res))              -> return expectedArgs,res,Some "Function"
              | TGeneric (_, GenericSysFun , [_, t1 ; _, t2]) -> return [t1],t2,Some "Func"
              | TFun (ParametricFun (typingFun)) ->
                match typingFun t_args with
                | Result.Ok res -> return t_args, res, Some "Function"
                | Result.Error err ->
                    do! addErr (BadArgs err, errBounds ())
                    return t_args, TErr, Some "Function"
              | TErr -> return [t1],TErr,None
              | _ -> do! addErr (ErrorMsgs.not_callable (Printer.printTyp t1) (leftPos e) (rightPos e))
                     return [], TErr, None
            }
        // Todo, figure out why this option
        do!
          match errString with
          | None -> annoResult {return ()}
          | Some errString ->
            addErrIfNot
              (BadArgs <| sprintf "%s takes (%A) got (%A)"
                errString
                (String.concat "," <| List.map Printer.printTyp args_expected)
                (String.concat "," <| List.map Printer.printTyp t_args), errBounds ())
              (List.length t_args = List.length  args_expected &&
                List.forall2 (isSubOfTyp ds.superTypes) t_args args_expected)
        return FCall(result_type, sA, e1_t, e_args_t_sA)
      }
    | EPar(_,sA,e1) ->
      annoResult {
        let! e1_t = typExpr tagMap ds env e1
        let t1 = eAnnoOf e1_t
        return EPar(t1, sA, e1_t)
      }
  and typLetBinding (ta : tagMap<Position>) (ds : dataStructure<Position>) (env: environment<typ<Position>>) lb
    : AnnotatedResult<typeAnno, environment<typ<Position>> * letBinding<typ<Position>,Position>> =
    annoResult {
      let (sA, (xPos, x), e1) = lb
      let! e1_t = typExpr ta ds env e1
      let t1 = eAnnoOf e1_t
      return (extEnv env x t1, letBinding (sA, (xPos, x), e1_t))
    }
  and typForIn (ta : tagMap<Position>)
               (ds : dataStructure<Position>)
               (env: environment<typ<Position>>)
               (fI : forIn<'aE, Position>)
               (mayMapOnOption : bool)
               loopName
    : AnnotatedResult<typeAnno, environment<typ<Position>> * forIn<typ<Position>,Position>> =
      let  (sA,args) = fI
      //let! args_t =
      let reg many =
        annoResult {
        let! args_t =
          mapMA (fun (x, e) ->
            annoResult {
              let! e_t = typExpr ta ds env e
              let t1 = eAnnoOf e_t
              let! t_elem =
                annoResult {
                  match t1 with
                  | TGeneric (_, GenericList, [_, t_elem] )
                  | TGeneric (_, GenericOption, [_, t_elem] ) -> return t_elem
                  | _ -> do! addErr (ErrorMsgs.not_enumerable (loopName, Printer.printTyp t1) (leftPos e_t) (rightPos e_t))
                         return TErr
                }
              return (t_elem, x, e_t)
            }) many
        let env = List.fold (fun env (t_elem, (_,x),e) -> extEnv env x t_elem) env args_t
        let args_t = List.map (fun (_,b,c) -> (b,c)) args_t
        return env, forIn (sA, Choice1Of2 args_t)
        }
      let kvp (((_,(_,k),(_,v)) as kv), e) =
        annoResult {
          let! e_t = typExpr ta ds env e
          let t1 = eAnnoOf e_t
          let! t_key,t_val =
            annoResult {
              match t1 with
              | TGeneric(_, GenericMap, [_,t_k ; _ , t_v]) -> return t_k,t_v
              | _ -> do! addErr (ErrorMsgs.not_enumerable (loopName, Printer.printTyp t1) (leftPos e_t) (rightPos e_t))
                     return TErr,TErr
            }
          let env = extEnv (extEnv env k t_key) v t_val
          return env, forIn (sA, Choice2Of2 (kv, e_t))
        }
      Choices.joinChoice reg kvp args

  and typWhere (ta : tagMap<Position>) (ds : dataStructure<Position>) (env: environment<typ<Position>>) (w : where<'aE, Position>)
      : AnnotatedResult<typeAnno, where<typ<Position>,Position>> =
    annoResult {
      let (sA, e1) = w
      let! e1_t = typExpr ta ds env e1
      let t1 = eAnnoOf e1_t
      do!
        addErrIfNot
          (ErrorMsgs.where_must_be_bool (Printer.printTyp t1) (leftPos e1) (rightPos e1))
          (t1 = TBool noSyntaxAnno)
      return (where (sA, e1_t))
    }
  and typProj (ta : tagMap<Position>) (ds : dataStructure<Position>) (env: environment<typ<Position>>)
    (t_1, x, (err_left, err_right) : Position * Position)
    : AnnotatedResult<typeAnno, typ<Position>> =
    annoResult {
      match t_1 with
      | TRec (_, tags) ->
        let! typs =
          mapMA
            (fun tag ->
              annoResult {
                let recInfo =
                  match Map.tryFind tag ta with
                  | None -> Map.empty // This is an error, but an error-message should be generated by dataStructureAnalysis
                  | Some v -> v
                match Map.tryFind x recInfo with
                | Some t_l -> return t_l
                | None ->
                  do! addErr (ErrorMsgs.data_does_not_contain (tag, x) err_left err_right)
                  return TErr
              }) tags
        return List.reduce greatestCommonTyp typs // todo: Examine gcp seems wrong
      | TModule (nm, mp) ->
        match Map.tryFind x mp with
        | None ->
          do! addErr (ErrorMsgs.module_does_not_contain (nm, x) err_left err_right)
          return TErr
        | Some t ->
          return t
      | TErr (_) -> // Fails silently
        return TErr
      | _ ->
        do! addErr (ErrorMsgs.expected_module_or_record (Printer.printTyp t_1) err_left err_right)
        return TErr
    }

  let typeAssignProj (ta : tagMap<Position>) (ds : dataStructure<Position>) (env : environment<typ<Position>>)
                     ((_,(xPos,x)), labels) = //labels er projections
    annoResult {
      let! assObj_t = typExpr ta ds env (Var ((), xPos, ( xPos, x)))
      let t_assObj = eAnnoOf assObj_t
      let! t_new, labels_t =
        foldMA
          (fun (typ,typProjs) (_, projSyntax, (labelPos, label)) ->
            annoResult {
              let! res = typProj ta ds env (typ, label, (xPos.kwStart, xPos.kwEnd))
              let projInfo = (res, projSyntax, (labelPos, label))
              return (res, projInfo::typProjs)
            }
          )
          (t_assObj,[]) labels
      return t_new, ((t_assObj, (xPos,x)), List.rev labels_t)
    }

  let rec typStmt (ta : tagMap<Position>) (ds : dataStructure<Position>) (env : environment<typ<Position>>) level (stmt : stmt<'a, Position>)
    : AnnotatedResult<typeAnno, environment<typ<Position>> * stmt<typ<Position>,Position>> =
    match stmt with
    | SkippedStmt aS ->
      annoResult {
        return (env, (SkippedStmt aS))
      }
    | SUpdate (sA, forIn, e_filter, stmt) ->
      annoResult {
        let! (env', forIn_t) = typForIn ta ds env forIn true sA.loopName
        let! e_filter_t =
          annoResult {
          match e_filter with
            | None -> return None
            | Some filter -> let! where_t = typWhere ta ds env' filter
                             return Some where_t
          }
        let! _,stmt_t = typStmt ta ds env' level stmt
        return (env, SUpdate (sA, forIn_t, e_filter_t, stmt_t))
      }
    | SAss(sA, assProj, expr) ->
      annoResult {
        let! expr_t = typExpr ta ds env expr
        let t_new = eAnnoOf expr_t
        let! t_ass, projs = typeAssignProj ta ds env assProj
        do!
          addErrIfNot
            (ErrorMsgs.cannot_assign_to (Printer.printTyp t_ass, Printer.printTyp t_new) (leftPos expr) (rightPos expr))
            (t_ass = t_new)

        do!
          addErrIfNot
            (ErrorMsgs.cannot_assign_to_a_reserve () (leftPos expr) (rightPos expr))
            (t_ass <> nos_TReserve)

        do!
          addErrIfNot
            (ErrorMsgs.cannot_assign_to (Printer.printTyp t_ass, "any") (leftPos expr) (rightPos expr))
            (List.length (snd projs) > 0 || match t_ass with TFun _ | TAct _ -> false | _ -> true)

        return env, SAss(sA, projs, expr_t)
      }
    | SLet(lebinding) ->
      annoResult {
        let! (env, lb_t) = typLetBinding ta ds env lebinding
        return env, SLet(lb_t)
      }
    | SDoCSharp(sA, args) ->
      annoResult {
        return env, SDoCSharp(sA, args) // Todo, figure out how to fix
      }
    | SDo(sA, ((_,(p,act_nm)), locations), args) ->
      annoResult {
        let! act_nmTyp = lookupAnno env act_nm TErr
                          (ErrorMsgs.action_or_module_undefined act_nm p.kwStart p.kwEnd)
        let! act_typ, locations =
          foldMA
            ( fun (curTyp,acc_loc) (_,pA,(p, x)) ->
              annoResult {
                let! projTyp =
                  annoResult {
                    match curTyp with
                    | TModule (_,mp) ->
                        match Map.tryFind x mp with
                        | Some t -> return t
                        | None ->
                          do! addErr <| ErrorMsgs.the_module_does_not_export x p.kwStart p.kwEnd
                          return TErr
                    | TErr -> return TErr
                    | _ ->
                      do! addErr <| ErrorMsgs.X_does_not_contain (printTyp curTyp, x) p.kwStart p.kwEnd
                      return TErr
                  }
                return (projTyp,(projTyp, pA, (p,x)) :: acc_loc)
              }
            ) (act_nmTyp, []) locations

        let locations = List.rev locations

        let act_t = (act_nmTyp, (p, act_nm)), locations

        let! args' =
          mapMA (fun (sa, e) ->
            annoResult {
              let! e' = typExpr ta ds env e
              return (sa, e')
            }
            ) args
        let args_t = List.map (eAnnoOf << snd) args'
        match act_typ with
        | TAct(args_exp) ->
          do!
            addErrIfNot
              (ErrorMsgs.action_expected (String.concat "," <| List.map Printer.printTyp args_exp,
                                          String.concat "," <| List.map Printer.printTyp args_t)
                                          sA.kwLPar sA.kwRPar)
              (List.length args' = List.length args_exp &&
                List.forall2 (isSubOfTyp ds.superTypes) args_t args_exp)
          return env, SDo(sA, act_t, args')
        | TFun(_) ->
          do! addErr (ErrorMsgs.only_do_action () p.kwStart p.kwEnd)
          return env, SDo(sA, act_t, args')
        | _ ->
          do! addErr (ErrorMsgs.only_do_action () p.kwStart p.kwEnd)
          return env, SDo(sA, act_t, args')
      }
    | SOverwrite(sA, assProjs, e1) ->
      annoResult {
        let! expr_t = typExpr ta ds env e1
        let t_new = eAnnoOf expr_t
        let! t_ass, projs = typeAssignProj ta ds env assProjs
        do!
          addErrIfNot
            (ErrorMsgs.can_only_overwrite_with (Printer.printTyp t_new) (leftPos e1) (rightPos e1))
            (t_new = nos_TReserve || t_new = nos_TDouble)
        do!
          addErrIfNot
            (ErrorMsgs.can_only_overwrite_a (Printer.printTyp t_new) (leftPos e1) (rightPos e1))
            (t_ass = nos_TReserve)
        return env, SOverwrite(sA, projs, expr_t)
      }
    | STransfer(sA, assProjs1, expr, assProjs2) ->
      annoResult {
        let! expr_t = typExpr ta ds env expr
        let t_new = eAnnoOf expr_t
        let! t_ass1, projs1 = typeAssignProj ta ds env assProjs1
        let! t_ass2, projs2 = typeAssignProj ta ds env assProjs2
        do!
          addErrIfNot
            (ErrorMsgs.can_only_transfer_float (Printer.printTyp t_new) (leftPos expr) (rightPos expr))
            (t_new = nos_TDouble)
        do!
          addErrIfNot
            (ErrorMsgs.can_only_transfer_from_r (Printer.printTyp t_ass1) (leftPos expr) (rightPos expr))
            (t_ass1 = nos_TReserve)
        do! addErrIfNot
              (ErrorMsgs.can_only_transfer_to_r (Printer.printTyp t_ass2) (leftPos expr) (rightPos expr))
              (t_ass1 = nos_TReserve)
        return env, STransfer(sA, projs1, expr_t, projs2)
      }
    | SBlock (sA,stmts) ->
      annoResult
        {
          let! stmts_t = typStmts ta ds env level stmts
          return env, SBlock (sA,stmts_t)
        }
    | SIf(sA, expr, t, fOpt) ->
      annoResult {
        let! expr_t = typExpr ta ds env expr
        let t_new = eAnnoOf expr_t
        let! _,t_t = typStmt ta ds env level t
        let! _,fOpt_t =
          match fOpt with
          | Some(fA, f) -> annoResult { let! env,f_t = typStmt ta ds env level f in return env, Some (fA, f_t) }
          | None -> annoResult { return env, None }
        do!
          addErrIfNot
            (ErrorMsgs.if_expr_must_be_boolean (Printer.printTyp t_new) (leftPos expr) (rightPos expr))
            (t_new = nos_TBool)
        return env, SIf(sA, expr_t, t_t, fOpt_t)
      }
    | SMatch(sA, expr, cases) ->
      annoResult {
        let! expr_t = typExpr ta ds env expr
        let t_new = eAnnoOf expr_t
        let! expectedPatterns = verifyTagMatch t_new expr ds
        let stmtTyper env s = annoResult { let! _,s' = typStmt ta ds env level s in return s' }
        let! cases', unMatched = annotateCases stmtTyper env expr expectedPatterns cases
        let cases' = List.rev cases'
        do!
          iterMA
            (fun pat ->
              annoResult{
                do! addErr (ErrorMsgs.expected_branch_for (sprintf "%A" pat) (leftPos expr) (rightPos expr))
              }) unMatched
        return env, SMatch(sA, expr_t, cases')
      }

  and typStmts
        (ta : tagMap<Position>)
        (ds : dataStructure<Position>)
        (env : environment<typ<Position>>)
        level
        (stmts : stmt<'a, Position> list)
    : AnnotatedResult<typeAnno, stmt<typ<Position>,Position> list> =
    annoResult {
      let! (_env', stmts_t) =
          foldMA
            (fun (env',stmts_t) stmt ->
              (annoResult {
                let! (env', stmt_t) = typStmt ta ds env' level stmt
                return (env', stmt_t::stmts_t)
              })) (env,[]) stmts
      return List.rev stmts_t
    }
  let toProperType tagMap t : AnnotatedResult<typeAnno,typ<Position>>=
    annoResult {
        match t with
        | TRec (sA, tags) ->
          if List.forall (isTag tagMap) tags
          then return t
          else
            do! addErr (ErrorMsgs.the_type_is_not_valid (printTagSyntax tags) sA sA) // Todo: improve position
            return TErr
        | _ -> return t
    }

  let typeExtend (tagMap : tagMap<Position>)
                 (env : environment<typ<Position>>)
                 (dataDec : dataDec<'a, Position>)
                 : AnnotatedResult<typeAnno, dataDec<typ<Position>, Position>>
    =
    annoResult {
      let (x_sA, x) = dataDec.name
      let (x_type : typ<Position>) =
        match Map.tryFind x tagMap with
        | Some (_t) -> TRec (x_sA.kwStart, [x])
        | None -> TErr
      let fields = dataDec.fields
      let! fields' =
        mapMA
          (fun arg ->
            annoResult {
              let ((_, (sA,x), t), info) = arg
              let! t' = toProperType tagMap t
              let extInfo = ((t',sA, x), t')
              return (extInfo , info)
            }
          ) fields
      return { fields = fields; name = (x_sA, x) ; extends = dataDec.extends }
    }

  let typeFunActDec env tagMap (ds : dataStructure<Position>) all_decs name =
    let typeFunDec sA f_sA f args body =
      annoResult {
        let! envs =
          mapMA
            (fun (_,(_,x),t) ->
              annoResult {
                do! checkForTagUnion t
                let! t' = toProperType tagMap t
                return ((x,t'),t')
              }
            ) args
        let (smallEnv,argsTyp) = List.unzip envs
        let! body_t = typExpr tagMap ds (appendEnv env smallEnv) body
        let t_body = eAnnoOf body_t
        let env' = extEnv env f (TFun(BIFun(argsTyp, t_body)))
        return env', FunDec (sA, (f_sA, f), args, body_t)
      }
    let typeActDec sA f_sA f args stmt =
      annoResult {
        let! envs =
          mapMA
            (fun (_,(_,x),t) ->
              annoResult {
                do! checkForTagUnion t
                let! t' = toProperType tagMap t
                return ((x,t'),t')
              }
            ) args
        let (smallEnv,argsTyp) = List.unzip envs
        let! (_,stmt_t) = typStmt tagMap ds (appendEnv env smallEnv) 1 stmt
        let env' = extEnv env f (TAct argsTyp)
        return env', ActDec (sA, (f_sA, f), args, stmt_t)
      }

    annoResult {
      match tryFindFunction all_decs name with
      | Some(Export(sA_ex, FunDec (sA, (f_sA, f), args, body))) ->
        let! env', dec = typeFunDec sA f_sA f args body
        return Some (env', Export(sA_ex, dec))
      | Some(FunDec (sA, (f_sA, f), args, body)) ->
        let! env', dec = typeFunDec sA f_sA f args body
        return Some (env', dec)
      | Some(ActDec (sA, (f_sA, f), args, stmt)) ->
        let! env', dec = typeActDec sA f_sA f args stmt
        return Some (env', dec)
      | Some(Export(sA_ex, ActDec (sA, (f_sA, f), args, stmt))) ->
        let! env', dec = typeActDec sA f_sA f args stmt
        return Some (env', Export(sA_ex, dec))
      | _ -> return None
    }

  let typeFunActDecs tagMap (ds : dataStructure<Position>) env decs closures =
    annoResult {
      return!
        foldMA (fun (env', accDecs_t) name ->
          annoResult {
            let! res = typeFunActDec env' tagMap ds decs name
            match res with
            | Some (env', dec_t) -> return (env', dec_t :: accDecs_t)
            | None -> return (env', accDecs_t)
          }
        ) (env,[]) (closures)
    }

  let typeMode (modeName : string)
               closures tagMap (ds : dataStructure<Position>) env (decs : dec<'a, Position> list) =
    let addAnalysis =
      let rec typeDec dec env =
        annoResult
          {
            match dec with
            | ActDec (sA,(sA_nm, nm), args, stmt) when nm = modeName ->
              do! iterMA (fun (_,_,t) -> checkForTagUnion t) args
              let! (env, stmt_t) = typStmt tagMap ds env 0 stmt
              return Some(ActDec(sA,(sA_nm,nm), args, stmt_t))
            | Export(sA_e, dec) ->
              let! dec' = typeDec dec env
              return Option.map (fun d -> Export(sA_e, d)) dec'

            | Data (sA,dataDec) ->
                  let! dataDec' = typeExtend tagMap env dataDec
                  return Some(Data (sA, dataDec'))
            | EndOfFile (sA) -> return Some(EndOfFile (sA))
            | SkippedDec(sA, str) -> return Some(SkippedDec(sA, str))
            | Import(sA, modul) -> return Some(Import(sA, modul))
            | _ -> return None
          }

      annoResult {
        let! (env, closures_t) = typeFunActDecs tagMap ds env decs closures
        let! blocks_t =
          foldMA (fun decs dec ->
            annoResult {
              let! dec' = typeDec dec env
              match dec' with
              | Some dec' -> return dec' :: decs
              | None -> return decs
            }
          ) [] decs
        return! AnnoResult.ok (blocks_t@closures_t)
      }
    AnnoResult.run (Set.empty,[]) addAnalysis

  let createEnv : ((string * typ<Position>) list) * ((string * typ<Position>) list) =
    let baseEnv =
      [   var_equities,   nos_TList <| nos_TRec [tag_equity]
        ; var_groups,     nos_TList <| nos_TRec [tag_group]
        ; var_policies,   nos_TList <| nos_TRec [tag_policy]
        ; var_cashFlows,  nos_TList <| nos_TRec [tag_cashFlow]
        ; var_global,     nos_TRec [tag_global]
      ]

    let env = BuiltIns.builtInTypes@baseEnv
    (env, (var_projection, nos_TRec [tag_projection])::env)

  let typeDecs
    initEnv
    updateEnv
    (closureResult : closureResult)
    (ds : dataStructure<Position>)
    (decs : dec<'a, Position> list) =

    let rec checkDuplicateFunctions (annos : typErr list) (decs : dec<'a,Position> list) : typErr list=
      match decs with
      | [] -> annos
      | dec::decs' ->
        let annos' =
          match dec with
          | FunDec (sA, (f_sA, f) ,_,_) ->
            match tryFindFunction decs' f with
            | Some (_) -> (DuplicateFunction <| sprintf "The function or action %s is defined twice" f, (posToIndex (f_sA.kwStart, 0) , posToIndex (f_sA.kwEnd, 0)))::annos //Todo bad bounds
            | None -> annos
          | ActDec (_, (f_sA, f),_,_) ->
            match tryFindFunction decs' f with
            | Some (_) -> (DuplicateFunction <| sprintf "The function or action %s is defined twice" f, (posToIndex (f_sA.kwStart, 0) , posToIndex (f_sA.kwEnd, 0)))::annos
            | None -> annos
          | _ -> annos
        checkDuplicateFunctions annos' decs'

    let duplicateAnnos = checkDuplicateFunctions [] decs
    let recursiveAnnos =
      List.map
        (fun f ->
          match tryFindFunction decs f with
          | Some(Export(_,FunDec  (_, (f_sA, f),_,_)))
          | Some(FunDec (_, (f_sA, f),_,_)) -> (RecursiveFunction <| sprintf "The function %s appears in a recursive cycle" f, (posToIndex (f_sA.kwStart, 0) , posToIndex (f_sA.kwEnd, 0))) //Todo bad anno
          | Some(ActDec (_, (f_sA, f),_,_))
          | Some(Export(_,ActDec (_, (f_sA, f),_,_))) -> (RecursiveFunction <| sprintf "The action %s appears in a recursive cycle" f, (posToIndex (f_sA.kwStart, 0) , posToIndex (f_sA.kwEnd, 0)))
          | _ -> failwith "Can not find recursive function, should be impossible") closureResult.recursives

    let (initAst, (initUnions, initAnnos)) =
      typeMode "init"
               closureResult.initFuns
               ds.initTagMap
               ds
               initEnv
               decs

    let (manageAst, (manageUnions, manageAnnos)) =
      typeMode "manage" closureResult.manageFuns ds.manageTagMap ds updateEnv decs

    let (initEnv, _closures_t),_     = AnnoResult.run (Set.empty,[]) <| typeFunActDecs ds.initTagMap   ds initEnv   decs closureResult.unusedFuns
    let (manageEnv, _closures_t),_   = AnnoResult.run (Set.empty,[]) <| typeFunActDecs ds.manageTagMap ds updateEnv decs closureResult.unusedFuns

    let unusedUnions,unusedClosures',unusedErrs =
      List.fold (fun (accUnions,accDec,accErrs) name ->
        let initAttempt                           = typeFunActDec initEnv ds.initTagMap ds decs name
        let initRes,(initUnions,initErrs)         = AnnoResult.run (Set.empty,[]) initAttempt
        let manageAttempt                         = typeFunActDec manageEnv ds.manageTagMap ds decs name
        let manageRes, (manageUnions,manageErrs)  = AnnoResult.run (Set.empty,[]) manageAttempt
        let allUnions = Set.union accUnions <| Set.union initUnions  manageUnions
        let decs, errs =
          match initRes,initErrs,manageRes, manageErrs with
          | _,_,Some (acc, dec_t), []   -> dec_t :: accDec, accErrs
          | Some (acc, dec_t), [], _, _ -> dec_t :: accDec, accErrs
          | _,_,Some (acc, dec_t), errs -> dec_t :: accDec, errs @ accErrs
          | Some (acc, dec_t), errs,_,_ -> dec_t :: accDec, errs @ accErrs
          | _,_,_,_ -> failwith "unexpected"
        allUnions ,decs, errs

        ) (Set.empty,[], []) closureResult.unusedFuns

    let combinedTree =
      mergeTrees decs (mergeTrees decs initAst manageAst) unusedClosures'

    let allUnions = Set.union unusedUnions <| Set.union manageUnions initUnions
    (combinedTree,List.rev (unusedErrs@manageAnnos@initAnnos@duplicateAnnos@recursiveAnnos), allUnions )

  let typeModule initEnv updateEnv (closures : closureResult) (ds : dataStructure<Position>) ((modulDef, decs) as modul) =
    let typedDecs, errs, unionTags = typeDecs (initEnv) (updateEnv) closures ds decs
    (modulDef, typedDecs), errs, unionTags

