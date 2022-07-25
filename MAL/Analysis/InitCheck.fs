namespace itu.dk.MAL


module InitCheck =
  open AST
  open Structure
  open ASTUtil
  open Parser
  open Monads
  open Constants
  open Printer
  open PositionBounds
  open AnalyserTypes

  let addAnnoWhenFalse a = AnnoResult.addAnnoWhenFalse (fun annos -> a::annos )

  type initTypes =
    | TotalCollection of string list
    | TotalRecord of string list
    | Bot

  // these are already initialized
  let globalHasBeen = [ "Policy" , "Input"
                      ; "Policy" , "Result"
                      ; "Policy" , "Name"
                      ; "Policy" , "Param"
                      ; "Policy" , "Groups"
                      ; "Policy" , "Equity"
                      ; "Policy" , "CashFlows"
                      ; "Policy" , "ExpenseGroup"
                      ; "Policy" , "RiskGroups"
                      ; "Policy" , "InterestGroup"
                      ; "ThreeStatePolicy" , "LumpedStates"
                      ; "Group" , "Input"
                      ; "Group" , "Result"
                      ; "Group" , "Param"
                      ; "Group" , "Name"
                      ; "Group" , "Policies"
                      ; "Group" , "Assets"
                      ; "Group" , "Reserve"
                      ; "Equity" , "Input"
                      ; "Equity" , "Result"
                      ; "Equity" , "Param"
                      ; "Equity" , "Name"
                      ; "Equity" , "Policies"
                      ; "Equity" , "Assets"
                      ; "Equity" , "Reserve"
                      ; "Global" , "Input"
                      ; "Global" , "Result"
                      ; "Global" , "Name"
                      ; "Global" , "Param"
                      ; "Global" , "Result"
                      ; "CashFlow" , "Input"
                      ; "CashFlow" , "Param"
                      ; "CashFlow" , "Name"
                      ; "CashFlow" , "Policies"
                      ; "CashFlow" , "Policy"
                      ; "CashFlow" , "Result"
                      ]


  // just for debug printing of maps, you can delete this
  let mapPrint (mp: Map<'a, 'b>) =
    for kvp in mp do
        printfn "[ %A, %A ]\n" kvp.Key kvp.Value

  /// <summary>
  /// Lookup in environment, return initType
  /// Input: environement, key to look up
  /// </summary>
  ///
  let rec EnvLookup env key =
    match env with
      | [] -> None
      | (x,typ)::t when x = key -> Some typ
      | _::t -> EnvLookup t key

  /// <summary>
  /// Lookup in init list, return fieldName if it matches tag key or a supertype supertype of tag key
  /// Input: init, tag key to look up
  /// </summary>
  ///
  let rec InitLookup init (key : string) subTypes =
    match init with
      | [] -> None
      | (x,fieldName)::t when (x = key || ASTUtil.tagIsSubOf subTypes key x) -> Some fieldName
      | _::t -> InitLookup t key subTypes

  // Lookup specific fieldName in init list. Return fieldName if it matches tag key or a supertype of tag key
  // Input: init, tag key to look up, fieldName to look for
  let rec InitLookup2 init (key : string) (fieldName : string) subTypes =
    match init with
      | [] -> None
      | (x,fName)::t when ((fName = fieldName) && (x = key || ASTUtil.tagIsSubOf subTypes key x)) -> Some fieldName
      | _::t -> InitLookup2 t key fieldName subTypes

  /// <summary>
  /// Gamma |- expr -> initType
  /// </summary>
  ///
  let rec ExprToInitType env expr =
    match expr with
      | EMissing  _               -> Bot // done
      | CDouble   _               -> Bot // done
      | CBool     _               -> Bot // done
      | CStr      _               -> Bot // done
      | CEnum     _               -> Bot // done
      | Pair      _               -> Bot // done
      | ENone     _               -> Bot // done
      | ESome     _               -> Bot // done
      | Proj      _               -> Bot // done
      | Let       _               -> Bot // done
      | If        _               -> Bot // done
      | Match     _               -> Bot // done
      | Map       _               -> Bot // done
      | List      _               -> Bot // done
      | FCall     _               -> Bot // done
      | BinOp     _               -> Bot // done
      | CreateRec _               -> Bot // done
      | EPar      _               -> Bot // done
      | Var       (_, _, (_, x))  -> match (EnvLookup env x) with
                                      | Some b -> b
                                      | None -> Bot
      | Filter    (_,_,expr,_)    -> // no need to check subtyping, this happens in type checker
                                     match (ExprToInitType env expr) with
                                     | TotalCollection tag -> TotalCollection tag
                                     | _ -> Bot


  type varBindings = (string * initTypes) list  // Variables bound to values that can be used for initialisation
  type fieldInits = (string * string ) list  // Map from tags to fields initialised
  type moduleInits = string list // List of initialised modules

  type checkEnv =
    {
      varBindings : varBindings
      inits : fieldInits * moduleInits
    }

  /// <summary>
  /// Gamma |- forin -> Gamma'
  /// </summary>
  ///
  let rec CheckForIn forin (env) =
    match forin with
      // map p1 in P, or
      // map p1,p2 in P,P
      // should only be one element!
      | Choice1Of2 (((a,f),expr)::[]) -> match ExprToInitType env expr with
                                         | TotalCollection tag -> let recordTag = f, (TotalRecord tag)
                                                                  recordTag::env
                                         | _ -> env
      | Choice1Of2 _  -> failwith "impossible with `x1,x2 in e1,e2` in update"
      // map (p1,p2) in PMap
      // should also be error?
      | Choice2Of2 ((_,(_,k),(_,v)), elist) -> env

  let rec findProjBottom tagList projs =
    match projs with
    | [(_,_,(_,proj))] -> Seq.map (fun tag -> (tag, proj)) tagList
    | proj :: projs -> Seq.empty // We only consider immediate (1-proj) initialisation
    | [] -> failwith "definatly unexpected"

  let assproj env init projs x =
    match (EnvLookup env x) with
    | Some (TotalRecord tagList) ->
      (env, Seq.toList (findProjBottom tagList projs) @ init)
    | Some _ -> env, init
    | None -> env, init

  let initialEnv = ["Policies", TotalCollection [tag_policy] ; "Groups", TotalCollection [tag_group] ;
                    "Equities", TotalCollection [tag_equity] ; "CashFlows", TotalCollection [tag_cashFlow] ;
                    "Global", TotalRecord [tag_global]]

  let subOfInitialEnd (dS : dataStructure<Position>) tag =
    List.exists (fun (_, tagCol) ->
      let tag' =  match tagCol with TotalCollection[tag'] -> tag' | TotalRecord [tag'] -> tag' | _ -> failwith "impossible"
      ASTUtil.tagIsSubOf dS.superTypes tag tag'
    ) <| initialEnv

  let rec canProj (dS : dataStructure<Position>) tag proj (init : (string * string) list) =
    if List.exists (fun (tag',proj') -> tag = tag' && proj = proj') <| init@globalHasBeen
    then true
    else
      match dS.superTypes.[tag] with
      | None -> false
      | Some tag' -> canProj dS tag' proj init

  let rec CheckModuleUsage
            dS
            (dsModul : Map<string, constantSyntax<Position> * dataStructure<Position>>)
            env
            init
            moduleName
            callName
            callName_SA
            (isAction : bool) : AnnotatedResult<typErr list, checkEnv> =
    annoResult {
      if not <| dsModul.ContainsKey moduleName
      then
        return { varBindings = env; inits = init}  // The module is unkown, handled error handled by typer
      else
        let _,mDs = dsModul.[moduleName]
        if isAction && callName = "init"
        then
          let iMap = Option.get mDs.interfaceMap
          let varInit' =
            Map.fold
              (fun init' k m ->
                Map.fold
                  ( fun init'' k' m' ->
                    (k, k') :: init''
                  ) init' (snd m)
              ) (fst init) iMap
          return { varBindings = env; inits = varInit', moduleName :: snd init }
        else
          if
            match mDs.interfaceMap with
            | None -> false
            | Some (iface) -> Map.exists (fun k (req,p : Map<string, typ<Position>>) -> (Map.count p) > 0) iface
          then
            // Here we could inline. Like above but it is a bit weird since we have the contract
            let abstractionName = if isAction then "actions" else "functions"
            do! addAnnoWhenFalse
                  ((ErrorMsgs.module_not_initialised (moduleName, abstractionName)) callName_SA.kwStart callName_SA.kwEnd)
                  (List.contains moduleName (snd init))
            return { varBindings = env; inits = init}
          else
            return { varBindings = env; inits = init}
    }


  and CheckExpr (dS : dataStructure<Position>)
                (dsModul : Map<string, constantSyntax<Position> * dataStructure<Position>> )
                allDecs
                (init : fieldInits * moduleInits)
                (expr: expr<typ<Position>, Position>) : AnnotatedResult<typErr list, unit> =
    let CE = CheckExpr dS dsModul allDecs init
    match expr with
    | EMissing   (_, _)                     -> annoResult { return () }
    | CDouble    (_, _, d)                  -> annoResult { return () }
    | CBool      (_, _, b)                  -> annoResult { return () }
    | CEnum      (_, _, s)                  -> annoResult { return () }
    | Pair       (_, _, e1, e2)             -> annoResult { do! CE e1
                                                            do! CE e2 }
    | CStr       (_, _, s)                  -> annoResult { return () }
    | Var        (_, _, x)                  -> annoResult { return () }
    | Proj       (_, _, e1, (sA_proj,proj))       -> annoResult {
                                                 do! CE e1
                                                 match eAnnoOf e1 with
                                                 | TRec (_,tags) ->
                                                     do! iterMA
                                                          (fun tag ->
                                                              if subOfInitialEnd dS tag && not (canProj dS tag proj (fst init))
                                                              then
                                                                annoResult {
                                                                   do! AnnoResult.add <| ErrorMsgs.value_used_before_init (proj, tag) sA_proj.kwStart sA_proj.kwEnd
                                                                   return ()
                                                                 }
                                                              else annoResult {return ()}
                                                             ) tags
                                                 | TModule (m_name,_) ->
                                                    let! _ = CheckModuleUsage dS dsModul [] init m_name proj sA_proj false
                                                    return ()
                                                 | _ -> return () // Problem reported by typer
                                               }
    | Filter     (_, _, e1, tags)           -> annoResult { do! CE e1 }
    | Let        (_, _, lets, e_body)       -> annoResult {
                                                do! iterMA (fun (_,_,e1) -> annoResult {do! CE e1} ) lets
                                                do! CE e_body
                                              }
    | If         (_, _, e1, e2, e3)         -> annoResult { do! CE e1
                                                            do! CE e2
                                                            do! CE e3
                                              }
    | Match      (_, _, e1, cases)          -> annoResult { do! CE e1
                                                            do! iterMA (fun (_,_,e1) -> annoResult {do! CE e1} ) cases
                                              }
    | Map        (_, _, forIns, oWhere, e1) -> annoResult {
                                                 do! iterMA
                                                       (fun (_,choice) ->
                                                          annoResult {
                                                            match choice with
                                                            | Choice1Of2 (es) -> do! iterMA (fun (_,e1) -> annoResult {do! CE e1} ) es
                                                            | Choice2Of2 (_,e1) -> do! CE e1
                                                          } ) forIns
                                                 do! match oWhere with
                                                     | None -> annoResult {return ()}
                                                     | Some (_,e1) -> annoResult {do! CE e1}
                                                 do! CE e1
                                              }
    | FCall      (_, _, e1, args)           ->
      annoResult {
          do! CE e1
          do! iterMA (fun (_, e1) -> annoResult { do! CE e1} ) args
      }
    | BinOp      (_, _, binOp, e1, e2)      -> annoResult {
                                                do! CE e1
                                                do! CE e2
                                              }
    | EPar       (_, _, e1)                 -> annoResult { do! CE e1 }
    | ENone      (_, _, _)                  -> annoResult { return () }
    | ESome      (_, _, e1)                 -> annoResult { do! CE e1 }
    | CreateRec  (_, _, _, args)            -> annoResult { do! iterMA (fun (_,e1) -> annoResult { do! CE e1} ) args  }
    | List       (_, _, contents, anno)     -> annoResult { do! iterMA (fun (_,e1) -> annoResult { do! CE e1} ) contents }


  /// <summary>
  /// Updates environment and list of initialized variables for a statement
  /// Input: statement, current environment, and current init list
  /// </summary>
  ///
  let rec CheckStmt
            (dS : dataStructure<Position>)
            (dsModul : Map<string, constantSyntax<Position> * dataStructure<Position>>)
            allDecs
            env
            (init : fieldInits * moduleInits)
            stmt : AnnotatedResult<typErr list, checkEnv> =
    match stmt with
    | SDoCSharp   _ ->
      annoResult {
        return { varBindings = env; inits = init}
      }
    | SDo         (_, ((_,(_,s)),[]) ,args) -> // Module-internal action
      annoResult {
        do! iterMA (fun (_,e1) -> annoResult { do! CheckExpr dS dsModul allDecs init e1 } ) args

        let actDec =
          List.tryPick
            (fun dec ->
              match dec with
              | ActDec (_,(_,s'),actPs,stmt) when s = s' -> Some (actPs,stmt)
              | _ -> None
            ) allDecs

        match actDec with
        | None -> return { varBindings = env; inits = init} // Handled by typer
        | Some (actPs, actionStmt) ->
          if List.length actPs <> List.length args
          then return { varBindings = env; inits = init} // Handled by typer
          else
            let actionEnv =
              List.map2 (fun (_,(_,s),_) (_,e) ->
                  match ExprToInitType env e with
                  | Bot -> None
                  | initType -> Some (s, initType)
                ) actPs args
            let actionEnv = List.choose id actionEnv
            let! result' = CheckStmt dS dsModul allDecs (actionEnv@initialEnv) init actionStmt
            return  { varBindings = env; inits = result'.inits }
      }
    | SDo         (_, ((_,(_,m_name)), (_,_,(p_sA,p)) :: [])  ,args) -> // Module-external action
      annoResult {
        do! iterMA (fun (_,e1) -> annoResult { do! CheckExpr dS dsModul allDecs init e1 } ) args
        return! CheckModuleUsage dS dsModul env init m_name p p_sA true
      }
    | SDo         (_, _, _) -> failwith "unexpected case"
    | SOverwrite  (_,((_, (xPos, x)), projs), expr) ->
      annoResult {
        do! CheckExpr dS dsModul allDecs init expr
        let env, varInits = assproj env (fst init) projs x
        return { varBindings = env; inits = varInits, snd init}
      }
    | STransfer   (_,_,expr,((_, (xPos, x)), projs)) ->
      annoResult {
        do! CheckExpr dS dsModul allDecs init expr
        return { varBindings = env; inits = init}
      }
    | SkippedStmt _ -> annoResult { return { varBindings = env; inits = init} }
    | SUpdate (_, (_,forIn), wherefilter, stmt) ->
      annoResult {
        do!
          match forIn with
          | Choice1Of2 a -> iterMA (fun (_,e1) -> annoResult { do! CheckExpr dS dsModul allDecs init e1 } ) a
          | Choice2Of2 (_,e1) -> annoResult { do! CheckExpr dS dsModul allDecs init e1 }

        match wherefilter with
        | Some (_,e1) ->
          do! CheckExpr dS dsModul allDecs init e1
          return { varBindings = env; inits = init} // Nothing, done
        | None ->
          let forinEnv     = CheckForIn forIn env
          let! result = CheckStmt dS dsModul allDecs forinEnv init stmt
          return { varBindings = env; inits = result.inits}
      }
    | SLet (_, (pos,x), expr) ->
      annoResult {
        do! CheckExpr dS dsModul allDecs init expr
        let env' = match (ExprToInitType env expr) with | Bot -> env | typ -> (x, typ)::env
        return { varBindings = env'; inits = init} // [x -> inittype]
      }
    | SAss (_, ((_, (xPos, x)), projs), expr) ->
      annoResult {
        do! CheckExpr dS dsModul allDecs init expr
        let env, varInits = assproj env (fst init) projs x
        return { varBindings = env; inits = varInits, snd init}
      }
    | SBlock (sA, stmts) ->
      annoResult {
        return! foldMA (fun res -> CheckStmt dS dsModul allDecs res.varBindings res.inits) { varBindings = env; inits = init} stmts
      }
    | SIf (_, cond, t, fOpt) ->
      annoResult {
        do! CheckExpr dS dsModul allDecs init cond
        //TODO: Inits in if-branches are ignored for now
        //let! result_T = CheckStmt dS dsModul allDecs env init t
        //let! result_F =
        //  match fOpt with
        //  | Some(_, f) -> CheckStmt dS dsModul allDecs env init f
        //  | None -> annoResult { return { varBindings = env; inits = ([],[]) }}
        return { varBindings = env; inits = init }
      }
    | SMatch (_, e, cases) ->
      annoResult {
        do! CheckExpr dS dsModul allDecs init e
        //TODO: Inits in switch-cases are ignored for now
        return { varBindings = env; inits = init }
      }

  let CollectInitialised (decs : dec<typ<Position>, Position> list)
                         (dS : dataStructure<Position>)
                         (dsModul : Map<string, constantSyntax<Position> * dataStructure<Position>>) =
    let initStmt = List.tryPick (fun dec -> match dec with | ActDec(_,(_,"init"),_,stmt) | Export(_,ActDec(_,(_,"init"),_,stmt)) -> Some stmt | _ -> None) decs

    // Modules may consider its 'required' fields as being initialised
    let requiredInitialised =
      match dS.interfaceMap with
      | None -> []
      | Some contracts ->
        contracts
          |> Map.toSeq
          |> Seq.map
               (fun (tag, (reqs, _)) ->
                  reqs
                  |> Map.toSeq
                  |> Seq.map (fun (fieldName,_) -> tag,fieldName)
                  |> Seq.toList
               )
          |> List.concat

    let res, errs =
      match initStmt with
      | Some stmt -> AnnoResult.run [] <| CheckStmt dS dsModul decs initialEnv (requiredInitialised, []) stmt
      | None -> { varBindings = []; inits = [], []}, []
    res.varBindings, (errs, (fst res.inits)@globalHasBeen)

  /// <summary>
  /// This function removes all fields that has been initialised
  /// from the map that contains the fields that should be initialised.
  /// 'shouldBe' is a map from 'tag' to a (map from 'fieldName' to 'Type'))
  /// 'hasBeen' is a list of 'tag' 'fieldname'
  /// </summary>
  let FilterInitialisedFields (modul : modul<'eA, Position>) (dS : dataStructure<Position>)
                              (hasBeen : (string * string) list) =
    // We are only interested in checking initialisation of entities subtype of initialEnv
    let shouldBe =
      match fst modul with
      | MainModul ->
        Map.filter (fun tag _ -> subOfInitialEnd dS tag) dS.totalMap
      | ImportModul _ ->
        let iMap = Option.defaultWith (fun _ -> failwith "impossible") dS.interfaceMap
        Map.map (fun _ (_,v) -> v) iMap


    // ex shouldBe = ["Group" -> ["Foo" -> T, "Bar" -> T], "Interest" -> ["Foo" -> T]]
    // ex hasBeen  = ["Group", "Foo"; "Group", "Bar"]
    // ex res      = ["Group" -> [], "Interest" -> []]
    let shouldBeFilter =
      shouldBe |> Map.map (fun tag m -> Map.filter (fun fieldName t ->
                                                        // filter out map entries which are initialized
                                                        // the remaining entries are the uninitialized
                                                        match (InitLookup2 hasBeen tag fieldName dS.superTypes) with | None -> true | Some fName -> false) m )
    shouldBeFilter

  let InitCheckModule (modul : modul<typ<Position>, Position>)
                      (dS : dataStructure<Position>)
                      (dsModul : Map<string, constantSyntax<Position> * dataStructure<Position>>) =
    let typErrs, hasBeen = snd (CollectInitialised (snd modul) dS dsModul)
    typErrs, FilterInitialisedFields modul dS hasBeen |> Map.toSeq

  let addErrors (dS : dataStructure<Position>) errs  =
    let usageErrs, initErrs = errs
    let initErrors =
       initErrs
       |> Seq.map (fun (tag, fields) ->
         let fields_info = dS.fieldNameInfo.[tag]
         Map.toSeq fields
           |> Seq.map (fun (field, _) ->
              let pos =
                if Map.containsKey field fields_info
                then fields_info.[field]
                else dS.positions.[tag]
              ErrorMsgs.field_not_init_by_init (field, tag) pos.kwStart pos.kwEnd
           )
           |> Seq.toList
       )
       |> Seq.concat
       |> Seq.toList
    usageErrs@initErrors


  /// <summary>
  /// Ensures that all entity fields are initialised.
  /// Approach described in github wiki
  /// </summary>
  /// <param name="modul">Module to be init-checked</param>
  /// <param name="deps">Flat module dependencies of 'modul'</param>
  /// <param name="dS">Datastructure information of 'modul'</param>
  /// <param name="dsModul">Syntax information and datastructure of dependencies.</param>
  let InitCheckProgram (modul : modul<typ<Position>, Position>)
                       (deps : (dependencyKind * string * modul<typ<Position>, Position>) list)
                       (dS : dataStructure<Position>)
                       (dsModul : Map<string, constantSyntax<Position> * dataStructure<Position>>) =
    let a =
      deps
        |> List.filter (fun (depKind,n,_) -> depKind = DependAll)
        |> List.map (fun (d,(m_name),modul) ->
            let (sA_dS, m_dS) = dsModul.[m_name]
            let typErrs, initErrs = InitCheckModule modul m_dS dsModul
            if Seq.forall (fun (_,m) -> Map.isEmpty m) initErrs && List.isEmpty typErrs
            then None
            else
                Some <| ErrorMsgs.module_does_not_init_provides m_name sA_dS.kwStart sA_dS.kwEnd
           )

    let impErrs : typErr list = List.choose id a
    let moduleCheck = InitCheckModule modul dS dsModul
    impErrs @ (addErrors dS moduleCheck)
