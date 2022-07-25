namespace itu.dk.MAL

module Dataflow =
  open ToIntermediate
  open System
  open System.Configuration
  open System.Collections.Generic

  type Id = int
  type ScopeLevel = int
  type Vertex = Id * ScopeLevel
  type Edge = Vertex * Vertex
  type Graph = int * Set<Edge> * Map<Id, ILstmt>
  type AliasClass = Set<name>
  //type Settings = AppSettings<"../AppConfig/AMLU.config">

  let SetFilter p (s : HashSet<'a>) =
    let h = new HashSet<'a>()
    for v in s do
      if p v
      then h.Add(v) |> ignore
      else ()
    h
  let SetIsEmpty (s : HashSet<'a>) = s.Count > 0
  let SetMap (f: 'a -> 'b) (s : IEnumerable<'a>) =
    let h = new HashSet<'b>()
    for v in s do
      h.Add(f v) |> ignore
    h

  let assignName = function
    | Assign ((_,x), e) -> fullname [] x
    | Let (x, _, _) -> [x]
    | For (_, iters, _, _) -> List.map fst iters
    | Match (_, cases) -> List.map (fun (_,x,_) -> x) cases
    | _ -> failwith "can only be used on assignments"

  let rec extendGraph ((count, edges, map) as graph) stmt scope connectTo : Vertex * Graph =
    let newCount = count + 1
    let addFragment stmt =
      let newVert = newCount, scope
      let newEdge = (newVert, connectTo)
      newVert, (newCount, Set.add newEdge edges, Map.add newCount stmt map)
    match stmt with
    | Let _ | Expression _
    | Assign _ | Declare _
    | SkipIfNot _ | DoCSharp _ -> addFragment stmt
    | If (e, s1, s2) ->
      let v1, g1 = extendGraph graph s1 (scope + 1) connectTo //either increment scope at statements that can contain a block, or at the block itself
      let v2, (Vertexs, edges, map) = extendGraph g1 s2 (scope + 1) connectTo
      let newCount = Vertexs + 1
      let thisVert = (newCount, scope)
      let e1,e2 = (thisVert, v1), (thisVert, v2)
      thisVert,
      ( newCount,
        Set.add e1 edges |> Set.add e2,
        Map.add newCount (Expression e) map) //Todo hvorfor ikke bare If ?
    | VectorLoop(counterName, iters, body, d, wrapup) ->
      let (v,g) = extendGraph graph body scope connectTo
      extendGraph g wrapup scope v
    | Foreach(_,_,_,_,body,_)
    | For(_, _, body, _) ->
      let newCount = count + 1
      let thisVert = newCount, scope
      let thisEdge = (thisVert, connectTo)
      let graph =
        ( newCount,
          Set.add thisEdge edges,
          Map.add newCount stmt map)
      let (v, (Vertexs, edges, map)) = extendGraph graph body (scope + 1) thisVert
      thisVert, (Vertexs, Set.add (thisVert, v) edges, map)
    | Block stmts ->
      Seq.foldBack (fun stm (v,g) -> extendGraph g stm scope v) stmts (connectTo, graph)
    | Match (e, cases) ->
      let folder (verts, g) (_, x, s) =
        let (v, g) = extendGraph g s (scope + 1) connectTo
        let assign = Let(x, e, true)
        let (v', g) = extendGraph g assign (scope + 1) v
        (v'::verts, g)
      let (verts, (Vertexs, edges, map)) =
        List.fold folder ([],graph) cases
      let newCount = Vertexs + 1
      let thisVert = newCount, scope
      let es = List.map (fun v -> (thisVert, v)) verts
      let edges = Set.ofList es |> Set.union edges
      thisVert,
      ( newCount,
        edges,
        Map.add newCount (Expression e) map)
  let stmtsToCFG stmts =
    let graph = (0, Set.empty, Map.add 0 (Block []) Map.empty)
    List.foldBack (fun stm (v,g) -> extendGraph g stm 0 v) stmts ((0, 0), graph)

  type AssignmentSet = HashSet<Id*ILstmt>
  type Environment = {globals : AssignmentSet; locals : AssignmentSet list}

  let emptySet() = new HashSet<_>()
  let partition decider (set:HashSet<_>) =
    let st, sf = emptySet(), emptySet()
    for i in set do
      if decider i then st.Add i |> ignore else sf.Add i |> ignore
    st,sf
    //Seq.iter (fun (i:AssignmentSet) -> if decider i then st.Add i |> ignore else sf.Add i |> ignore) (enum)

  let globals (env:Environment) = env.globals
  let locals (env:Environment) = env.locals

  let pop env = {globals = globals env; locals = List.tail <| locals env}
  let push env = {globals = globals env; locals = emptySet()::locals env}
  let empty = {globals = emptySet(); locals = [emptySet()]}

  let isAssignment state map =
    match Map.tryFind state map with
    | None -> false
    | Some stm ->
      match stm with
      | Let (_) | Assign (_) -> true
      | _ -> false
  let getAssigns map vert =
    match Map.tryFind vert map with
    | None -> emptySet()
    | Some (assigns,_) -> assigns
  let getOverrides map vert =
    match Map.tryFind vert map with
    | None -> emptySet()
    | Some (_, overrides) -> overrides

  let allBindings env =
    let set = emptySet()
    set.UnionWith (globals env)
    List.iter (set.UnionWith) (locals env)
    set
  let chkShadowing vname assignSet = partition (snd >> assignName >> isPrefixOf vname) assignSet

  let chkLocalShadowing vname locs =
    let rec inner acc =
      function
      | [] -> emptySet(), List.rev acc
      | l::ls ->
        let shadows, assignments = chkShadowing vname l
        if shadows = emptySet() then inner (l::acc) ls else shadows, List.rev acc@assignments::ls
    inner [] locs

  let unify aliasses assignee assigned =
    let intermediate = Set.map (Set.filter (not << (=) assignee)) aliasses
    if Set.exists (fun s -> Set.contains assigned s) intermediate
    then Set.map (fun ac -> if Set.contains assigned ac then Set.add assignee ac else ac) intermediate
    else Set.add (set [assignee; assigned]) intermediate

  let updateAliasset s old =
    match s with
    | Assign((_, Var x), (_, Var y)) -> unify old x y
    | Let(x, (_, Var y), _) -> unify old x y
    | _ -> old

  let addBinding env (vid,s) =
    let g = globals env
    let l = locals env
    match s with
    | Let _ | For _ ->
      let bindName = assignName s
      let overrides, locs = chkLocalShadowing bindName l
      let lh, lt = locs.Head, locs.Tail
      lh.Add (vid, s) |> ignore
      { globals = g ; locals =  lh :: lt}, overrides
    | Assign _ ->
      let bindName = assignName s
      let overrides, glob = chkShadowing bindName g
      glob.Add(vid, s) |> ignore
      { globals = glob; locals = l}, overrides
    | _ -> env, emptySet()

  type Analysis = Map<Id, AssignmentSet * AssignmentSet>
  let analysis (start : Vertex, (_, edges, map) : Graph) : Analysis =
    let rec loop env scope acc vert =
      let vId, newScope = vert
      let env = if newScope > scope then push env else if newScope < scope then pop env else env
      let newEnv, overrides = addBinding env (vId, Map.find vId map)

      let oldAssignSet = getAssigns acc vId
      let copy = oldAssignSet.Count
      oldAssignSet.UnionWith (allBindings env)

      let oldOverride = getOverrides acc vId
      oldOverride.UnionWith overrides

      let accNew = Map.add vId (oldAssignSet, oldOverride) acc //using old env gives us assignments before the node
      let succ = Set.filter (fst >> (=) vert) >> Set.map snd <| edges
      assert(copy <= oldAssignSet.Count)
      let succ =
        if copy < oldAssignSet.Count
        then succ
        else Set.filter (fun (s, _) -> not (Map.containsKey s accNew)) succ
      Set.fold (loop newEnv newScope) accNew succ
    loop empty 0 Map.empty start

  let readsfrom reader stmt =
    let var = assignName stmt
    //Does not check recursively, as any inner statements will have
    //their own Vertex assosicated with them.
    let stmtContainsVar name stmt =
      let exp =
        match stmt with
        | Let (_, e, _) -> e
        | Assign(_,e) -> e
        | VectorLoop(_,_,_,_,_) -> (TBool, CstB false)
        | Foreach _ -> (TBool, CstB false)
        | For (_, _, _, _) -> (TBool, CstB false) //the collection being iterated should not be relevant for dependency analysis
        | Match (e, _) -> e
        | Declare _ | Block _ -> (TBool, CstB false)
        | DoCSharp _ -> (TBool, CstB false)
        | SkipIfNot (e) -> e
        | Expression (e) -> e
        | If (e,_,_) -> e
      exprContainsVar name (snd exp)
    let assignLHS =
      match reader with
      | Assign((_,x),_) -> isPrefixOfStrict var (fullname [] x)
      | _ -> false
    //assignLHS ||
    stmtContainsVar var reader

  let mkDependencyGraph (start, (_, edges, lookup) as graph) =
    let analysis = analysis graph

    let addNode transitions vertex =
      let assignments, overrides = Map.find vertex analysis
      let (f, o) = transitions
      let outs = Seq.fold (fun s (v,_) -> Set.add (v, vertex) s) o overrides
      let addDependencies (flow, output) (vert, stmt) =
        if readsfrom (Map.find vertex lookup) stmt
        then Set.add (vert, vertex) flow, output
        else (flow, output)
      Seq.fold addDependencies (f, outs) assignments

    let rec traverse vert visited trans =
      let visited = Set.add vert visited
      let succ = Set.filter (fun (f,t) -> f = vert && not <| Set.contains t visited) edges |> Set.map snd
      let transitions = addNode trans (fst vert)
      Set.fold (fun (t,v) s -> traverse s v t) (transitions, visited) succ
    traverse start Set.empty (Set.empty, Set.empty) |> fst

  let rec revStmt stm =
    match stm with
    | Let _ | Expression _
    | Assign _ | Declare _
    | DoCSharp _ | SkipIfNot _ -> stm
    | VectorLoop(counterName, iters, body, d, wrap) -> VectorLoop(counterName, iters, revStmt body, d, revStmt wrap)
    | If (e, s1, s2) -> If (e, revStmt s1, revStmt s2)
    | For(iName, iters, body, dest) -> For (iName, iters, revStmt body, dest)
    | Foreach(cName, kName, vName, iters, body, dest) -> Foreach (cName, kName, vName, iters, revStmt body, dest)
    | Block stmts -> Seq.rev stmts |> Seq.map revStmt |> Block
    | Match (e, cases) -> Match (e, List.map (fun (t,n,s) -> (t,n, revStmt s)) cases)

  type DependencySet = HashSet<ILstmt*ILstmt>
  let collectDependencies stmts : DependencySet * DependencySet * DependencySet =
    let cfg = stmtsToCFG stmts
    let (flow, output) = mkDependencyGraph cfg
    let revcfg = stmtsToCFG ([revStmt <| Block stmts])
    let (anti, _) = mkDependencyGraph revcfg
    let (_,(_,_,lookup)) = cfg
    let (_,(_,_,revlookup)) = revcfg
    let revlookup = Map.map (fun _ v -> revStmt v) revlookup
    let find (v1,v2) = Map.find v1 lookup, Map.find v2 lookup
    let revfind (v1,v2) = Map.find v2 revlookup, Map.find v1 revlookup
    SetMap find flow, SetMap revfind anti, SetMap find output

  let rec prExpr (e : annoILexpr) : string =
    match e with
    | _ , CstD d -> sprintf "%f" d
    | _ , CstI i -> sprintf "%d" i
    | _ , CstB b -> sprintf "%b" b
    | _ , CstS s -> sprintf "\"%s\"" s
    | _ , Init (t,es) -> "init"
    | _ , Var nm -> nm
    | _ , Access (e, nm) -> sprintf "%s.%s" (prExpr e) nm
    | _ , Call (_, f, args) -> sprintf "%s(%s)" (prExpr f) (List.map prExpr args |> List.reduce (fun c n -> sprintf "%s, %s" c n))
    | _ -> "anyE"
    //| _ , (Index  of annoILexpr * annoILexpr
    //| _ , (Binop  of binOp * annoILexpr * annoILexpr
    //| _ , (Call   of funtyp * annoILexpr * annoILexpr list

  let prStmt (stmt : ILstmt) =
    match stmt with
    | For  (iName, iters, smt, dest) ->
      let names = String.concat "," <| List.map fst iters
      let exprs = String.concat "," <| List.map (prExpr << snd) iters
      sprintf "for %s (%s) in %s" names iName exprs
    | Foreach (cName, kName, vName, source, body, dest) ->
      sprintf "foreach (%s,%s) in %s" kName vName (prExpr source)
    | DoCSharp (extIdent) -> sprintf "doC# %s()" extIdent
    | Let  (name, expr, mut) -> sprintf "let %s = %s in" name (prExpr expr)
    | Expression  (expr) -> sprintf "expr(%s)" (prExpr expr)
    | Assign (e1, e2) -> sprintf "%s=%s" (prExpr e1) (prExpr e2)
    | Block (_) -> "Block"
    | Declare (_) -> "Declare"
    | If (_) -> "If"
    | Match (_) -> "Match"
    | SkipIfNot (_) -> "SkipIfNot"
    | VectorLoop(_) -> "VectorLoop"

  type depsRec =
    { Flow : HashSet<ILstmt * ILstmt>
    ; Anti : HashSet<ILstmt * ILstmt>
    ; Output : HashSet<ILstmt * ILstmt>
    }

  //let depsToDot (deps : depsRec) (aliasMap : Dictionary<ILstmt, ILstmt list>) : string =
  //  let mutable output = "digraph G {"
  //  let mutable count = 0
  //  let (idMap : Dictionary<ILstmt , int>) = new Dictionary<ILstmt , int>()
  //  let lookup stmt =
  //    if idMap.ContainsKey stmt
  //    then idMap.[stmt]
  //    else idMap.Add(stmt, count)
  //         count <- count + 1
  //         count - 1
  //  let printSet set style =
  //    Set.fold
  //      ( fun out (a,b) -> sprintf "%s%d -> %d[arrowhead=%s]\n" out (lookup a) (lookup b) style
  //      ) "" set
  //  output <- sprintf "%s %s" output (printSet deps.Flow "normal")
  //  output <- sprintf "%s %s" output (printSet deps.Anti "inv")
  //  output <- sprintf "%s %s" output (printSet deps.Output "dot")
  //  Map.iter
  //    (fun stmt aliases ->
  //      List.iter
  //        (fun alias ->
  //          output <- sprintf "%s %d->%d[color=green]\n" output (lookup stmt) (lookup alias)
  //        ) aliases
  //     ) aliasMap
  //  Map.iter
  //    (fun stmt i -> output <- sprintf "%s %d[label=\"%s\"]" output (idMap.[stmt]) (prStmt stmt))
  //    idMap
  //  sprintf "%s}" output

  let rec containedIn stm1 stm2 =
    if stm1 = stm2 then true else
    let travel = containedIn stm1
    match stm2 with
    | Let _ | Expression _
    | Assign _ | Declare _
    | SkipIfNot _ | DoCSharp _ -> false
    | If (e, s1, s2) -> (Expression e) = stm1 || travel s1 || travel s2
    | VectorLoop(_, _, body,_,_)
    | Foreach(_,_,_,_,body,_)
    | For(_, _, body, _) -> travel body
    | Block stmts -> Seq.exists travel stmts
    | Match (e, cases) -> List.exists (fun (_,x,s) -> (Let (x,e,true)) = stm1 || travel s) cases

  /// <summary>
  /// Returns true if depedency is a write-dependency to anything other than a sub-field of the iteration variable
  /// </summary>
  let troublesomeOutput iterVar dependency =
    match dependency with
    | Assign((_,x),_),_ -> not <| isPrefixOf [iterVar] (fullname [] x)
    | _ -> false

  let sameClass aliasses x y =
    Set.exists (fun s -> Set.contains x s && Set.contains y s) aliasses

  let rec findAliases (aMap : Dictionary<ILstmt, ILstmt list>) fstmt stmt aliases : Dictionary<ILstmt, ILstmt list> =
    let isAlias expr =
      match expr with
      | _,Var _ -> Seq.exists (readsfrom (Expression expr)) aliases
      | _ -> false


    let filter aliases x = List.filter (not << readsfrom (Expression (TBool, Var x))) aliases
    aMap.Add(stmt, aliases)

    match stmt with
    | For (_, iters, stmt, _) ->
        let names = List.map fst iters
        let aliases' = List.fold filter aliases names
        findAliases aMap fstmt stmt aliases'
    | Match (aE, cases) ->
        let newAlias = isAlias aE
        List.fold
          (fun m (_, nm, stmt')
            ->
              let caseStmt = Let(nm, aE, true)
              m.Add(caseStmt, aliases)
              let aliases' = filter aliases nm
              let aliases' = if newAlias then caseStmt :: aliases' else aliases'
              findAliases m fstmt stmt' aliases') aMap cases
    | Block stmts -> Seq.fold (fun m' stmt -> findAliases m' fstmt stmt aliases) aMap stmts
    | Let (x, e1, _) -> aMap
    | _ -> aMap

  let getAlias (aliasMap : Dictionary<'a,'b list>) s =
    if (aliasMap.ContainsKey s)
    then aliasMap.[s]
    else []

  let sameSource s2 s1 = isPrefixOf (assignName s1) (assignName s2)

  let canBeParallelized dependencies s =
    let readTest (aliasMap : Dictionary<ILstmt, ILstmt list>) (s1, s2) =
      List.exists ((=) s1) (getAlias aliasMap s2)

    let writeTest (aliasMap : Dictionary<ILstmt, ILstmt list>) (s1, s2) =
      List.exists (sameSource s2) (getAlias aliasMap s1)

    let relevant (s1, s2) = containedIn s2 s //&& not <| containedIn s1 s
    //possibly, dependencies ON the for-loop is also irrelevant, so maybe only containedIn s2 s
    match s with
    | For (_iter, iters, b, d) ->
      let initDict = new Dictionary<ILstmt, ILstmt list>()
      initDict.Add(s, [s])
      let aliasMap = findAliases initDict s b [(s)]
      //printfn "%A" aliasMap
      let (flow, anti, output) = dependencies
      //let dotFile = Environment.CurrentDirectory + (sprintf "\\%s\\deps.dot" Settings.CompilationOutputDirectory);
      //use file = System.IO.File.CreateText(dotFile)
      //printfn "%A\n%A\n%A" flow anti output
      let flow = SetFilter relevant flow
      let anti = SetFilter relevant anti
      let output = SetFilter relevant output
      let flow = SetFilter (not << (readTest aliasMap)) flow
      let anti = SetFilter (not << (writeTest aliasMap)) anti
      let output = SetFilter (not << (writeTest aliasMap)) output
      //let output = Set.filter (troublesomeOutput x) output //remove Lets and Assigns to iteration variable
      //try
      //  file.WriteLine (depsToDot  {Flow = flow; Anti = anti; Output = output} aliasMap)
      //with
      //  e -> printfn "%A" e

      //let flowTest = Set.forall (readTest aliasMap) flow
      //let antiTest = Set.forall (fun (s1, s2) -> readTest aliasMap (s2, s1)) anti
      //printfn "%b %b" flowTest antiTest
      //let anti = emptySet()
      let flow = SetFilter (fun (s1,_) -> s1 <> s && match s1 with Let (_,_,b) -> not b | _ -> true) flow
      //let flow = Set.filter (fun (s1,_) -> not (sameClass (Map.find s1 aliasClasses) (List.head <| assignName s1) x)) flow
      let deps = [flow; anti; output]
      //printfn "%A" deps
      List.forall SetIsEmpty deps
    | _ -> failwith "not a loop"

  let rec mustPrecede deps s1 s2 =
    not (Set.exists((=) (s2, s1)) deps) && //allows for earlier termination during recursion
    Set.exists ((=) (s1, s2)) deps ||
    let relatives = Set.filter (fst >> (=) s1) deps |> Set.map snd
    Set.exists (fun s -> mustPrecede deps s s2) relatives

  //let mergeLoops deps stmts =
  //  let rec getLoops s =
  //    match s with
  //    | For _ -> [s]
  //    | Block ss -> List.collect getLoops ss
  //    | _ -> [s]
  //  let loops = List.collect getLoops stmts
  //  let merge acc = function
  //    | For ()