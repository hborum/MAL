module Benchmarks

//open BenchmarkDotNet.Attributes

//open FSharp.Configuration
//open Gen

//open System.Reflection
//open System.Linq
//open System.Collections.Generic

//type Settings = AppSettings<"App.Config">

//type ManageMethod = {
//  classObject : obj;
//  info : MethodInfo;
//  parameters : obj []
//}
//let nullMethod = {
//  classObject = null;
//  info = null;
//  parameters = null
//}

//let setup groups policies states=
//  let inputGen = new InputGenerator(TestUtil.createBenchmarkConfig(1,100,1,1,0, groups, policies))
//  inputGen.CreateInput(100, states)

//let setupStateForFull groups policies depKind states =
//  let mutable dependencies = Simulator.GetDependencies(depKind)
//  let struct (configuration, input) = setup groups policies states
//  if dependencies :? ProjectionStateHandlerDependencies
//  then
//    let dep = dependencies :?> ProjectionStateHandlerDependencies
//    dep._stateFactory.Input <- input
//  else if dependencies :? DummyStateHandlerDependencies
//  then
//    let dep = dependencies :?> DummyStateHandlerDependencies
//    dep._stateFactory.Input <- input
//  else
//    failwith "Unknown kind of dependencies"
//  input, dependencies, configuration

//let setupStateAndResult (statehandler : IProjectionStateHandler<IFunLambda,ProjectionOutput, 'ProjResult>) groups policies depKind states =
//  let input, dependencies, configuration = setupStateForFull groups policies depKind states
//  let calcPeriodEndTimes = ref null
//  statehandler.SetupDependencies(dependencies)
//  let mutable state = statehandler.Initialize(input, configuration, calcPeriodEndTimes)
//  state,  ProjectionResult.FakeProjection(dependencies.FunctionFactory, state, input, 1.0, 2.0)


//let setupGenExecutor statehandler groups policies depKind states =
//  let state, result = setupStateAndResult statehandler groups policies depKind states
//  let customStateValue = state.CustomStateValue :?> System.Runtime.CompilerServices.ITuple
//  let postResult = statehandler.PreUpdate(result, state)
//  statehandler.Update(state, postResult) |> ignore
//  customStateValue.Item 1, result

//let inline invokeManage (method : ManageMethod) = method.info.Invoke(method.classObject, method.parameters)

//let mutable assemblies = []

//let loadAssembly assemblyPath =
//  match List.tryFind (fun (k,_) -> k = assemblyPath) assemblies with
//  | None ->
//    let assem = Assembly.LoadFile assemblyPath
//    assemblies <- (assemblyPath,assem) :: assemblies
//    assem
//  | Some (_,v) -> v


//[<  MemoryDiagnoser >]
//[<SimpleJob(launchCount = 1, warmupCount = 3, targetCount = 6)>]
//type InitBench () =
//  member val defaultInit    = id with get,set
//  member val deforestInit   = id with get,set
//  member val vectorInit     = id with get,set
//  member val edlundInit     = id with get,set
//  member val input          = null with get,set

//  [<Params (1)>]
//  member val groups = 0 with get,set
//  [<Params (10000)>] //
//  member val policies = 0 with get,set
//  [<Params ("MinimalDep")>]
//  member val depKind = "RealisticDep" with get,set
//  [<Params ( PolicyTypes.One)>] //, "3-state", "1&3-state"
//  member val states =  PolicyTypes.One with get,set

//  member self.GetFullInitMethod(statehandler : IProjectionStateHandler<IFunLambda,ProjectionOutput, 'ProjResult>) =
//    let struct(configuration,input) = setup self.groups self.policies self.states
//    let mutable dependencies = Simulator.GetDependencies("MinimalDep")
//    if dependencies :? ProjectionStateHandlerDependencies
//    then
//      let dep = dependencies :?> ProjectionStateHandlerDependencies
//      dep._stateFactory.Input <- input
//    else if dependencies :? DummyStateHandlerDependencies
//    then
//      let dep = dependencies :?> DummyStateHandlerDependencies
//      dep._stateFactory.Input <- input

//    statehandler.SetupDependencies(dependencies)
//    fun () ->
//      let calculationTimes : IReadOnlyList<float> ref = ref <| (new List<float>() :> IReadOnlyList<float>)
//      statehandler.Initialize(input, configuration, calculationTimes) |> ignore

//  member self.MalInit(assemblyName : string) =
//    let assemblyPath =
//      System.IO.Path.GetFullPath <|
//      sprintf ".\\%s\\%s.dll"
//        Settings.CompilationOutputDirectory
//        assemblyName
//    let assembly = loadAssembly assemblyPath
//    let sim = new Simulator()
//    let statehandler = sim.GetStateHandler(assembly) // new BenchmarkDefault.FMAExampleAdapter<IFunLambda, ProjectionOutput>() //
//    self.GetFullInitMethod(statehandler)

//  [<GlobalSetup>]
//  member self.Setup() =
//    self.defaultInit <- self.MalInit(Settings.BenchmarkAssemblyDefault)
//    self.deforestInit <- self.MalInit(Settings.BenchmarkAssemblyDeforest)
//    self.vectorInit <- self.MalInit(Settings.BenchmarkAssemblyVectorized)
//    self.edlundInit <- self.GetFullInitMethod(new ExampleProjectionStateHandler<IFunLambda, ProjectionOutput>())

//  //[<Benchmark>]
//  //member self.Default () = self.defaultInit()
//  [<Benchmark>]
//  member self.Deforest () = self.deforestInit()
//  //[<Benchmark>]
//  //member self.Vector () = self.vectorInit()

//  [<Benchmark(Baseline = true)>]
//  member self.Edlund() = self.edlundInit()


//[<  MemoryDiagnoser
//>]
//[<SimpleJob(launchCount = 1, warmupCount = 4, targetCount = 8)>]
//type GeneralBench () =
//  member val edlundUpdate   = id with get,set
//  member val defaultUpdate  = id with get,set
//  member val vectorUpdate   = id with get,set
//  member val deforestUpdate = id with get,set
//  member val state          = null with get,set

//  [<Params (1)>]
//  member val groups = 0 with get,set
//  [<Params (1000,10000,20000)>] //
//  member val policies = 0 with get,set
//  [<Params ("MinimalDep")>]
//  member val depKind = "RealisticDep" with get,set
//  [<Params ( PolicyTypes.One)>] //, "3-state", "1&3-state"
//  member val states =  PolicyTypes.OneAndThree with get,set

//  member self.GetFullUpdateMethod(statehandler) =
//    let state, result = setupStateAndResult statehandler self.groups self.policies self.depKind self.states
//    fun () ->
//      let postResult = statehandler.PreUpdate(result, state)
//      statehandler.Update(state, postResult) |> ignore

//  member self.EdlundManage() = self.GetFullUpdateMethod(new ExampleProjectionStateHandler<IFunLambda, ProjectionOutput>())

//  member self.MalManage(assemblyName : string) =
//    let assemblyPath =
//      System.IO.Path.GetFullPath <|
//      sprintf ".\\%s\\%s.dll"
//        Settings.CompilationOutputDirectory
//        assemblyName
//    let assembly = loadAssembly assemblyPath
//    let sim = new Simulator()
//    let statehandler = sim.GetStateHandler(assembly)
//    let reset = statehandler.GetType().GetMethod("ResetProjNumber")
//    let func = self.GetFullUpdateMethod(statehandler)
//    fun () ->
//      do func ()
//      do reset.Invoke(statehandler, [||]) |> ignore

//  [<GlobalSetup>]
//  member self.Setup() =
//    self.edlundUpdate <- self.EdlundManage()
//    self.defaultUpdate <- self.MalManage(Settings.BenchmarkAssemblyDefault)
//    self.vectorUpdate <- self.MalManage(Settings.BenchmarkAssemblyVectorized)
//    self.deforestUpdate <- self.MalManage(Settings.BenchmarkAssemblyDeforest)

//  [<Benchmark>]
//  member self.Deforest() = self.deforestUpdate()

//  [<Benchmark(Baseline = true)>]
//  member self.Edlund() = self.edlundUpdate()
//  //[<Benchmark>]
//  //member self.Vector() = self.vectorUpdate()
//  //[<Benchmark>]
//  //member self.Default() = self.defaultUpdate()

//[<MemoryDiagnoser>]
//[<SimpleJob(launchCount = 1, warmupCount = 16, targetCount = 16)>]
//type ManageBench () =

//  member val defaultManage  = nullMethod with get,set
//  member val vectorManage   = nullMethod with get,set
//  member val parallelManage = nullMethod with get,set
//  member val deforestManage = nullMethod with get,set

//  [<Params (10)>]
//  member val groups = 0 with get,set
//  [<Params (10000)>]
//  member val policies = 0 with get,set
//  [<Params ("RealisticDep")>]
//  member val depKind = "RealisticDep" with get,set

//  member self.RetrieveManage(assemblyName : string) =
//    let assemblyPath =
//      System.IO.Path.GetFullPath <|
//      sprintf ".\\%s\\%s.dll"
//        Settings.CompilationOutputDirectory
//        assemblyName
//    let assembly = Assembly.LoadFile assemblyPath
//    let name = assembly.Modules.First()
//    let ifunQual = typeof<Stubs.IFunLambda>.AssemblyQualifiedName
//    let projoutQual = typeof<Stubs.ProjectionOutput>.AssemblyQualifiedName
//    let execName = sprintf "%s.GenExecutor`2[[%s],[%s]]" assemblyName ifunQual projoutQual
//    let execClass = name.GetType(execName, true, true)
//    let sim = new Simulator()
//    let statehandler = sim.GetStateHandler(assembly)
//    let customStateValue, result = setupGenExecutor statehandler self.groups self.policies self.depKind PolicyTypes.OneAndThree
//    let projNumber = 0 :> obj
//    {
//      classObject = assembly.CreateInstance(execName);
//      info = execClass.GetMethod("Update");
//      parameters = [|result; customStateValue; projNumber|]
//    }

//  [<GlobalSetup>]
//  member self.Setup() =
//    self.defaultManage   <- self.RetrieveManage Settings.BenchmarkAssemblyDefault
//    self.vectorManage    <- self.RetrieveManage Settings.BenchmarkAssemblyVectorized
//    //self.parallelManage  <- self.RetrieveManage Settings.BenchmarkAssemblyParallel
//    self.deforestManage  <- self.RetrieveManage Settings.BenchmarkAssemblyDeforest


//  //[<Benchmark>]
//  //member self.Parallel () = invokeManage self.parallelManage
//  [<Benchmark(Baseline = true)>]
//  member self.Default () = invokeManage self.defaultManage
//  [<Benchmark>]
//  member self.Vectorized () = invokeManage self.vectorManage
//  [<Benchmark>]
//  member self.Deforested () = invokeManage self.deforestManage



//[<  MemoryDiagnoser
//>]
//[<SimpleJob(launchCount = 1, warmupCount = 0, targetCount = 2)>]
//type BenchFinalize () =
//  member val defaultFinalize = id with get,set
//  member val edlundFinalize  = id with get,set
//  member val input           = null with get,set

//  [<Params (1)>]
//  member val groups = 0 with get,set
//  [<Params (1000)>] //
//  member val policies = 0 with get,set
//  [<Params ("MinimalDep")>]
//  member val depKind = "RealisticDep" with get,set
//  [<Params ( PolicyTypes.OneAndThree)>] //, "3-state", "1&3-state"
//  member val states =  PolicyTypes.One with get,set

//  member self.GetFullFinalizeMethod(statehandler : IProjectionStateHandler<IFunLambda,ProjectionOutput, 'ProjResult>) =
//    let struct (configuration, input) = setup self.groups self.policies self.states
//    let mutable dependencies = Simulator.GetDependencies("MinimalDep")
//    if dependencies :? ProjectionStateHandlerDependencies
//    then
//      let dep = dependencies :?> ProjectionStateHandlerDependencies
//      dep._stateFactory.Input <- input
//    else if dependencies :? DummyStateHandlerDependencies
//    then
//      let dep = dependencies :?> DummyStateHandlerDependencies
//      dep._stateFactory.Input <- input

//    statehandler.SetupDependencies(dependencies)

//    let calculationTimes : IReadOnlyList<float> ref = ref <| (new List<float>() :> IReadOnlyList<float>)
//    let mutable state = statehandler.Initialize(input, configuration, calculationTimes)
//    let mutable t0 = 0.0
//    Seq.iter
//      (fun t ->
//        let res = ProjectionResult.FakeProjection(dependencies.FunctionFactory, state, input, t0, t)
//        let r = statehandler.PreUpdate(res, state)
//        let r = statehandler.Update(state, r)
//        t0 <- t
//      ) (calculationTimes.Value)

//    fun () ->
//      statehandler.Finalize(state) |> ignore

//  member self.MalFinalize(assemblyName : string) =
//    let assemblyPath =
//      System.IO.Path.GetFullPath <|
//      sprintf ".\\%s\\%s.dll"
//        Settings.CompilationOutputDirectory
//        assemblyName
//    let assembly = loadAssembly assemblyPath
//    let sim = new Simulator()
//    let statehandler = sim.GetStateHandler(assembly) //new BenchmarkDeforest.FMAExampleAdapter<IFunLambda, ProjectionOutput>() //
//    self.GetFullFinalizeMethod(statehandler)

//  [<GlobalSetup>]
//  member self.Setup() =
//    self.defaultFinalize <- self.MalFinalize(Settings.BenchmarkAssemblyDefault)
//    self.edlundFinalize <- self.GetFullFinalizeMethod(new ExampleProjectionStateHandler<IFunLambda, ProjectionOutput>())


//  [<Benchmark>]
//  member self.Default() = self.defaultFinalize()

//  [<Benchmark(Baseline = true)>]
//  member self.Edlund() = self.edlundFinalize()

//[<  MemoryDiagnoser
//>]
//[<SimpleJob(launchCount = 2, warmupCount = 8, targetCount = 16)>]
//type FullBench () =
//  member val edlundUpdate   = id with get,set
//  member val defaultUpdate  = id with get,set
//  member val vectorUpdate   = id with get,set
//  member val deforestUpdate = id with get,set
//  member val state          = null with get,set

//  member val defaultAssembly = null with get,set
//  member val vectorAssembly   = null with get,set
//  member val deforestAssembly = null with get,set

//  [<Params (1)>]
//  member val groups = 0 with get,set
//  [<Params (1000,2000,3000,4000,5000,6000,7000,8000,9000,10000)>] //
//  //[<Params (1000,5000,10000)>] //
//  member val policies = 0 with get,set
//  [<Params ("MinimalDep")>]
//  member val depKind = "RealisticDep" with get,set
//  [<Params ( PolicyTypes.One)>] //, "3-state", "1&3-state"
//  member val states =  PolicyTypes.OneAndThree with get,set

//  member val input = null with get,set
//  member val dependencies = null with get,set
//  member val configuration = null with get,set

//  member self.GetFullProjectionMethod<'T>(gen : unit -> IProjectionStateHandler<IFunLambda, ProjectionOutput, 'T>) =
//    fun () ->
//      let statehandler = gen()
//      let calcPeriodEndTimes = ref null

//      statehandler.SetupDependencies(self.dependencies)
//      let mutable state = statehandler.Initialize(self.input, self.configuration, calcPeriodEndTimes)
//      let mutable t0 = 0.0
//      let mutable res = null
//      Seq.iter
//        (fun t ->
//          res <- ProjectionResult.FakeProjection(self.dependencies.FunctionFactory, state, self.input, t0, t)
//          let r = statehandler.PreUpdate(res, state)
//          state <- statehandler.Update(state, r)
//          t0 <- t
//        ) (calcPeriodEndTimes.Value)
//      statehandler.Finalize(state) |> ignore

//  member self.EdlundManage() = self.GetFullProjectionMethod(fun () -> new ExampleProjectionStateHandler<IFunLambda, ProjectionOutput>() :> IProjectionStateHandler<IFunLambda, ProjectionOutput, 'T>)

//  member self.loadAssembly assemblyName =
//    let assemblyPath =
//      System.IO.Path.GetFullPath <|
//      sprintf ".\\%s\\%s.dll"
//        Settings.CompilationOutputDirectory
//        assemblyName
//    loadAssembly assemblyPath

//  member self.MalManage(assembly) =
//    let gen () =
//      let sim = new Simulator()
//      sim.GetStateHandler(assembly)
//    self.GetFullProjectionMethod(gen)

//  [<GlobalSetup>]
//  member self.Setup() =
//    let input, dependencies, configuration = setupStateForFull self.groups self.policies self.depKind self.states

//    self.input <- input
//    self.dependencies  <- dependencies
//    self.configuration <- configuration

//    self.defaultAssembly <- self.loadAssembly (Settings.BenchmarkAssemblyDefault)
//    self.vectorAssembly <- self.loadAssembly(Settings.BenchmarkAssemblyVectorized)
//    self.deforestAssembly <- self.loadAssembly(Settings.BenchmarkAssemblyDeforest)

//    self.edlundUpdate <- self.EdlundManage()
//    self.defaultUpdate <- self.MalManage(self.defaultAssembly)
//    self.vectorUpdate <- self.MalManage(self.vectorAssembly)
//    self.deforestUpdate <- self.MalManage(self.deforestAssembly)

//  [<Benchmark>]
//  member self.Deforest() = self.deforestUpdate()

//  [<Benchmark(Baseline = true)>]
//  member self.Edlund() = self.edlundUpdate()
//  [<Benchmark>]
//  member self.Vector() = self.vectorUpdate()
//  [<Benchmark>]
//  member self.Default() = self.defaultUpdate()
