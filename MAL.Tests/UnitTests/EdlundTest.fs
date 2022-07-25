module EdlundTest


//open Microsoft.VisualStudio.TestTools.UnitTesting
//open itu.dk.MAL
//open Bridge
//open TestUtil
//open itu.dk.MAL.SyntaxUtil
//open FSharp.Configuration
//open System.IO
//open System
//open Actulus.StateHandlers
//open ActulusServices.Shared.Projections
//open Stubs
//open ActulusServices.Shared.Projections.Debugging

//type Settings = AppSettings<"app.config">

//let sessionLock = new Object()

//let compileStatehandler options path filename =
//  lock sessionLock
//    (fun () ->
//      let prog = MALInterface.MalFromFile path filename
//      if (Array.length prog.ParseErrors) > 0
//      then failwith (sprintf "%A" prog.ParseErrors)
//      else
//        prog.Compile options
//        Array.iter (fun err -> printfn "%s" err) prog.CompileError
//        prog.CompileError.Length = 0
//    )

//[<TestClass>]
//type GeneratorTest() =
//  let simulator = new Simulator()
//  let config = createBenchmarkConfig (0,10,1,1,0,3,50)

//  let path_adjust = "../../../"
//  let oldDir = Directory.GetCurrentDirectory()

//  [<TestInitialize>]
//  member __.Initialize () =
//    Directory.SetCurrentDirectory(path_adjust)

//  [<TestCleanup>]
//  member __.CleanUp () =
//    Directory.SetCurrentDirectory(oldDir)

//  [<TestMethod>]
//  member __.ExampleMinimalStateHandler_runs () =
//    let inputGen = new InputGenerator(config);
//    ignore <| simulator.Benchmark(
//      Func<IProjectionStateHandler<IFunLambda, ProjectionOutput, ExampleMinimalStateHandler<IFunLambda, ProjectionOutput>.PeriodResult>>
//        (fun () -> new ExampleMinimalStateHandler<IFunLambda, ProjectionOutput>() :> (IProjectionStateHandler<IFunLambda, ProjectionOutput, ExampleMinimalStateHandler<IFunLambda, ProjectionOutput>.PeriodResult>))
//      , inputGen
//      , config)

//  [<TestMethod>]
//  member __.ExampleProjectionStateHandler_runs () =
//    let inputGen = new InputGenerator(config);
//    ignore <| simulator.Benchmark(
//      Func<IProjectionStateHandler<IFunLambda, ProjectionOutput, ExampleProjectionStateHandler<IFunLambda, ProjectionOutput>.PeriodResult>>
//        (fun () -> new ExampleProjectionStateHandler<IFunLambda, ProjectionOutput>() :> (IProjectionStateHandler<IFunLambda, ProjectionOutput, ExampleProjectionStateHandler<IFunLambda, ProjectionOutput>.PeriodResult>))
//      , inputGen
//      , config)


//  //[<TestMethod>]
//  //member __.ExampleMinimalStateHandler_EdlundTest () =

//  [<TestMethod>]
//  member __.ExampleProjectionStateHandler_EdlundTest () =
//    let a = new ExampleProjectionStateHandler<IFunction, ProjectionOutputForDebugging>() :> (IProjectionStateHandler<IFunction, ProjectionOutputForDebugging, ExampleProjectionStateHandler<IFunction, ProjectionOutputForDebugging>.PeriodResult>)
//    simulator.EdlundTest<ExampleProjectionStateHandler<IFunction, ProjectionOutputForDebugging>.PeriodResult>(a)


//  [<TestMethod>]
//  member __.FMAGEN_EdlundTest () = ()
//    //let a = new ShNormal.FMAExampleAdapter<IFunction, ProjectionOutputForDebugging>()
//    //simulator.EdlundTest<IProjectionReservesResult>(a)
