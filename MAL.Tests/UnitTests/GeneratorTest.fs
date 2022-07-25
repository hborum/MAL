module GeneratorTest

//open Microsoft.VisualStudio.TestTools.UnitTesting
//open itu.dk.MAL
//open Bridge
//open TestUtil
//open itu.dk.MAL.SyntaxUtil
//open FSharp.Configuration
//open System.IO
//open System
//open Stubs
//open Actulus.StateHandlers
//open ActulusServices.Shared.Projections

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
//  let config = createBenchmarkConfig (0,1,1,1,0,1,2)

//  let path_adjust = "../../../"
//  let path_statehandler = "statehandler.fmal"
//  let path_statehandler_noParam = "statehandler-noParam.fmal"
//  let path_passive = "passive.fmal"
//  let path_minimal = "minimal.fmal"
//  let path_compTest = "compilertest.fmal"
//  let path_asserter = "asserter.fmal"

//  let shNameGen name =
//    sprintf "%sNormal" name, sprintf "%sVector" name, sprintf "%sDeforest" name


//  let sh_normal, sh_vec, sh_deforest = shNameGen "Sh"
//  let sh_np_normal, sh_np_vec, sh_np_deforest = shNameGen "ShNP"
//  let passive_normal, passive_vec, passive_deforest = shNameGen "Passive"
//  let minimal_normal, minimal_vec, minimal_deforest = shNameGen "Minimal"
//  let compTest_normal, compTest_vec, compTest_deforest = shNameGen "CompTest"
//  let asserter_normal, asserter_vec, asserter_deforest = shNameGen "Asserter"


//  let path = Settings.CompilationOutputDirectory

//  let compile path assemblyName vector deforest =
//    compileStatehandler {
//      parallelize = false
//      ; inln = false
//      ; vectorize = vector
//      ; deforest = deforest
//      ; outPath = Settings.CompilationOutputDirectory
//      ; outName = assemblyName
//      ; optLevel = OptimizationLevel.ToDebug
//    } "examples/" path

//  let oldDir = Directory.GetCurrentDirectory()

//  let testVectorEquivalence mal_path name1 name2 =
//    Assert.IsTrue <| compile mal_path name1 false false
//    Assert.IsTrue <| compile mal_path name2 true false
//    Assert.IsTrue <| simulator.StateHandlerEquivalence(config, path, name1, name2)

//  let testDeforestEquivalence mal_path name1 name2 =
//    Assert.IsTrue <| compile mal_path name1 false false
//    Assert.IsTrue <| compile mal_path name2 false true
//    Assert.IsTrue <| simulator.StateHandlerEquivalence(config, path, name1, name2)

//  let edlundTest mal_path name1 name2 =
//    Assert.IsTrue <| compile mal_path name1 false false
//    //Assert.IsTrue <| compile mal_path name2 true false
//    simulator.EdlundTest(simulator.GetStateHandler("out",name1))
//    //simulator.EdlundTest(path, name2)

//  [<TestInitialize>]
//  member __.Initialize () =
//    Directory.SetCurrentDirectory(path_adjust)

//  [<TestCleanup>]
//  member __.CleanUp () =
//    Directory.SetCurrentDirectory(oldDir)

//  // Test can not pass in its current state
//  // we could create a version which allowed a small difference when comparing floats
//  //[<TestMethod>]
//  member __.EdlEq_ProjectionStatehandler () =
//    Assert.IsTrue <| compile path_statehandler sh_normal false false
//    Assert.IsTrue <| simulator.StateHandlerEquivalence_Edlund(config, path, sh_normal)


//  [<TestMethod>]
//  member __.EdlEq_Minimal () =
//    Assert.IsTrue <| compile path_minimal minimal_normal false false
//    let inputGen = new InputGenerator(config);
//    let res =
//      simulator.CompareStateHandlers(config
//          , inputGen
//          , fun () -> simulator.GetStateHandler<IFunLambda, ProjectionOutput>(path, minimal_normal)
//          , fun () -> new ExampleMinimalStateHandler<IFunLambda, ProjectionOutput>() :> (IProjectionStateHandler<IFunLambda, ProjectionOutput, ExampleMinimalStateHandler<IFunLambda, ProjectionOutput>.PeriodResult>)
//          );
//    Assert.IsTrue <| res

//  //[<TestMethod>]
//  member __.VecEq_StateHandler () =
//    testVectorEquivalence path_statehandler sh_normal sh_vec

//  [<TestMethod>]
//  member __.VecEq_Passive () =
//    testVectorEquivalence path_passive passive_normal passive_vec

//  [<TestMethod>]
//  member __.VecEq_Minimal () =
//    testVectorEquivalence path_minimal minimal_normal minimal_vec

//  // This test fails but should not
//  [<TestMethod>]
//  member __.VecEq_CompTest () =
//    testVectorEquivalence path_compTest compTest_normal compTest_vec

//  //[<TestMethod>]
//  member __.DeforestEq_StateHandler () =
//    testDeforestEquivalence path_statehandler sh_normal sh_deforest

//  [<TestMethod>]
//  member __.DeforestEq_Passive () =
//    testDeforestEquivalence path_passive passive_normal passive_deforest

//  [<TestMethod>]
//  member __.DeforestEq_Minimal () =
//    testDeforestEquivalence path_minimal minimal_normal minimal_deforest

//  [<TestMethod>]
//  member __.DeforestEq_CompTest () =
//    testDeforestEquivalence path_compTest compTest_normal compTest_deforest

//  //[<TestMethod>]
//  member __.EdlundTest_Passive() = edlundTest path_passive passive_normal passive_vec

//  [<TestMethod>]
//  member __.EdlundTest_StateHandler () = edlundTest path_statehandler sh_normal sh_vec

//  [<TestMethod>]
//  member __.EdlundTest_Minimal () =
//    edlundTest path_minimal minimal_normal minimal_vec

//  //[<TestMethod>]
//  member __.EdlundTest_Statehandler_noParam () = edlundTest path_statehandler_noParam sh_np_normal sh_np_vec

//  [<TestMethod>]
//  member __.AssertTest () =
//    Assert.IsTrue <| compile path_asserter asserter_normal false false
//    simulator.SimulateCSV(config, path, "examples/excelInputTest", asserter_normal)
