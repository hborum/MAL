module InterpreterTest

//open Microsoft.VisualStudio.TestTools.UnitTesting
//open itu.dk.MAL
//open AST
//open Bridge
//open Stubs
//open TestUtil

//let noExceptions (program : string) =
//  let mal = MALInterface.MAL ("", TestDataDef.dataDefinition + program)
//  if (Array.length mal.ParseErrors) > 0
//  then Array.iter
//         (fun pA ->
//            printfn "%s" pA.message
//         ) mal.ParseErrors
//       false
//  else
//    let errs = mal.Analyser.Errors
//    if List.length errs > 0
//    then
//      printfn "Following unexpected errors:"
//      List.iter (fun (err, (posBegin,_)) -> mal.PrintErr err (posBegin)) errs
//      false
//    else
//      let benchmarkConfig = createBenchmarkConfig(42,10,1,1,0,1,10)
//      let inputGen = new InputGenerator(benchmarkConfig)
//      let struct (projConfig,input) = inputGen.CreateInput(10, PolicyTypes.OneAndThree)
//      match Interpreter.Interpreter.interpreterFromMAL mal with
//      | None -> failwith "Could not instantiate Interpreter"
//      | Some interpreter ->
//        interpreter.SetDependencies (new ProjectionStateHandlerDependencies())
//        interpreter.InitializeValueStructure (input, true)
//        interpreter.PrepareInit
//        interpreter.Init ()
//        true

//[<TestClass>]
//type InterpreterDoesNotFail () =
//  //[<TestMethod>]
//  member __.map () =
//    let prog =
//      "
//      init
//        let a =
//          map grp in Groups
//        	with grp.Reserve
//        	end
//      end
//      "
//    Assert.IsTrue(noExceptions prog)

//  //[<TestMethod>]
//  member __.map2 () =
//    let prog =
//      "
//      init
//        let a =
//          map grp in Groups
//          map grp1, grp2 in Groups,Groups
//          where grp.Reserve > 0.3
//        	with grp.Reserve + grp1.Reserve + grp2.Reserve
//        	end
//      end
//      "
//    Assert.IsTrue(noExceptions prog)

//  //[<TestMethod>]
//  member __.update () =
//    let prog =
//      "
//      init
//      	update cashFlow in CashFlows
//      	with
//      		cashFlow.Transfers = cashFlow.Input.Transfers
//      	end
//      end
//      "
//    Assert.IsTrue(noExceptions prog)
