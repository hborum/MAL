module InitTest

open itu.dk.MAL
open Analysis
open AST
open Constants
open InitCheck
open Microsoft.VisualStudio.TestTools.UnitTesting

//[<TestClass>]
//type TypeErrors () =

//  let path_adjust = "../../../"
//  let initialEnv = ["Policies", TotalCollection [tag_policy] ; "Groups", TotalCollection [tag_group] ;
//                    "Equities", TotalCollection [tag_equity] ; "CashFlow", TotalCollection [tag_cashFlow] ;
//                    "Global", TotalRecord [tag_global]]

//  [<TestMethod>]
//  member __.FirstTest () =

//    let program = MALInterface.MalFromFile (path_adjust + "examples/initTests/") "first.fmal"

//    let analyser = Analyser program.Program
//    let initClosures = analyser.Closures.initFuns
//    let filterResult = InitCheck.CheckProgram program.Program initClosures analyser.DataStructureMap

//    let test = Map.forall (fun t -> (fun m -> Map.isEmpty m)) filterResult
//    Assert.IsTrue(test)

//  [<TestMethod>]
//  member __.LetTest () =

//    let program = MALInterface.MalFromFile (path_adjust + "examples/initTests/") "let.fmal"
//    let analyser = Analyser program.Program
//    let initClosures = analyser.Closures.initFuns
//    let initCheckResult = InitCheck.CollectInitialised (snd program.Program.modul) analyser.DataStructureMap
//    printfn "RESULT: %A" initCheckResult
//    let test = initCheckResult = (initialEnv, [])
//    Assert.IsTrue(test)

//  [<TestMethod>]
//  member __.LetTest2 () =

//    let program = MALInterface.MalFromFile (path_adjust + "examples/initTests/") "let2.fmal"

//    let analyser = Analyser program.Program
//    let initClosures = analyser.Closures.initFuns
//    let initCheckResult = InitCheck.CollectInitialised (snd program.Program.modul) analyser.DataStructureMap

//    printfn "RESULT: %A" initCheckResult
//    let test = initCheckResult = (initialEnv @ [], [])
//    Assert.IsTrue(test)

//  [<TestMethod>]
//  member __.UpdateTest () =

//    let program = MALInterface.MalFromFile (path_adjust + "examples/initTests/") "update.fmal"

//    let analyser = Analyser program.Program
//    let initClosures = analyser.Closures.initFuns
//    let initCheckResult = InitCheck.CollectInitialised (snd program.Program.modul) analyser.DataStructureMap
//    printfn "RESULT: %A" initCheckResult
//    let test = initCheckResult = (initialEnv, ["Global", "ProjectionTimes";"pol", "Reserve"])
//    Assert.IsTrue(test)

//  [<TestMethod>]
//  member __.ProjTest () =

//    let program = MALInterface.MalFromFile (path_adjust + "examples/initTests/") "proj.fmal"

//    let analyser = Analyser program.Program
//    let initClosures = analyser.Closures.initFuns
//    let initCheckResult = InitCheck.CollectInitialised (snd program.Program.modul) analyser.DataStructureMap
//    printfn "RESULT: %A" initCheckResult
//    let test = initCheckResult = (initialEnv,["Global", "ProjectionTimes"])
//    Assert.IsTrue(test)

//  [<TestMethod>]
//  member __.Proj2Test () =

//    let program = MALInterface.MalFromFile (path_adjust + "examples/initTests/") "proj2.fmal"

//    let analyser = Analyser program.Program
//    let initCheckResult = InitCheck.CollectInitialised (snd program.Program.modul) analyser.DataStructureMap
//    printfn "RESULT: %A" initCheckResult
//    let test = initCheckResult = (initialEnv,["Global", "ProjectionTimes"; "Global", "GlobalScalar"])
//    Assert.IsTrue(test)

//  [<TestMethod>]
//  member __.BlockTest () =

//    let program = MALInterface.MalFromFile (path_adjust + "examples/initTests/") "block.fmal"

//    let analyser = Analyser program.Program
//    let initCheckResult = InitCheck.CollectInitialised (snd program.Program.modul) analyser.DataStructureMap
//    printfn "RESULT: %A" initCheckResult
//    let test = initCheckResult = (initialEnv, [])
//    Assert.IsTrue(test)

//  [<TestMethod>]
//  member __.AssTest () =

//    let program = MALInterface.MalFromFile (path_adjust + "examples/initTests/") "assign.fmal"

//    let analyser = Analyser program.Program
//    let initCheckResult = InitCheck.CollectInitialised (snd program.Program.modul) analyser.DataStructureMap
//    printfn "RESULT: %A" initCheckResult
//    let test = initCheckResult = (initialEnv, ["group", "TechnicalExpense";"group", "ExpenseDividends"])
//    Assert.IsTrue(test)

//  [<TestMethod>]
//  member __.FilterTest () =

//    let program = MALInterface.MalFromFile (path_adjust + "examples/initTests/") "assignFilter.fmal"

//    let analyser = Analyser program.Program
//    let initCheckResult = InitCheck.CollectInitialised (snd program.Program.modul) analyser.DataStructureMap
//    printfn "RESULT: %A" initCheckResult
//    let test = initCheckResult = (initialEnv, ["group", "TechnicalExpense";"group", "ExpenseDividends"])
//    Assert.IsTrue(test)

//  [<TestMethod>]
//  member __.ForInTest () =

//    let program = MALInterface.MalFromFile (path_adjust + "examples/initTests/") "forin.fmal"

//    let analyser = Analyser program.Program
//    let initCheckResult = InitCheck.CollectInitialised (snd program.Program.modul) analyser.DataStructureMap
//    printfn "RESULT: %A" initCheckResult
//    let test = initCheckResult = (initialEnv, [])
//    Assert.IsTrue(test)

//  [<TestMethod>]
//  member __.ExampleFilterTest () =
//    let program = MALInterface.MalFromFile (path_adjust + "examples/initTests/") "first.fmal"
//    let analyser = Analyser program.Program
//    let shouldBe = Map.empty.Add("Group",Map.empty.Add("Foo",TBool(5)).Add("Bar",TBool(5))).Add("Interest",Map.empty.Add("Foo",TBool(5)))
//    let hasBeen = ["Group","Foo";"Group","Bar"]
//    let filterResult = InitCheck.FilterInitialisedFields shouldBe hasBeen analyser.DataStructureMap.superTypes
//    let test = Map.forall (fun t -> (fun m -> Map.isEmpty m)) filterResult
//    Assert.IsTrue(test)

//  [<TestMethod>]
//  member __.FirstFilterTest () =
//    let program = MALInterface.MalFromFile (path_adjust + "examples/initTests/") "let.fmal"
//    let analyser = Analyser program.Program
//    let initClosures = analyser.Closures.initFuns
//    let filterResult = InitCheck.CheckProgram program.Program initClosures analyser.DataStructureMap
//    let test = Map.forall (fun t -> (fun m -> Map.isEmpty m)) filterResult
//    Assert.IsTrue(test)

//  [<TestMethod>]
//  member __.LargerFilterTest () =

//    let program = MALInterface.MalFromFile (path_adjust + "examples/initTests/") "initcheckTest.fmal"
//    let analyser = Analyser program.Program
//    let initClosures = analyser.Closures.initFuns
//    let filterResult = InitCheck.CheckProgram program.Program initClosures analyser.DataStructureMap
//    let test = Map.forall (fun t -> (fun m -> Map.isEmpty m)) filterResult
//    Assert.IsTrue(test)

//  [<TestMethod>]
//  member __.PassiveFilterTest () =

//    let program = MALInterface.MalFromFile (path_adjust + "examples/initTests/") "/passive.fmal"
//    let analyser = Analyser program.Program
//    let initClosures = analyser.Closures.initFuns
//    let filterResult = InitCheck.CheckProgram program.Program initClosures analyser.DataStructureMap
//    let test = Map.forall (fun t -> (fun m -> Map.isEmpty m)) filterResult
//    Assert.IsTrue(test)


//  [<TestMethod>]
//  member __.InitLookupTest () =
//    let test = match InitCheck.InitLookup ["group", "Foo"] "group" Map.empty with
//               | None -> false
//               | Some res -> res = "Foo"
//    Assert.IsTrue(test)

//  [<TestMethod>]
//  member __.InitLookup2Test () =
//    let test = match InitCheck.InitLookup2 globalHasBeen "CashFlow" "Name" Map.empty with
//               | None -> false
//               | Some res -> res = "Name"
//    Assert.IsTrue(test)

//  [<TestMethod>]
//  member __.LetFilterTest () =

//    let program = MALInterface.MalFromFile (path_adjust + "examples/initTests/") "let.fmal"

//    let analyser = Analyser program.Program
//    let initClosures = analyser.Closures.initFuns
//    let filterResult = InitCheck.CheckProgram program.Program initClosures analyser.DataStructureMap
//    let test = Map.forall (fun t -> (fun m -> Map.isEmpty m)) filterResult
//    Assert.IsTrue(test)

//  [<TestMethod>]
//  member __.AssFilterTest () =

//    let program = MALInterface.MalFromFile (path_adjust + "examples/initTests/") "assign.fmal"

//    let analyser = Analyser program.Program
//    let initClosures = analyser.Closures.initFuns
//    let filterResult = InitCheck.CheckProgram program.Program initClosures analyser.DataStructureMap
//    let test = Map.forall (fun t -> (fun m -> Map.isEmpty m)) filterResult
//    Assert.IsTrue(test)

