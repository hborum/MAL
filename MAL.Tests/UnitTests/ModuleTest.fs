module ModuleTest

open itu.dk.MAL
open AnalyserTypes
open Analysis
open Microsoft.VisualStudio.TestTools.UnitTesting
open itu.dk.MAL.SyntaxUtil
open FSharp.Configuration

type Settings = AppSettings<"app.config">

[<TestClass>]
type TypeErrors () =

  let path_adjust = "../../../"

  let options : CompilerOptions = {
    parallelize = false
    ; inln = false
    ; vectorize = false
    ; deforest = false
    ; outPath = path_adjust+Settings.CompilationOutputDirectory
    ; outName = "test"
    ; optLevel = OptimizationLevel.ToDebug
  }

  [<TestMethod>]
  member __.MultipleDefinitions1 () =
    let program = MALInterface.MalFromFile (path_adjust + "examples/moduleTests/") "main1.fmal"

    if (Array.length program.ParseErrors) > 0
    then failwith (sprintf "%A" program.ParseErrors)
    else
      program.Compile options
      Array.iter (fun err -> printfn "%s" err) program.CompileError
      Assert.IsTrue(program.CompileError.Length > 0)


  [<TestMethod>]
  member __.MultipleDefinitions2 () =
    let program = MALInterface.MalFromFile (path_adjust + "examples/moduleTests/") "main2.fmal"

    if (Array.length program.ParseErrors) > 0
    then failwith (sprintf "%A" program.ParseErrors)
    else
      program.Compile options
      Array.iter (fun err -> printfn "%s" err) program.CompileError
      Assert.IsTrue(program.CompileError.Length > 0)


  [<TestMethod>]
  member __.SingleModuleNestedData1 () =
    let program = MALInterface.MalFromFile (path_adjust + "examples/moduleTests/") "main3.fmal"

    if (Array.length program.ParseErrors) > 0
    then failwith (sprintf "%A" program.ParseErrors)
    else
      program.Compile options
      Array.iter (fun err -> printfn "%s" err) program.CompileError
      Assert.IsTrue(program.CompileError.Length = 0)

  [<TestMethod>]
  member __.SingleModuleNestedData2 () =
    let program = MALInterface.MalFromFile (path_adjust + "examples/moduleTests/") "main4.fmal"

    if (Array.length program.ParseErrors) > 0
    then failwith (sprintf "%A" program.ParseErrors)
    else
      program.Compile options
      Array.iter (fun err -> printfn "%s" err) program.CompileError
      Assert.IsTrue(program.CompileError.Length = 0)
