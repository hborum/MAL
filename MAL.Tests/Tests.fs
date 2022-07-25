open itu.dk.MAL
open TestUtil
open Argu
open System
open BenchmarkDotNet
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Diagnostics.Windows
open itu.dk.MAL.SyntaxUtil
open FSharp.Configuration
open System.Linq

type Settings = AppSettings<"app.config">

let stateHandlerPath = sprintf "%s\\%s" Environment.CurrentDirectory "examples/passive.fmal"

let mutable hasComp = false
let compileStatehandler options path fileName =
  if hasComp then ()
  else
    let prog = MALInterface.MalFromFile path fileName
    if (Array.length prog.ParseErrors) > 0
    then failwith (sprintf "%A" prog.ParseErrors)
    else
      prog.Compile options
      Array.iter (fun err -> printfn "%s" err) prog.CompileError
      hasComp <- false
      ()

//let printSystemInfo () =
//  SystemInfo.printProcessorInfo()
//  SystemInfo.printRam()
//  SystemInfo.printOperatingSystemInfo()

//let benchmark (steps,runs,iterPerRun,warmups,groups,policies) =
//  let simulator = new Simulator()
//  let config = createBenchmarkConfig (0,steps,runs,iterPerRun,warmups,groups,policies)
//  simulator.RunBenchmark(config)

//let simulate (steps,groups,policies) path assembly =
//  let simulator = new Simulator()
//  let config = createBenchmarkConfig (0,steps,1,1,0,groups,policies)
//  simulator.SimulateCSV(config, path, "examples/excelInput", assembly)

type Arguments =
  | Benchmark of filename:string * profile:bool
  | Simulate of steps:int * groups:int * policies:int * path:string * assembly:string
  | Compile of path:string * name:string * SIMD:bool
  | Run     of path:string * result:string
  | Debug of steps:int * groups:int * policies:int * loops:int * startseed:int
  | SystemInfo
  | Test
with
  interface IArgParserTemplate with
      member s.Usage =
          match s with
          | Debug _ -> ""
          | Benchmark _ -> "Benchmark the implementation."
          | Simulate _ -> "Run a single simulation and output result."
          | Compile _ -> "Compiles path."
          | SystemInfo -> "Prints system information"
          | Test -> "Run the test-suite on project."
          | Run _ -> "Compiles and simulates a projection"

[<EntryPoint>]
let main argv =
  System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo.InvariantCulture
  let parser = ArgumentParser.Create<Arguments>(programName = "MAL")
  let args = parser.Parse(argv).GetAllResults()
  let mutable notFailed = true
  if args.Length = 0
  then
    //compileStatehandler {parallelize = false; inln = false ; vectorize = false; deforest = true; outPath = "out/" ; outName = "ShNormal" ; optLevel = OptimizationLevel.ToRelease} "examples/" "statehandler.fmal"
    //printfn "Compile complete"
    //compileStatehandler {parallelize = false; inln = false ; vectorize = false; deforest = false; outPath = "out/" ; outName = "ShNormal" ; optLevel = OptimizationLevel.ToDebug } "examples/" "converttest.fmal"
    //let sim = new Simulator();
    //let c = new BenchmarkConfig();
    //let loopLimit = 0
    //let startSeed = 1
    //c.Seed <- startSeed - 1
    //while notFailed do
    //  c.Seed <- c.Seed + 1
    //  c.Steps <- 100
    //  c.Groups <- 1
    //  c.Policies <- 20000
    //  c.Runs <- 1
    //  c.IterPerRun <- 1

    //  let config = (new Configs.ManualConfig())
    //                .AddValidator(Validators.JitOptimizationsValidator.DontFailOnError)
    //                .AddLogger(DefaultConfig.Instance.GetLoggers().ToArray())
    //                .AddExporter(DefaultConfig.Instance.GetExporters().ToArray())
    //                .WithOptions(ConfigOptions.DisableOptimizationsValidator)
    //                .AddColumnProvider(DefaultConfig.Instance.GetColumnProviders().ToArray()
    //                )
    //  let config = if false then config.AddDiagnoser(new EtwProfiler()) else config
    //  sim.StateHandlerEquivalence_Edlund(c, "out/", "ShNormal")
    //  //ignore <| Running.BenchmarkRunner.Run<Benchmarks.FullBench>(config)
    //  notFailed <- false
      //printfn "%b" res
      //if res && c.Seed < startSeed + loopLimit //&&
      //then printfn "%d" c.Seed
      //else notFailed <- false
      //     printfn "%d" c.Seed
    ()
  else
    List.iter
      (
        fun arg ->
          match arg with
          | Benchmark (filename, shouldProfile) ->
            let config = (new Configs.ManualConfig())
                          .AddValidator(Validators.JitOptimizationsValidator.DontFailOnError)
                          .AddLogger(DefaultConfig.Instance.GetLoggers().ToArray())
                          .AddExporter(DefaultConfig.Instance.GetExporters().ToArray())
                          .WithOptions(ConfigOptions.DisableOptimizationsValidator)
                          .AddColumnProvider(DefaultConfig.Instance.GetColumnProviders().ToArray()
                          )
            let config = if shouldProfile then config.AddDiagnoser(new EtwProfiler()) else config
            #if FMALSTATIC
            ignore <| Running.BenchmarkRunner.Run<Benchmarks.StaticBench>(config)
            #else
            let outFolder = sprintf "%s/" Settings.CompilationOutputDirectory
            compileStatehandler {parallelize = false; inln = false ; vectorize = false; deforest = false; outPath = outFolder ; outName = Settings.BenchmarkAssemblyDefault    ; optLevel = OptimizationLevel.ToRelease } "examples/" filename
            compileStatehandler {parallelize = false; inln = false ; vectorize = true ; deforest = true; outPath = outFolder ; outName = Settings.BenchmarkAssemblyVectorized ; optLevel = OptimizationLevel.ToRelease } "examples/" filename
            compileStatehandler {parallelize = false; inln = false ; vectorize = false; deforest = true ; outPath = outFolder ; outName = Settings.BenchmarkAssemblyDeforest   ; optLevel = OptimizationLevel.ToRelease } "examples/" filename
            //compileStatehandler {parallelize = true ; inln = false ; vectorize = false; deforest = false; outPath = outFolder ; outName = Settings.BenchmarkAssemblyParallel   ; optLevel = OptimizationLevel.ToRelease } "examples/" filename
            //ignore <| Running.BenchmarkRunner.Run<Benchmarks.FullBench>(config)
            #endif
          | Test -> printfn "Test has been disabled"
          | Simulate(steps, groups, policies, path, assembly) -> () //simulate (steps, groups, policies) path assembly
          | Compile (path,out,simd) ->
            compileStatehandler {parallelize = false; inln = false ; deforest = true; vectorize = simd; outPath = "out/" ; outName = out ; optLevel = OptimizationLevel.ToRelease } "examples/" path
          | SystemInfo -> () //printSystemInfo ()
          | Run (path, resName) ->
              compileStatehandler {parallelize = false; inln = false ; vectorize = false; deforest = false; outPath = "out/" ; outName = "minimal" ; optLevel = OptimizationLevel.ToDebug } "" path
              () //simulate (0, 0, 0) path "minimal"
          | Debug (steps, groups, policies, loops, startseed) -> ()
            //let sim = new Simulator();
            //let c = new BenchmarkConfig();
            //let loopLimit = loops
            //let startSeed = startseed
            //c.Seed <- startSeed - 1
            //while notFailed do
            //  c.Seed <- c.Seed + 1
            //  c.Steps <- steps
            //  c.Policies <- policies
            //  c.Groups <- groups
            //  c.Runs <- 1
            //  c.IterPerRun <- 1
            //  if sim.StateHandlerEquivalence_Edlund(c, "examples/excelInput", "out/", "") //&&
            //     //sim.StateHandlerEquivalence_Edlund(c,"out/","ShNormal") && c.Seed < startSeed + loopLimit
            //  then printfn "%d" c.Seed
            //  else notFailed <- false
            //       printfn "%d" c.Seed

      ) <| args
  0