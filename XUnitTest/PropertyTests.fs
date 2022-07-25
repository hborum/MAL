module Tests

open System
open itu.dk.MAL
open AST
open Structure
open FsCheck.Xunit
open FsCheck
open Analysis

let private parseString = Parser.doParseSource "../../../../examples/" >> Option.get
let minimalProgram =
  let reader = new IO.StreamReader("../../../../examples/minimal.fmal")
  let prog = reader.ReadToEnd()
  reader.Close()
  prog
let printDecs (prog : program<'eA, 'sA>) = Printer.printASTWithNoInfo <| snd prog.modul
let printDecsWithTypes (prog : program<typ<'sA2>, 'sA>) = Printer.printASTWithTypeInfo <| snd prog.modul

[<Property>]
let printParseProgramGetsOriginal (prog : program<typ<Position>, Position>) =
  let original = printDecs prog
  let parsed = original |> parseString |> printDecs
  if original = parsed then true else let _ = printfn "Original:\n%s Parsed:\n%s" original parsed in false

[<Property>]
let printParseTypeGetsSameTypes (prog : program<typ<Position>, Position>) =
  let original = printDecs prog
  let originalWithTypes = printDecsWithTypes prog
  let analyser = Analyser(original |> parseString)
  let typedProgram = analyser.TypedProgram |> fst |> snd |> Printer.printASTWithTypeInfo
  if originalWithTypes = typedProgram then true else let _ = printfn "Typed:\n%s" typedProgram in false

[<Property>]
let compilationTerminatesWithoutError (prog : program<typ<Position>, Position>) =
  let untyped = prog |> printDecs |> parseString
  let analyser = Analyser(untyped)
  let compilerOpts = {SyntaxUtil.CompilerOptions.Default with outPath = "..\\..\\..\\..\\out"}
  Compiler.compile analyser compilerOpts |> Option.isNone

[<Property>]
let intentionalTypeErrorsAreDetected (errors : AnalyserTypes.typErr list, prog : program<typ<Position>, Position>) =
  let untyped = prog |> printDecs |> parseString
  let analyser = Analyser(untyped)
  let foundErrors = List.map fst analyser.Errors
  let expectedErrors = List.map fst errors
  let isDetected err = List.contains err foundErrors
  let isExpected err = List.contains err expectedErrors
  let allErrorsAreDetected = List.forall isDetected expectedErrors
  let noUnexpectedErrors = List.forall isExpected foundErrors
  let propHolds = allErrorsAreDetected && noUnexpectedErrors
  if propHolds then () else printfn "%A" foundErrors
  propHolds

type MalProperties = class end