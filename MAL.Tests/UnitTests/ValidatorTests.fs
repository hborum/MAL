module ValidatorTests

open Microsoft.VisualStudio.TestTools.UnitTesting
open itu.dk.MAL
open itu.dk.MAL.AST

let progContainsErr expectedKw prog =
  let prog = MALInterface.MAL ("",prog)
  let errs =
    snd prog.Program.modul |>
    ParserValidation.parseValidateDeclarations
  List.exists (fun (e : ErrorMessage) -> e.message.Contains expectedKw) errs
  |> Assert.IsTrue

[<TestClass>]
type ParserValidator () =

  [<TestMethod>]
  member __.ITEtest () =
    "action init()
    with
      update x in Policies
        x.Reserve = if 1 < 2 thn 0 else 1
      end
    end"
    |> progContainsErr "then"

  [<TestMethod>]
  member __.funTest () =
    "fun g ()
      2 * false"
    |> progContainsErr "="

  [<TestMethod>]
  member __.actTest () =
    "action g (x)
    wth
      x = 2
    end"
    |> progContainsErr "with"