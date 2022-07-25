namespace itu.dk.MAL

open AST

module MALInterface =
  open Analysis
  let toMap dictionary = GeneralUtil.toMap dictionary

  type MAL (path, source) =
    let parseResult =
      match Parser.doParseSource path source with
      | Some prog -> prog
      | None -> failwith "error parsing"

    let parseErrors = List.toArray <| ParserValidation.parseValidateProgram parseResult
    let mutable compileError = [||];
    let mutable analyser = None

    /// <summary>
    /// The source of the program
    /// </summary>
    member __.Source = parseResult.source

    /// <summary>
    /// The AST obtained by parsing the source
    /// </summary>
    member __.Program = parseResult

    /// <summary>
    /// Prints an error message pointing to the position of errPos
    /// </summary>
    /// <param name="err">The error message object (only used for printing with "%A").</param>
    /// <param name="errPos">The position of te error</param>
    member __.PrintErr err errPos = Printer.printErr parseResult.source err errPos

    /// <summary>
    /// An array containing all parsing errors
    /// </summary>
    member __.ParseErrors : ErrorMessage array = parseErrors

    /// <summary>
    /// The AST obtained by parsing the source
    /// </summary>
    member this.Analyser =
      match analyser with
      | None ->
        let ret = Analyser parseResult
        analyser <- Some <| ret
        ret
      | Some ret -> ret

    /// <summary>
    /// An array containing all type errors
    /// </summary>
    /// note: Todo cache type errors
    member this.TypeErrors() = this.Analyser.Errors

    /// <summary>
    /// (WIP) After compilation `CompileError` contains errors encountered during compilation
    /// </summary>
    member __.CompileError = compileError

    /// <summary>
    /// Compiles the program using the options parameters.
    /// Options specifies a compilation configuration by the tuple/record: (parallize : bool, inline : bool, outFolder : string, outName : string).<para />
    /// `parallelize` and `inline` should be `false` in non-experimental usage.
    /// </summary>
    /// <param name="options">Specifies a compilation configuration with the tuple/record: (parallize : bool, inline : bool, outFolder : string, outName : string).
    /// `parallelize` and `inline` should be `false` in non-experimental usage.
    /// </param>
    member this.Compile options =
        if this.ParseErrors.Length > 0
        then compileError <- [|"Could not compile due to parse errors. Look up in MAL.ParseErrors"|]
        else
          let typErrs = this.Analyser.Errors
          if typErrs.Length > 0
          then
            compileError <-
              typErrs |> Seq.map (fun err -> sprintf "Could not compile. Type Error:\n%A" err)
                      |> Seq.toArray
          else
            compileError <- [| |]
          //try
            match Compiler.compile this.Analyser options with
            | None -> ()
            | Some err -> compileError <- Array.ofSeq err

          //with
          //  | e ->
          //      compileError <- [|sprintf "Internal error: %s" e.Message|]

  let MalFromFile path file =
    MAL(path, System.IO.File.ReadAllText <| path+file)