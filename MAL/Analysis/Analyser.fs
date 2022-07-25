namespace itu.dk.MAL

module Analysis =
  open AST
  open AnalyserTypes
  open ExportAnalyser
  open Structure

  type Modes =
    | InitMode
    | ManageMode
    | CombinedMode

  type Analyser<'eA> (program : program<'eA, Position>) =
    let dependencyAnalysis =
      Map.ofSeq <| Seq.map (fun (kind, (sA,nm), dep) -> nm, (sA, kind, Analyser dep)) program.dependencies

    let dependencyErrors =
      Map.toSeq dependencyAnalysis
        |> Seq.choose
            (fun (nm, (sA,_,v)) ->
              match v.Errors with
              | [] -> None
              | _ -> Some (ErrorMsgs.import_module_error nm sA.kwStart sA.kwEnd)
            )
        |> Seq.toList

    let dataImports =
      Map.toSeq dependencyAnalysis
        |> Seq.collect (fun (_, (_,_,v)) -> v.exportDataDecs)
        |> Seq.toList

    let onlyDependAll = Map.filter (fun _ (_,k,_) -> k = DependAll) dependencyAnalysis

    let dependUnionTags = Map.fold (fun unionTags _ (_,_,depAnalyser : Analyser<'eA>) -> Set.union unionTags depAnalyser.UnionTags) Set.empty dependencyAnalysis

    let modulImports =
      Map.toSeq onlyDependAll
        |> Seq.map (fun ((nm), (_,_, v)) -> nm, v.modulTyp)
        |> Seq.toList
    let modulDs =
      Map.map (fun (nm) (sA,_,v : Analyser<'eA>) -> sA,v.DataStructure) onlyDependAll

    let program =
      { program with modul = fst program.modul, dataImports @ (snd program.modul)}

    let dataStructure, inhErr : (dataStructure<Position> * typErr list) = DataStructureAnalyser.getDataInfoProgram program
    let _, contractErrors = checkContract dataStructure modulDs

    let closure = ClosureAnalysis.closureAnalysisDecs (snd program.modul)
    let (initEnv, updateEnv) = Typer.createEnv

    let typedProgram, typeErrs, unionTags =
      Typer.typeModule (modulImports@initEnv) (modulImports@updateEnv) closure dataStructure program.modul

    let allUnionTags = Set.union unionTags dependUnionTags

    member __.exportDataDecs (*: seq<dec<'eA, Position>>*) =
      Seq.choose
        (fun (d (*: dec<'eA, Position>*)) ->
          match d with
          | Export(_,Data(sA,info)) -> Some(Data(sA,info))
          | _ -> None
        ) (snd program.modul)

    member __.modulTyp =
      let a =
        (snd typedProgram)
          |> Seq.choose
              (fun (d) ->
                match d with
                | Export(_,ActDec(_,(_,nm), param, _)) -> Some(nm, TAct (List.map (fun (_,_,t) -> t) param))
                | Export(_,FunDec(_,(_,nm), param, e1)) -> Some(nm, TFun (BIFun ((List.map (fun (_,_,t) -> t) param), ASTUtil.eAnnoOf e1)))
                | _ -> None
              )
      let mdName =
        match fst program.modul with
        | MainModul -> failwith "Is not allowed, but maybe possible?"
        | ImportModul (_,(_,nm)) -> nm
      TModule(mdName,Map.ofSeq a)

    member this.InitCheck () : typErr list =
      let deps =
        List.map
          (fun (_,(cons : constantSyntax<Position>,depKind : dependencyKind,a : Analyser<'eA>))
            -> (depKind,cons.str,fst a.TypedProgram)
          ) <| Map.toList dependencyAnalysis
      InitCheck.InitCheckProgram typedProgram deps dataStructure modulDs

    member __.TagMapForMode mode =
        match mode with
        | InitMode      -> (dataStructure).initTagMap
        | ManageMode    -> (dataStructure).manageTagMap
        | CombinedMode  -> (dataStructure).combinedTagMap

    member __.Environments = (initEnv, updateEnv)
    member __.ModulDataStructure = modulDs
    member __.DataStructure  = dataStructure
    member __.Closures = closure
    member __.Errors = dependencyErrors @ contractErrors @ inhErr @ typeErrs
    member __.UnionTags = allUnionTags
    member __.TypedProgram = typedProgram, dependencyAnalysis