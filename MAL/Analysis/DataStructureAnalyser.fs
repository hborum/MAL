namespace itu.dk.MAL

module DataStructureAnalyser =

  open AST
  open Structure
  open AnalyserTypes
  open Monads
  open PositionBounds

  let addAnnoWhenFalse a = AnnoResult.addAnnoWhenFalse (fun annos -> a::annos )

  let err_cycleInInh name cycle startPos endPos =
    let cycle = String.concat " -> " cycle
    (BadInheritance (sprintf "data %s inherits in the cycle (%s)" name cycle), (posToIndex (startPos, 0),posToIndex (endPos, 0)))
  let err_inhFromNonData tag nonTag startPos endPos =
    (BadInheritance (sprintf "data %s inherits from %s which does not have a data definition." tag nonTag), (posToIndex (startPos, 0),posToIndex (endPos, 0)))
  let err_badFieldInh fieldName subTag superTag typString1 typString2 startPos endPos=
    ( BadInheritance (sprintf "The field %s in %s is a\n%s\nIt must be an extension of\n%s\nBecause %s is an extension of %s"
    fieldName subTag typString1 typString2 subTag superTag)
    , (posToIndex (startPos, 0),posToIndex (endPos, 0)))
  let err_multipleDefs tag startPos endPos =
    BadInheritance (sprintf "data %s is defined multiple times" tag),
    (posToIndex (startPos, 0), posToIndex (endPos, 0))
  let err_redefinesBase tag startPos endPos =
    BadInheritance (sprintf "An import module can not redefine %s" tag),
    (posToIndex (startPos, 0), posToIndex (endPos, 0))
  let err_expectedField tag fldName fldType startPos endPos =
    BadInheritance (sprintf "data %s must have the field %s : %s." tag fldName fldType),
    (posToIndex (startPos, 0), posToIndex (endPos, 0))
  let err_expectedData isMain tagName =
    let defString = if isMain then "data" else "contract"
    BadInheritance (sprintf "The program must contain a %s definition for %s.\nFix by adding:\n%s %s end" defString tagName defString tagName),
    (0, 100)
  let err_unknownType typeName startPos endPos =
    BadInheritance (sprintf "Unknown type '%s'. Fix add data definition or fix spelling." typeName),
      (startPos, endPos)

  let err_mustExtend nm exp startPos endPos =
    BadInheritance (sprintf "%s must extend %s" nm exp),
    (posToIndex (startPos, 0), posToIndex (endPos, 0))

  let err_mayNotExtend nm startPos endPos =
    BadInheritance (sprintf "%s may not extend enything" nm),
    (posToIndex (startPos, 0), posToIndex (endPos, 0))

  let err_importModuleData moduleName startPos endPos =
    ModuleError (sprintf "The imported module %s has an error in its data definition." moduleName), (startPos, endPos)

  let err_doubleRequireProvide contract_name startPos endPos =
    ModuleError (sprintf "The module has two 'contract' definitions for %s." contract_name), (posToIndex (startPos, 0), posToIndex (endPos, 0))

  let err_requireProvideBase entName startPos endPos =
    let bsEntities = String.concat ", " <| List.map (sprintf "'%s'") BaseProgram.baseEntities
    ModuleError (sprintf "It is not possible to 'contract' a '%s'. Only possible on %s." entName bsEntities), (posToIndex (startPos, 0), posToIndex (endPos, 0))


  let hasCycles (dataStructure : dataStructure<Position>) : AnnotatedResult<typErr list, bool> =
    let rec testSingle nm seen (superTypes : superTypeMap) =
      annoResult {
        if List.contains nm seen
        then
          let sA = dataStructure.positions.[nm]
          do! AnnoResult.add (err_cycleInInh nm seen sA.kwStart sA.kwEnd)
          return true
        else
          match Map.tryFind nm superTypes with
          | None ->
            let prevName = List.head seen
            let sA = dataStructure.positions.[prevName]
            do! AnnoResult.add (err_inhFromNonData prevName nm sA.kwStart sA.kwEnd)
            return false
          | Some (None) -> return false
          | Some (Some next) -> return! testSingle next (nm :: seen) superTypes
      }
    annoResult
      {
        let! res =
          mapMA (fun (k,_v) ->
            (annoResult
              {
                return! (testSingle k [] dataStructure.superTypes)
              })
          ) (Map.toList dataStructure.superTypes)
        return (List.exists id res)
      }

  // Probably unessesary complication to do this as AnnoResult since there is no result
  let rec buildTotalTagMap isMain (completeSuperMap : superTypeMap) (superMap : superTypeMap)  (subTypesMap : subTypeMap) (totalTagMap : tagMap<'sA>) (syntaxMap : Map<pureTag, constantSyntax<Position>>) =
    annoResult
      {
        if Map.isEmpty superMap
        then
          return totalTagMap
        else
          // Find and analyse tag without a super
          match Map.tryFindKey (fun _ v -> Option.isNone v) superMap with
          | None -> return totalTagMap
          | Some tag ->
            let superMap = Map.remove tag superMap
            let superMap = Map.map (fun k' tag' -> if tag' = Some tag then None else tag' ) superMap
            let superFields = totalTagMap.[tag]
            let subTypesTags = match Map.tryFind tag subTypesMap with | Some v -> v | None -> []
            let! totalTagMap =
              foldMA (fun (totalTagMap : tagMap<'sA>) subTag ->
                annoResult {
                  let subFields = totalTagMap.[subTag]
                  let! subFields' =
                    foldMA (fun subFields (fieldName, typ) ->
                      annoResult {
                        match Map.tryFind fieldName subFields with
                        | None ->
                          return Map.add fieldName typ subFields
                        | Some typ' ->
                          do!
                            addAnnoWhenFalse
                              (err_badFieldInh fieldName subTag tag (Printer.printTyp typ') (Printer.printTyp typ) syntaxMap.[subTag].kwStart syntaxMap.[subTag].kwEnd)
                              (ASTUtil.isSubOfTyp completeSuperMap typ' typ)
                          return subFields
                      }
                    ) subFields (Map.toList superFields)
                  return Map.add subTag subFields' totalTagMap
                }
              ) totalTagMap subTypesTags
            return! buildTotalTagMap isMain completeSuperMap superMap subTypesMap totalTagMap syntaxMap
      }

  let splitTagMapOnTag (dataStructure : dataStructure<Position>) (initTagMap, manageTagMap) tag fields =
    if not <|
        List.exists (fun baseTag ->
          ASTUtil.isSubOfTyp dataStructure.superTypes (TRec (NoPos, [tag])) (TRec (NoPos,[baseTag]))
        ) BaseProgram.baseEntities
    then // No split
      (Map.add tag fields initTagMap, Map.add tag fields manageTagMap)
    else
      let initFields = Map.remove "Result" fields
      let manageFields = Map.remove "Input" fields
      (Map.add tag initFields initTagMap, Map.add tag manageFields manageTagMap)

  let splitTagMap (dataStructure : dataStructure<Position>) (totalTagMap : tagMap<Position>) =
    Map.fold
      (splitTagMapOnTag dataStructure)
      (Map.empty, Map.empty) totalTagMap

  let expectAndRemove supers tag (errPos : constantSyntax<Position>) fields (name , typ as nm) =
    annoResult
      {
        do!
          addAnnoWhenFalse
            (err_expectedField tag name (Printer.printTyp typ) errPos.kwStart errPos.kwEnd)
            (Map.exists (fun name' typ' -> name = name' && ASTUtil.isSubOfTyp supers typ' typ) fields)
        return Map.remove name fields
      }

  ///<summary>
  ///Iterates over all defined data and builds the extension map
  ///Also checks that each data definition is congruent with the expected fields defined in BaseProgram
  ///</summary>
  let rec buildExtMap isMain dataStructure (totalTagMap : (string * Map<string, typ<Position>>) list) accTagMap supers : AnnotatedResult<typErr list, tagMap<Position>> =
    annoResult
      {
        match totalTagMap with
        | [] -> return accTagMap
        | (tag, fields) :: totalTagMap ->
          do!
            annoResult {
              match Map.tryFind tag BaseProgram.expectedFields with
              | None ->  return ()
              | Some expFields ->
                  let! a =
                    if isMain
                    then foldMA (expectAndRemove supers tag dataStructure.positions.[tag]) fields expFields
                    else annoResult {return Map.empty}
                  return ()
            }
          let! fields =
            let superTagOption =
              Map.tryFindKey
                (fun tag' _ ->
                  ASTUtil.isSubOfTyp dataStructure.superTypes (TRec (NoPos, [tag])) (TRec (NoPos, [tag']))
                ) BaseProgram.specialHandlingFields
            match superTagOption with
            | None ->
              annoResult {
                return fields
              }
            | Some superTag ->
              annoResult {
                //let expectAndRemove = expectAndRemove supers superTag dataStructure.positions.[superTag]
                let fields = List.fold (fun acc name -> Map.remove name acc) fields (BaseProgram.specialHandlingFields.[superTag])
                return fields
              }
          return! buildExtMap isMain dataStructure totalTagMap (Map.add tag fields accTagMap) supers
      }

  let rec validateInheritence superTypes (syntaxPos : Map<pureTag, constantSyntax<Position>>) =
    foldMA
      ( fun _ (tag, exp) ->
        if Map.containsKey tag superTypes
        then
          let sa = syntaxPos.[tag]
          match exp with
          | None ->
              addAnnoWhenFalse
                  (err_mayNotExtend tag sa.kwEnd sa.kwStart)
                  (superTypes.[tag] = exp)
          | Some expTag ->
            addAnnoWhenFalse
                (err_mustExtend tag expTag sa.kwEnd sa.kwStart)
                (superTypes.[tag] = exp)
        else annoResult { return () }
      ) () BaseProgram.baseSuperTypes

  let rec totalTagMapContainsBase isMain (tagMap : tagMap<'aE>) (baseEntities : List<string>) =
    annoResult {
      match baseEntities with
      | [] -> return ()
      | baseTag :: baseEntities' ->
        do!
          addAnnoWhenFalse
            (err_expectedData isMain baseTag)
            (Map.containsKey baseTag tagMap)
        return! totalTagMapContainsBase isMain tagMap baseEntities'
    }

  let outputLevels = [Constants.tag_group ; Constants.tag_policy ; Constants.tag_equity ; Constants.tag_global]

  let buildOutputMap (program : dec<'aE, Position> list) (dataStructure : dataStructure<Position>) : outputMap<Position> =
    let buildForDataDec (dataDec : dataDec<'aE, Position>) =
      let (_,tag) = dataDec.name
      let fields =
        dataDec.fields
          |> List.map (fun ((_,(_,nm),t), output) -> Option.map (fun out -> (nm, out)) output)
          |> List.choose id
      (tag, Map.ofList fields)

    let rec propogateOutput accOutputMap superTypes =
      match Map.tryFindKey (fun k v -> v = None) superTypes with
      | None -> accOutputMap
      | Some k ->
        let subTypes =
          match Map.tryFind k dataStructure.subTypes with
          | None -> []
          | Some subs -> subs
        let toAdd = (Map.find k accOutputMap)
        let accOutputMap =
          List.fold (fun acc subTypeTag ->
            let oldFields = (Map.find subTypeTag acc)
            let newFields = Map.fold (fun acc k v -> Map.add k v acc  ) oldFields toAdd
            Map.add subTypeTag newFields acc
          ) accOutputMap subTypes
        propogateOutput accOutputMap (Map.remove k superTypes)

    /// <summary>
    /// Finds all output fields per output level (e.g. Global, Group, Policy...).
    /// </summary>
    let rec findOutputPerLevel shrinkingOutput (outputMap : Map<string,Map<string, outputInfo<'sA>>>) accOutput =
      let rec collectFields tag fields seen path acc : Map<pureIdentifier list, outputInfo<'sA>> =
        if List.contains tag seen
        then acc
        else
          let outputFields =
            match Map.tryFind tag outputMap with
            | None -> Map.empty
            | Some vs -> vs
          Map.fold
            (fun acc fieldName typ ->
              let acc =
                match Map.tryFind fieldName outputFields with
                | None -> acc
                | Some v -> Map.add (List.rev <| fieldName :: path) v acc
              let acc =
                match typ with
                | TRec (_, [tag2]) ->
                  match Map.tryFind tag2 dataStructure.initTagMap with
                  | None -> acc
                  | Some fields' -> collectFields tag2 fields' (tag::seen) (fieldName :: path) acc
                | _ -> acc
              acc
            ) acc fields

      let tagToAnalyse =
        Map.tryFindKey
          (fun tag v ->
            List.exists
              (fun superTag ->
                ASTUtil.isSubOfTyp dataStructure.superTypes (TRec (NoPos, [tag])) (TRec (NoPos, [superTag]))
              ) outputLevels
          ) shrinkingOutput

      match tagToAnalyse with
      | None -> accOutput
      | Some tagToAnalyse ->
        let shrinkingOutput = Map.remove tagToAnalyse shrinkingOutput
        let fields = Map.find tagToAnalyse dataStructure.initTagMap
        let outputFields = collectFields tagToAnalyse fields [] [] Map.empty
        let accOutput = Map.add tagToAnalyse outputFields accOutput
        findOutputPerLevel shrinkingOutput outputMap accOutput

    let outputMap =
      List.fold
        (fun outputMap (dec : dec<'aE, Position>) ->
          match dec with
          | Data (sA, dataDec) ->
            let (tag, output) = buildForDataDec dataDec
            Map.add tag output outputMap
          | _ -> outputMap
        ) (Map.empty) program

    let outputMap = propogateOutput outputMap dataStructure.superTypes

    findOutputPerLevel outputMap outputMap Map.empty

  /// <summary>
  /// Creates a map from an entity to a list of its sub-types from a superTypeMap
  /// </summary>
  let getSubTypeMap superTypes =
    Map.fold
      (fun (acc : Map<string, string list>) (k : string) (v : string option) ->
        // Morten has a burning desire to have this line included, but it breaks a lot of assumptions in other modules :(
        //let acc = if not <| acc.ContainsKey k then Map.add k [] acc else acc
        match v with
        | None -> acc
        | Some v ->
          match Map.tryFind v acc with
          | None -> Map.add v [k] acc
          | Some ks -> Map.add v (k::ks) acc
      ) Map.empty superTypes

  let buildInitialMaps_main program =
    foldMA (fun (syntaxMap,superTypes,tagMap, fieldMap) (dec : dec<'aE, Position>) ->
      (annoResult {
        match dec with
        | Data (sA, dataDec) ->
          let (sA_nm,nm) = dataDec.name
          do! addAnnoWhenFalse
                (err_multipleDefs nm sA.kwData sA_nm.kwEnd)
                (not <| Map.containsKey nm superTypes)
          let fields = Map.ofList <| List.map (fun ((_,(_,x),f),_) -> (x,f)) dataDec.fields
          let fieldPos = Map.ofList <| List.map (fun ((_,(d,x),f),_) -> (x,d)) dataDec.fields
          return Map.add nm sA_nm syntaxMap
                 , Map.add nm dataDec.extends superTypes
                 , Map.add nm fields tagMap
                 , Map.add nm fieldPos fieldMap
        | _ -> return (syntaxMap,superTypes, tagMap, fieldMap)
      })
      ) (Map.empty, Map.empty, Map.empty, Map.empty) program

  let buildInitialMaps_import program =
    foldMA (fun (syntaxMap,superTypes,tagMap,contracts, fieldMap) (dec : dec<'aE, Position>) ->
      (annoResult {
        match dec with
        | Export (_,Data (sA, dataDec))
        | Data (sA, dataDec) ->
          let (sA_nm,nm) = dataDec.name

          do! addAnnoWhenFalse
                (err_multipleDefs nm sA.kwData sA_nm.kwEnd)
                (not <| Map.containsKey nm superTypes)
          do! addAnnoWhenFalse
                (err_redefinesBase nm sA.kwData sA_nm.kwEnd)
                (not <| List.contains nm BaseProgram.baseEntities)

          let fields = Map.ofList <| List.map (fun ((_,(_,x),f),_) -> (x,f)) dataDec.fields
          let fieldPos = Map.ofList <| List.map (fun ((_,(d,x),f),_) -> (x,d)) dataDec.fields

          return Map.add nm sA_nm syntaxMap
                  , Map.add nm dataDec.extends superTypes
                  , Map.add nm fields tagMap
                  , contracts
                  , Map.add nm fieldPos fieldMap


        | Contract (_, (sA_nm, nm), extends, req_fields, prov_fields) ->
          let syntaxMap' = Map.add nm sA_nm syntaxMap
          let requires = Map.ofList <| List.map (fun ((_,(_,x),f)) -> (x,f)) req_fields
          let provides = Map.ofList <| List.map (fun ((_,(_,x),f)) -> (x,f)) prov_fields
          let reqFieldPos = List.map (fun ((_,(d,x),f)) -> (x,d)) req_fields
          let reqProvPos = List.map (fun ((_,(d,x),f)) -> (x,d)) prov_fields

          do! addAnnoWhenFalse
                (err_doubleRequireProvide nm sA_nm.kwStart sA_nm.kwEnd)
                (not <| Map.containsKey nm contracts)
          do! addAnnoWhenFalse
                (err_requireProvideBase nm sA_nm.kwStart sA_nm.kwEnd)
                (List.contains nm BaseProgram.allEntityTypes)

          let superTypes' = Map.add nm extends superTypes

          let tagMap' = Map.add nm (GeneralUtil.mapJoin requires provides) tagMap

          return   syntaxMap'
                 , superTypes'
                 , tagMap'
                 , Map.add nm (requires, provides) contracts
                 , Map.add nm (Map.ofSeq <| reqFieldPos @ reqProvPos) fieldMap  // TODO ADD expand FieldMap
        | _ -> return (syntaxMap, superTypes, tagMap,contracts, fieldMap)
      })
      ) (Map.empty, Map.empty, Map.empty, Map.empty, Map.empty) program

  let rec wellFormedType (totalTagMap : tagMap<Position>) (typ : typ<Position>) : AnnotatedResult<typErr list, Unit> =
    match typ with
    | TRec (sA,tags) ->
      foldMA
        (fun _ tag ->
          addAnnoWhenFalse
              (err_unknownType tag (posToIndex (sA, 0)) (posToIndex (sA, tag.Length)))
              (Map.containsKey tag totalTagMap)
        ) () tags
    | TGeneric(_,_,genTyps) ->
      foldMA
        (fun _ (_,t)  ->
          wellFormedType totalTagMap t
        ) () genTyps
    | _ ->
      annoResult
        {
        return ()
        }

  ///<summary>
  /// Validates that all types in 'testMap' is well-formed.
  /// This is done by asserting that record definitions exists in 'totalTagMap'
  ///</summary>
  let wellFormedTypes (totalTagMap : tagMap<Position>) (fields : Map<string, typ<Position>>) : AnnotatedResult<typErr list, Unit> =
      foldMA
        (fun _ (pr,tp) ->
          wellFormedType totalTagMap tp
        ) () (Map.toList fields)

  let buildDataInfo ((modul , program) as _p : modul<'aE, Position>) : AnnotatedResult<typErr list, dataStructure<Position>> =
    annoResult
      {
        let isMain =
          match modul with
          | MainModul -> true
          | ImportModul _ -> false
        let! (syntaxMap, superTypes, tagMap, fieldMap, interfaceMap) =
          if isMain
          then
            annoResult {
              let! syntaxMap, superTypes, tagMap, fieldMap = buildInitialMaps_main program
              return syntaxMap, superTypes, tagMap, fieldMap, None
            }
          else
            annoResult {
              let! syntaxMap, superTypes, tagMap, contract, fieldMap = buildInitialMaps_import program
              return syntaxMap, superTypes, tagMap, fieldMap, Some( contract )
            }

        let subTypes = getSubTypeMap superTypes
        let dataStructure = { positions = syntaxMap
                            ; subTypes = subTypes
                            ; superTypes = superTypes
                            ; initTagMap = tagMap
                            ; manageTagMap = tagMap
                            ; combinedTagMap = tagMap
                            ; totalMap = Map.empty
                            ; extMap = Map.empty
                            ; outputMap = Map.empty
                            ; interfaceMap = interfaceMap
                            ; fieldNameInfo = fieldMap
                            }

        let! cycles = hasCycles dataStructure
        if cycles
        then
          do! totalTagMapContainsBase isMain tagMap BaseProgram.baseEntities
          return dataStructure
        else
          // We build totalTagMap containing everything
          let! totalTagMap = buildTotalTagMap isMain dataStructure.superTypes dataStructure.superTypes dataStructure.subTypes tagMap dataStructure.positions

          // Validates totalTagMap contains everything it must contain
          do! totalTagMapContainsBase isMain totalTagMap BaseProgram.baseEntities
          do! validateInheritence superTypes syntaxMap

          do!
            foldMA
              (fun _ (_,fields) ->
                wellFormedTypes totalTagMap fields
              ) () (Map.toList totalTagMap)

          do!
              match interfaceMap with
              | Some (contracts) ->
                foldMA
                  (fun _ (_,(req,prov)) ->
                    annoResult
                      {
                        do! wellFormedTypes totalTagMap req
                        do! wellFormedTypes totalTagMap prov
                        return ()
                      }
                  ) () (Map.toList contracts)
              | None -> annoResult {return ()}

          // Splits totalTagMap into its init and manage part
          let initTagMap, manageTagMap = splitTagMap dataStructure totalTagMap
          // Builds a map containing output information
          let outputMap =
            if isMain
            then buildOutputMap program dataStructure
            else Map.empty

          let! extMap = buildExtMap isMain dataStructure <| Map.toList totalTagMap <| Map.empty <| superTypes
          return { dataStructure with initTagMap = initTagMap
                                    ; manageTagMap = manageTagMap
                                    ; totalMap = totalTagMap
                                    ; extMap = extMap
                                    ; outputMap = outputMap}
      }

  ///<summary>
  /// Returns a map from module name to dataStructure
  ///</summary>
  let getDataInfoProgram (program : program<'eA, Position>) : dataStructure<Position> * typErr list =
    AnnoResult.run [] <| buildDataInfo program.modul