namespace itu.dk.MAL

module ExportAnalyser =
  open AST
  open Monads

  let addAnnoWhenFalse a = AnnoResult.addAnnoWhenFalse (fun annos -> a::annos )

  ///<summary>
  /// Validates that the datastructure of the main module is compatible with the import module.
  ///</summary>
  let dataStructureConsistent (ds_main : dataStructure<Position>)
                              (ds_import : dataStructure<Position>)
                              (modulName : string)
                              (modulSa : constantSyntax<Position>) =
    annoResult
      {
        do!
          foldMA
            ( fun _ (tag,fields_import) ->
              annoResult
                {
                  match Map.tryFind tag ds_main.totalMap with
                  | None ->
                    do! AnnoResult.add (ErrorMsgs.import_modul_unknown_data (modulName, tag) modulSa.kwStart modulSa.kwEnd)
                    return ()
                  | Some fields_main ->
                    return!
                      foldMA
                        (fun () (field_name, typ_import) ->
                          annoResult
                            {
                              let sA = ds_main.positions.[tag]
                              let validated =
                                match Map.tryFind field_name fields_main with
                                | None -> false
                                | Some (typ_main) -> ASTUtil.isSubOfTyp ds_main.superTypes typ_main typ_import
                              do! addAnnoWhenFalse
                                    (ErrorMsgs.import_modul_data_inc (modulName, tag, field_name, (Printer.printTyp typ_import)) sA.kwStart sA.kwEnd )
                                    validated
                              return ()
                            }
                        ) () (Map.toList fields_import)
                }
            ) () (Map.toList ds_import.totalMap)
      }


  ///<summary>
  /// Validates datastructure contains anything sepcified by 'iMap'.
  /// 'iMap' is either a requires or provides map.
  /// 'modulName' is the name of the modul defining the interface
  ///</summary>
  let dataStructureMeetsInterface (dataStructure : dataStructure<Position>)
                                  (iMap : Map<string, Map<string, typ<Position>> * Map<string, typ<Position>>> )
                                  (modulName : string) =
    foldMA
      ( fun () (tag,(requires, provides)) ->
          match Map.tryFind tag dataStructure.totalMap with
          | None -> annoResult { return () } // Problem but error has been generated previously
          | Some entMap ->
            let sA = dataStructure.positions.[tag]
            foldMA
              ( fun () (f_name, f_type) ->
                annoResult
                  {
                    let meetsInterface = // We generate same error message for both situation
                      match Map.tryFind f_name entMap with
                      | None -> false
                      | Some main_typ -> main_typ = f_type
                    do! addAnnoWhenFalse
                          (ErrorMsgs.import_modul_data_must_include (modulName, tag, f_name, Printer.printTyp f_type) sA.kwStart sA.kwEnd)
                          meetsInterface
                    return ()
                  }
              ) () (Map.toList requires @ Map.toList provides)
      ) () (Map.toList iMap)

  let checkContract (ds_main : dataStructure<Position>) (deps : Map<string, constantSyntax<Position> *  dataStructure<Position>>) =
    let inner =
        foldMA
          (fun _ (modul_nm,(model_sA,ds)) ->
            annoResult
              {
                // All dependency modules should have an interface. However, this is required earlier.
                let contract = Option.defaultValue (Map.empty) ds.interfaceMap
                do! dataStructureMeetsInterface ds_main contract modul_nm
                do! dataStructureConsistent ds_main ds modul_nm model_sA
                return ()
              }
          ) () <| Map.toList deps
    AnnoResult.run [] inner
