namespace itu.dk.MAL

#nowarn "40"
module BuiltIns =

  open Gen
  open AST
  open Monads
  open System.Reflection
  open FParsec

  // Conceptually similar to BaseProgram.fs::CsTypeToMalType

  let strToTyp str =
    match str with
    | "Double" -> AST.nos_TDouble
    | "Int32" | "Int64" -> AST.nos_TDouble //Interger type not yet defined
    | "String" -> AST.nos_TStr
    | "Boolean" -> AST.nos_TBool
    | "IFunLambda" | "IFunction" -> AST.nos_IFun
    | "Void" -> AST.TVoid
    | _ when List.contains str AST.enumerations -> AST.nos_TEnum str
    | s ->
      AST.nos_TRec [s]
  let simpleName : Parser<typ<AST.Position>,unit> =
    parse {
      let! s = sepBy1 (many1Chars (letter <|> digit <|> pchar '_')) (pchar '.')
      let typeString = s.[s.Length-1]
      return strToTyp typeString
    }
  let rec nestedName =
    let innerName = parse {
      do! skipChar '['
      let! res = coreName
      do! skipManyTill anyChar (pchar ']')
      return res
    }
    parse {
      do! skipChar '`'
      do! digit |>> ignore
      return! between (pchar '[') (pchar ']') (sepBy1 innerName (pchar ','))
    }
  and coreName =
    parse {
      let! tName = simpleName
      let! fullType =
        parse {
          do! notFollowedBy (pchar '`')
          return
            match tName with
            | TRec (_,[s]) ->
              match Map.tryFind s Constants.typeNameConversionEdlundtoMAL with
              | Some s -> nos_TRec [s]
              | None -> tName
            | _ -> tName
        } <|>
        parse {
          let! inners = nestedName
          let typ =
            match tName with
            | TRec (_,[s]) ->
              match Map.tryFind s Constants.typeNameConversionEdlundtoMAL with
              | Some s -> nos_TRec [s]
              | None ->
                match s with
                | "Nullable" -> nos_TOption inners.[0]
                | "Func" ->
                  assert(List.length inners = 2)
                  nos_TSysFunc inners.[0] inners.[1]
                | "IEnumerable" ->
                  nos_TList inners.[0]
                | d when d.EndsWith "Dictionary" ->
                  nos_TMap inners.[0] inners.[1]
                | d when d.StartsWith "MAL_" ->
                  nos_TRec [d.Substring 4]
                | "Tuple" ->
                  nos_TPair inners.[0] inners.[1]
                | "List" ->
                  nos_TList inners.[0]
                | _ ->
                  printfn "crash %s" s
                  failwith "unexpected type in builtins"
            | _ ->
              printfn "tName %A" tName
              failwith "unexpected type in builtins"

          return typ
        }
      let! arr = many <| pstring "[]" |>> List.fold (fun t _ -> AST.nos_TList t) fullType
      return arr
    }
  let parseTypeName nm =
    (runParserOnString coreName () "" >>
        (fun res ->
          match res with
          | Success(name, _, _) -> name
          | Failure(err, _, _) -> failwith err
         )) nm

  // Needs som polymorphism
  let builtInTypes =

    let append =
      ParametricFun (
        fun ts ->
          match ts with
          | [TGeneric (_, GenericList, [_,t1]); TGeneric (_, GenericList, [_,t2]) ] ->
             Result.Ok <| nos_TList t1
          | _ -> Result.Error <| sprintf "count expects (List<Any>) got %A" ts
      )

    let countType =
      ParametricFun (
        fun ts ->
          match ts with
          | [TGeneric (_, GenericList, [_,t])] -> Result.Ok nos_TDouble
          | _ -> Result.Error <| sprintf "count expects (List<Any>) got %A" ts
      )
    let createMapType =
      ParametricFun (
        fun ts ->
          match ts with
          | [TGeneric (_, GenericList, [ _, TGeneric (_, GenericPair, [_,t1 ; _,t2 ] ) ]) ] -> Result.Ok <| nos_TMap t1 t2
          | _ -> Result.Error <| sprintf "createMap expects (List<Pair<Any_1, Any_2>>) got %A" ts
      )

    let fst =
      ParametricFun (
        fun ts ->
          match ts with
          | [TGeneric (_, GenericPair, (_,t) :: _ )] -> Result.Ok t
          | _ -> Result.Error <| sprintf "fst expects (Pair<Any_1, Any_2>) got %A" ts
      )

    let firstOrNone =
      ParametricFun (
        fun ts ->
          match ts with
          | [TGeneric (s, GenericList, (ss,t) :: _ )] -> Result.Ok <| TGeneric (s, GenericOption, [(ss,t) ] )
          | _ -> Result.Error <| sprintf "fst expects (Pair<Any_1, Any_2>) got %A" ts
      )

    let getValue =
      ParametricFun (
        fun ts ->
          match ts with
          | [TGeneric (_, GenericOption, (_,t) :: _ )] -> Result.Ok t
          | _ -> Result.Error <| sprintf "count expects (List<Any>) got %A" ts
      )

    let mapToKeys =
      ParametricFun (
        fun ts ->
          match ts with
          | [TGeneric (_, GenericMap, [_,t1 ; _,_])] -> Result.Ok <| nos_TList t1
          | _ -> Result.Error <| sprintf "mapToKeys expects (TMap<Any_1, Any_2>) got %A" ts
      )
    let mapToValues =
      ParametricFun (
        fun ts ->
          match ts with
          | [TGeneric (_, GenericMap, [_,_ ; _,t2])] -> Result.Ok <| nos_TList t2
          | _ -> Result.Error <| sprintf "mapToValues expects (TMap<Any_1, Any_2>) got %A" ts
      )

    let single =
      ParametricFun (
        fun ts ->
          match ts with
          | [TGeneric (_, GenericList, [_,t])] -> Result.Ok <| t
          | _ -> Result.Error <| sprintf "single expects (List<Any>) got %A" ts
      )

    let skip =
      ParametricFun (
        fun ts ->
          match ts with
          | [TDouble _ ; TGeneric (_, GenericList, [_,t])] -> Result.Ok <| nos_TList t
          | _ -> Result.Error <| sprintf "skip expects (List<Any>) got %A" ts
      )

    let skipTail =
      ParametricFun (
        fun ts ->
          match ts with
          | [TDouble _ ; TGeneric (_, GenericList, [_,t])] -> Result.Ok <| nos_TList t
          | _ -> Result.Error <| sprintf "skip expects (List<Any>) got %A" ts
      )

    let snd =
      ParametricFun (
        fun ts ->
          match ts with
          | [TGeneric (_, GenericPair, _ :: (_,t) :: _ )] -> Result.Ok t
          | _ -> Result.Error <|  sprintf "snd expects (Pair<Any_1, Any_2>) got %A" ts
      )

    let uniqueType =
      ParametricFun (
        fun ts ->
          match ts with
          | [TGeneric (_, GenericList, [_,t])] -> Result.Ok <| nos_TList t
          | _ -> Result.Error <| sprintf "unique expects (List<Any>) got %A" ts
      )

    let index=
      ParametricFun (
        fun ts ->
          match ts with
          | [TGeneric (_, GenericList, [_,t1]) ; TDouble _] ->
            Result.Ok <| t1
          | _ -> Result.Error <| sprintf "index expects (List<Any>, double) got %A" ts
      )

    let isEmpty =
      ParametricFun (
        fun ts ->
          match ts with
          | [TGeneric (_, GenericList, [_,t1]) ] ->
            Result.Ok <| nos_TBool
          | _ -> Result.Error <| sprintf "isEmpty expects (List<Any>) got %A" ts
      )

    let isPermutation =
      ParametricFun (
        fun ts ->
          match ts with
          | [TGeneric (_, GenericList, [_,t1]) ; TGeneric (_, GenericList, [_,t2])] ->
            Result.Ok <| nos_TBool
          | _ -> Result.Error <| sprintf "isPermutation expects (List<Any>, List<Any>) got %A" ts
      )

    let println =
      ParametricFun (
        fun ts ->
          match ts with
          | [t] -> Result.Ok <| t
          | _ -> Result.Error <| sprintf "println expects (Any) got %A" ts
        )

    let parametricFuns =
      [    "append",                        append
        ;  "count",                         countType
        ; "createMap",                      createMapType
        ; "fst",                            fst
        ; "firstOrNone",                    firstOrNone
        ; "getValue",                       getValue
        ; "index",                          index
        ; "isEmpty",                        isEmpty
        //; "mapToValues",                    mapToValues
        ; "mapToKeys",                      mapToKeys
        ; "single",                         single
        ; "skip",                           skip
        ; "skipTail",                       skipTail
        ; "snd",                            snd
        ; "unique",                         uniqueType
        ; "println",                        println
        ; "print",                          println
        ; "isPermutation",                  isPermutation
      ]
    let notVoid : typ<AST.Position> -> bool = function TVoid -> false | _ -> true


    let tryParseTypeName pName = if pName |> isNull then AST.TVoid else parseTypeName pName

    let reflBuiltInTypes = query {
      for (methodName, args, ret) in Gen.BuiltIns<IFunLambda>._Methods() do
        where (not (List.exists (fun (n,_) -> n = methodName) parametricFuns) || methodName.StartsWith "___")
        select (methodName
               , Seq.map tryParseTypeName args
               , tryParseTypeName ret)
    }

    let dynamicFuns =
      query {
        for (name, ins, out) in reflBuiltInTypes do
        where (Seq.forall notVoid ins)
        select (name, BIFun (List.ofSeq ins, out))
      }
    let allFuns = parametricFuns @ List.ofSeq dynamicFuns
    List.map(fun (name,t) -> (name, TFun(t))) allFuns