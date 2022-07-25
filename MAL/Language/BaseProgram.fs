namespace itu.dk.MAL

open Gen
open FParsec

module BaseProgram =
  open Constants
  open AST
  open System
  open System.Reflection

   //This could probably a pretty bad/unflexible way of handling type translation
  let CsTypeToMalType =
    Map.ofList
      [ "System.Double[]", (nos_TList nos_TDouble  , "double[]")
      ; "System.Double"                                                   , ( nos_TDouble                                                                           , "double")
      ; "Nullable<Double>"                                                , ( nos_TOption nos_TDouble                                                               , "Nullable<Double>")
      ; "Double"                                                          , ( nos_TDouble                                                                           , "double")
      ; "Double[]"                                                        , ( nos_TList nos_TDouble                                                                 , "double[]")
      ; "String[]"                                                        , ( nos_TList nos_TStr                                                                    , "string[]")
      ; "IDictionary<String,String[]>"                                    , ( nos_TMap nos_TStr (nos_TList nos_TStr)                                                , "IDictionary<String,String[]>")
      ; "IReadOnlyDictionary<String,String[]>"                            , ( nos_TMap nos_TStr (nos_TList nos_TStr)                                                , "IReadOnlyDictionary<String,String[]>")
      ; "String"                                                          , ( nos_TStr                                                                              , "String")
      ; "Boolean"                                                         , ( nos_TBool                                                                             , "Boolean")
      ; "Param"                                                           , ( nos_TRec ["Param"]                                                                    , "MAL_Param<TFunction>")
      ; "EquityResult"                                                    , ( nos_TRec ["EquityResult"]                                                             , "MAL_EquityResult<TFunction>")
      ; "EquityInput"                                                     , ( nos_TRec ["EquityInput"]                                                              , "MAL_EquityInput<TFunction>")
      ; "EquityResult[]"                                                  , ( nos_TList <| nos_TRec ["EquityResult"]                                                , "MAL_EquityResult<TFunction>[]")
      ; "Asset[]"                                                         , ( nos_TList <| nos_TRec ["Asset"]                                                       , "MAL_Asset<TFunction>[]")
      ; "Policy"                                                          , ( nos_TRec ["Policy"]                                                                   , "MAL_Policy<TFunction>")
      ; "Policy[]"                                                        , ( nos_TList <| nos_TRec ["Policy"]                                                      , "MAL_Policy<TFunction>[]")
      ; "PolicyInput"                                                     , ( nos_TRec ["PolicyInput"]                                                              , "MAL_PolicyInput<TFunction>")
      ; "PolicyResult"                                                    , ( nos_TRec ["PolicyResult"]                                                             , "MAL_PolicyResult<TFunction>")
      ; "Input"                                                           , ( nos_TRec ["Input"]                                                                    , "MAL_Input<TFunction>")
      ; "GroupResult"                                                     , ( nos_TRec ["GroupResult"]                                                              , "MAL_GroupResult<TFunction>")
      ; "Group"                                                           , ( nos_TRec ["Group"]                                                                    , "MAL_Group<TFunction>")
      ; "Group[]"                                                         , ( nos_TList <| nos_TRec ["Group"]                                                       , "MAL_Group<TFunction>[]")
      ; "Interest"                                                        , ( nos_TRec ["Interest"]                                                                 , "MAL_Interest<TFunction>")
      ; "Expense"                                                         , ( nos_TRec ["Expense"]                                                                  , "MAL_Expense<TFunction>")
      ; "Risk"                                                            , ( nos_TRec ["Risk"]                                                                     , "MAL_Risk<TFunction>")
      ; "Risk[]"                                                          , ( nos_TList <| nos_TRec ["Risk"]                                                        , "MAL_Risk<TFunction>[]")
      ; "ReserveGroupInput"                                               , ( nos_TRec ["ReserveGroupInput"]                                                        , "MAL_ReserveGroupInput<TFunction>")
      ; "Policy`1[]"                                                      , ( nos_TList <| nos_TRec ["Policy"]                                                      , "MAL_Policy<TFunction>[]")
      ; "Asset`1[]"                                                       , ( nos_TList <| nos_TRec ["Asset"]                                                       , "MAL_Asset<TFunction>[]")
      ; "Policy`1[]"                                                      , ( nos_TList <| nos_TRec ["Policy"]                                                      , "MAL_Policy<TFunction>[]")
      ; "Asset`1[]"                                                       , ( nos_TList <| nos_TRec ["Asset"]                                                       , "MAL_Asset<TFunction>[]")
      ; "Group`1[]"                                                       , ( nos_TList <| nos_TRec ["Group"]                                                       , "MAL_Group<TFunction>[]")
      ; "Risk`1[]"                                                        , ( nos_TList <| nos_TRec ["Risk"]                                                        , "MAL_Risk<TFunction>[]")
      ; "CashFlow`1[]"                                                    , ( nos_TList <| nos_TRec ["CashFlow"]                                                    , "MAL_CashFlow<TFunction>[]")
      ; "Expenses"                                                        , ( nos_TRec ["Expenses"]                                                                 , "MAL_Expenses<TFunction>")
      ; "IDictionary<String,Double>"                                      , ( nos_TMap nos_TStr nos_TDouble                                                         , "IDictionary<String,Double>")
      ; "IReadOnlyDictionary<String,Double>"                              , ( nos_TMap nos_TStr nos_TDouble                                                         , "IReadOnlyDictionary<String,Double>")
      ; "CashFlow"                                                        , ( nos_TRec ["CashFlow"]                                                                 , "MAL_CashFlow<TFunction>")
      ; "ICashFlow"                                                       , ( nos_TRec ["Transfers"]                                                                , "ICashFlow")
      ; "CashFlowInput"                                                   , ( nos_TRec ["CashFlowInput"]                                                            , "MAL_CashFlowInput<TFunction>")
      ; "ActualExpense"                                                   , ( nos_TRec ["ActualExpense"]                                                            , "MAL_ActualExpense<TFunction>")
      ; "WithExpenses"                                                    , ( nos_TRec ["WithExpenses"]                                                             , "MAL_WithExpenses<TFunction>")
      ; "WithoutExpenses"                                                 , ( nos_TRec ["WithoutExpenses"]                                                          , "MAL_WithoutExpenses<TFunction>")
      ; "PolicyPaidExpense"                                               , ( nos_TRec ["PolicyPaidExpense"]                                                        , "MAL_PolicyPaidExpense<TFunction>")
      ; "Transfers"                                                       , ( nos_TRec ["Transfers"]                                                                , "MAL_Transfers<TFunction>")
      ; "Func<Double,Double>"                                             , ( (nos_TSysFunc nos_TDouble nos_TDouble)                                                , "Func<Double,Double>")
      ; "Func<Double,Func<Double,Double>>"                                , ( (nos_TSysFunc nos_TDouble (nos_TSysFunc nos_TDouble nos_TDouble))                     , "Func<Double,Func<Double,Double>>")
      ; "IDictionary<String,Func<Double,Func<Double,Double>>>"            , ( nos_TMap nos_TStr (nos_TSysFunc nos_TDouble (nos_TSysFunc nos_TDouble nos_TDouble))   , "IDictionary<String,Func<Double,Func<Double,Double>>>")
      ; "AssetInput"                                                      , ( nos_TRec ["AssetInput"]                                                               , "MAL_AssetInput<TFunction>")
      ; "GlobalInput"                                                     , ( nos_TRec ["GlobalInput"]                                                              , "MAL_GlobalInput<TFunction>")
      ; "Equity"                                                          , ( nos_TRec ["Equity"]                                                                   , "MAL_Equity<TFunction>")
      ; "Asset"                                                           , ( nos_TRec ["Equity"]                                                                   , "MAL_Asset<TFunction>")
      ; "Global"                                                          , ( nos_TRec ["Global"]                                                                   , "MAL_Global<TFunction>")
      ; "MarketRateInterest"                                              , ( nos_TRec ["MarketRateInterest"]                                                       , "MAL_MarketInterestRate<TFunction>")
      ; "Projection"                                                      , ( nos_TRec ["Projection"]                                                               , "MAL_Projection<TFunction>")
      ; "IDictionary<String,MAL_InterpretedDiscountFactors>"              , ( nos_TMap nos_TStr (nos_TRec ["InterpretedDiscountFactors"])                           , "IDictionary<String,MAL_InterpretedDiscountFactors<TFunction>>")
      ; "IReadOnlyDictionary<LumpedState,MAL_ThreeStateState<IFunction>>" , ( nos_TMap (nos_TEnum "LumpedState") (nos_TRec ["ThreeStateState"])                     , "IReadOnlyDictionary<LumpedState,MAL_ThreeStateState<TFunction>>")
      ; "IDictionary<String,ICashFlow>"                                   , ( nos_TMap nos_TStr (nos_TRec ["Transfers"])                                            , "IDictionary<String,ICashFlow>")
      ; "IReadOnlyDictionary<String,ICashFlow>"                           , ( nos_TMap nos_TStr (nos_TRec ["Transfers"])                                            , "IReadOnlyDictionary<String,ICashFlow>")
      ; "IReadOnlyDictionary<LumpedStateWithSurrender,IFunction>"         , ( nos_TMap (nos_TEnum "LumpedStateWithSurrender") nos_IFun                              , "IReadOnlyDictionary<LumpedStateWithSurrender,TFunction>")
      ; "IReadOnlyDictionary<Tuple<State,State>,IFunction>"               , ( nos_TMap (nos_TPair (nos_TEnum "State") (nos_TEnum "State")) nos_IFun                 , "IReadOnlyDictionary<Tuple<State,State>,TFunction>")
      ; "IDictionary<LumpedState,Double>"                                 , ( nos_TMap (nos_TEnum "LumpedState") nos_TDouble                                        , "IDictionary<LumpedState,Double>")
      ; "TypeSpan<MAL_Policy<IFunction>>"                                 , ( nos_TList (nos_TRec ["Policy"] )                                                      , "TypeSpan<MAL_Policy<TFunction>>")
      ; "TypeSpan<MAL_Asset<IFunction>>"                                  , ( nos_TList (nos_TRec ["Asset"] )                                                       , "TypeSpan<MAL_Asset<TFunction>>")
      ; "TypeSpan<MAL_Policy<IFunction>>"                                 , ( nos_TList (nos_TRec ["Policy"] )                                                      , "TypeSpan<MAL_Policy<TFunction>>")
      ; "TypeSpan<MAL_Asset<IFunction>>"                                  , ( nos_TList (nos_TRec ["Asset"] )                                                       , "TypeSpan<MAL_Asset<TFunction>>")
      ; "TypeSpan<MAL_Group<IFunction>>"                                  , ( nos_TList (nos_TRec ["Group"] )                                                       , "TypeSpan<MAL_Group<TFunction>>")
      ; "TypeSpan<MAL_Risk<IFunction>>"                                   , ( nos_TList (nos_TRec ["Risk"] )                                                        , "TypeSpan<MAL_Risk<TFunction>>")
      ; "TypeSpan<MAL_CashFlow<IFunction>>"                               , ( nos_TList (nos_TRec ["CashFlow"] )                                                    , "TypeSpan<MAL_CashFlow<TFunction>>")
      ; "TypeSpan<MAL_Group<IFunction>>"                                  , ( nos_TList (nos_TRec ["Group"] )                                                       , "TypeSpan<MAL_Group<TFunction>>")
      ; "ThreeStateResult"                                                , ( nos_TRec ["ThreeStateResult"]                                                         , "ThreeStateResult<TFunction>")
      ; "IFunction"                                                       , ( nos_IFun                                                                              , "TFunction")
      ; "IDictionary<LumpedState,MAL_Expenses<IFunction>>"                , ( nos_TMap (nos_TEnum "LumpedState") (nos_TRec ["Expenses"])                            , "IDictionary<LumpedState,MAL_Expenses<TFunction>>")
      ; "Tuple`2[]"                                                       , ( nos_TList (nos_TPair nos_TDouble nos_TDouble)                                         , "Tuple<Double,Double>[]")
      ; "IReadOnlyDictionary<MAL_Policy<IFunction>,String[]>"             , ( nos_TMap (nos_TRec ["Policy"]) (nos_TList <| nos_TStr)                                , "IReadOnlyDictionary<MAL_Policy<TFunction>,String[]>")
      ]

  ///<summary>
  /// ´extPropertyInfo´ contains information about the properties a generated class must have to
  /// implement the interfaces defined in `GenInterfaces.cs`
  ///</summary>
  type extPropertyInfo =
    { fieldName : string         //The name of the property
    ; fieldTypeStr : string
    ; underlyingField : string   //The underlying field where we can read the property. E.g. The underlying field of `MAL_Reserve` is Reserve
    ; hasGet : bool
    ; hasSet : bool
    ; converter : (String * ConversionType) option
    }

  let reflectRequiredFields (t : Type) =
    let removePreAndPostfix (s: string) =
      let s =
        if s.StartsWith("MAL_")
        then s.Substring 4
        else s
      if s.EndsWith("<IFunction>")
      then s.Substring (0, (s.Length - "<IFunction>".Length))
      else s

    let changePrefix (s : string) =
      if s.EndsWith("<IFunction>")
      then s.Substring (0, (s.Length - "<IFunction>".Length)) + "<TFunction>"
      else s

    let convertType (t : Type) =
      let typName = removePreAndPostfix (t.GetFriendlyName())
      match Map.tryFind typName CsTypeToMalType with
      | Some t -> fst t
      | None -> printfn "Impossible: should contain %s" typName ; nos_TDouble

    let convertTypeMAL (t : Type) =
      let typName = removePreAndPostfix (t.GetFriendlyName())
      match Map.tryFind typName CsTypeToMalType with
      | Some t -> snd t
      | None -> printfn "Impossible: should contain %s" typName ; "double"

    let interfaceProp = Seq.collect (fun (t : Type) -> t.GetProperties()) <| t.GetInterfaces()
    let allProps = Seq.append interfaceProp (t.GetProperties())

    let MAL_fields =
      Seq.map
        (fun (prop : PropertyInfo) ->
          let malType = prop.GetCustomAttribute<MalTypeName>()
          let typ =
            if malType <> null
            then
              match run Parser.parseType malType.Name with
              | Success(t,_,_) -> t
              | _ -> failwith "Impossible invalid MalTypeName in interfaces"
            else
              convertType prop.PropertyType
          removePreAndPostfix prop.Name, typ
        ) allProps
    let interfaceProperties =
      Seq.map
        (fun (prop : PropertyInfo) ->
          let attributes = prop.GetCustomAttributes()
          let conversion =
            Seq.tryFind
              (fun (f : Attribute) ->
                match f with
                | :? MalConversionToMalI as a -> true
                | _ -> false
              ) attributes
            |> Option.map
              ( fun (f : Attribute) -> (f :?> MalConversionToMalI).Conversion,(f :?> MalConversionToMalI).ConversionType )

          { fieldName = prop.Name
          ; underlyingField = removePreAndPostfix (prop.Name)
          ; fieldTypeStr = convertTypeMAL prop.PropertyType
          ; hasGet = prop.CanRead
          ; hasSet = prop.CanWrite
          ; converter = conversion
          }
        ) allProps

    MAL_fields |> Seq.toList, (changePrefix <| t.GetFriendlyName(), interfaceProperties)

  let baseEntities =
     [ tag_baseEntity
     ; tag_group
     ; tag_reserveGroup
     ; tag_interest
     ; tag_expense
     ; tag_risk
     ; tag_marketRateInterest
     ; tag_policy
     ; tag_oneStatePolicy
     ; tag_threeStatePolicy
     ; tag_cashFlow
     ; tag_actualExpense
     ; tag_withExpenses
     ; tag_withoutExpenses
     ; tag_policyPaidExpense
     ; tag_equity
     ; tag_global
     ; tag_asset
     ]

  let allEntityTypes =
    "Projection" :: "LumpedStateResult" ::
      List.collect
        ( fun tag ->
            [tag; sprintf "%sInput" tag ; sprintf "%sResult" tag ; sprintf "%sParam" tag]
        ) baseEntities

  let baseSuperTypes =
      [ tag_baseEntity          , None

      ; tag_asset               , Some tag_baseEntity

      ; tag_global              , Some tag_baseEntity

      ; tag_equity              , Some tag_baseEntity


      ; tag_group               , Some tag_baseEntity
      ; tag_reserveGroup        , Some tag_group
      ; tag_marketRateInterest  , Some tag_group
      ; tag_interest            , Some tag_reserveGroup
      ; tag_expense             , Some tag_reserveGroup
      ; tag_risk                , Some tag_reserveGroup


      ; tag_policy              , Some tag_baseEntity
      ; tag_oneStatePolicy      , Some tag_policy
      ; tag_threeStatePolicy    , Some tag_policy

      ; tag_cashFlow            , Some tag_baseEntity
      ; tag_actualExpense       , Some tag_cashFlow
      ; tag_withExpenses        , Some tag_cashFlow
      ; tag_withoutExpenses     , Some tag_cashFlow
      ; tag_policyPaidExpense   , Some tag_cashFlow
      ]



  ///<summary>
  /// `baseExpectedFields` and `expectedFields` contain basic expected data-modeling in a MAL program.
  /// This is used for translating to-and-from Edlund's data representation.
  ///</summary>
  /// <note> unmodeled ´Transfers´ in CashFlow is translated to a ICashFlow<IFunction> </note>
  let basicSpecialHandling = ["Name"; "Input"; "Param"; "Result"]

  let specialHandlingFields =
    Map.ofList <|
      [ (tag_group ,  "Policies" ::" Assets" :: basicSpecialHandling)

      ; (tag_policy, "Equity" :: "Groups" :: "CashFlows" :: basicSpecialHandling)
      ; (tag_cashFlow , "Policy" :: basicSpecialHandling)
      ; (tag_equity, "Policies" ::" Assets" :: basicSpecialHandling)
      ; (tag_global, basicSpecialHandling)
      ; (tag_asset, "Policy" :: basicSpecialHandling)
      ]

  let baseExpectedFields =
    [ ("Param"  , nos_TRec ["Param"])
    ; ("Name"   , nos_TStr)
    ]

  let fieldInformation =
    [ tag_actualExpense        , reflectRequiredFields <| typeof<MAL_ActualExpense<IFunction>>
    ; tag_asset                , reflectRequiredFields <| typeof<MAL_Asset<IFunction>>
    ; tag_assetInput           , reflectRequiredFields <| typeof<MAL_AssetInput<IFunction>>
    ; tag_cashFlow             , reflectRequiredFields <| typeof<MAL_CashFlow<IFunction>>
    ; tag_cashFlowInput        , reflectRequiredFields <| typeof<MAL_CashFlowInput<IFunction>>
    ; tag_equity               , reflectRequiredFields <| typeof<MAL_Equity<IFunction>>
    ; tag_equityResult         , reflectRequiredFields <| typeof<MAL_EquityResult<IFunction>>
    ; tag_equityInput          , reflectRequiredFields <| typeof<MAL_EquityInput<IFunction>>
    ; tag_expense              , reflectRequiredFields <| typeof<MAL_Expense<IFunction>>
    ; tag_expenses             , reflectRequiredFields <| typeof<MAL_Expenses<IFunction>>
    ; tag_global               , reflectRequiredFields <| typeof<MAL_Global<IFunction>>
    ; tag_globalInput          , reflectRequiredFields <| typeof<MAL_GlobalInput<IFunction>>
    ; tag_group                , reflectRequiredFields <| typeof<MAL_Group<IFunction>>
    ; tag_groupResult          , reflectRequiredFields <| typeof<MAL_GroupResult<IFunction>>
    ; tag_input                , reflectRequiredFields <| typeof<MAL_Input<IFunction>>
    ; tag_interest             , reflectRequiredFields <| typeof<MAL_Interest<IFunction>>
    ; tag_marketRateInterest   , reflectRequiredFields <| typeof<MAL_MarketRateInterest<IFunction>>
    ; tag_param                , reflectRequiredFields <| typeof<MAL_Param<IFunction>>
    ; tag_policy               , reflectRequiredFields <| typeof<MAL_Policy<IFunction>>
    ; tag_oneStatePolicy       , reflectRequiredFields <| typeof<MAL_OneStatePolicy<IFunction>>
    ; tag_threeStatePolicy     , reflectRequiredFields <| typeof<MAL_ThreeStatePolicy<IFunction>>
    ; tag_threeStateState      , reflectRequiredFields <| typeof<MAL_ThreeStateState<IFunction>>
    ; tag_policyInput          , reflectRequiredFields <| typeof<MAL_PolicyInput<IFunction>>
    ; tag_policyPaidExpense    , reflectRequiredFields <| typeof<MAL_PolicyPaidExpense<IFunction>>
    ; tag_policyResult         , reflectRequiredFields <| typeof<MAL_PolicyResult<IFunction>>
    ; tag_oneStateResult       , reflectRequiredFields <| typeof<MAL_OneStateResult<IFunction>>
    ; tag_projection           , reflectRequiredFields <| typeof<MAL_Projection<IFunction>>
    ; tag_reserveGroupInput    , reflectRequiredFields <| typeof<MAL_ReserveGroupInput<IFunction>>
    ; tag_risk                 , reflectRequiredFields <| typeof<MAL_Risk<IFunction>>
    ; tag_transfers            , reflectRequiredFields <| typeof<MAL_Transfers<IFunction>>
    ; tag_withExpenses         , reflectRequiredFields <| typeof<MAL_WithExpenses<IFunction>>
    ; tag_withoutExpenses      , reflectRequiredFields <| typeof<MAL_WithoutExpenses<IFunction>>
    ]

  // todo combine interfaceFields and expectedFields
  let interfaceFields =
    List.map (fun (tag,(_,c)) -> tag,c) fieldInformation
  let expectedFields =
    Map.ofSeq <|
      Seq.map
        (fun (tag,(b,_)) ->
          if tag = "Policy"
          then tag, b //(("CashFlows"    , nos_TList (nos_TRec ["CashFlow"]) None) :: b)
          else tag, b
        ) fieldInformation

  // These are the basic types that should be handled
  // when transforming from internal to external representation
  type ExpectedTag =
    | Equity
    | Interest
    | Risk
    | Expense
    | MarketRateInterest
    | Policy
    | ActualExpense
    | WithExpenses
    | WithoutExpenses
    | PolicyPaidExpense
    | Asset
    | Global

  let tagToExpectedTag tag =
    match tag with
    | _ when tag = tag_equity                 -> Equity
    | _ when tag = tag_interest               -> Interest
    | _ when tag = tag_risk                   -> Risk
    | _ when tag = tag_expense                -> Expense
    | _ when tag = tag_marketRateInterest     -> MarketRateInterest
    | _ when tag = tag_policy                 -> Policy
    | _ when tag = tag_actualExpense          -> ActualExpense
    | _ when tag = tag_withExpenses           -> WithExpenses
    | _ when tag = tag_withoutExpenses        -> WithoutExpenses
    | _ when tag = tag_policyPaidExpense      -> PolicyPaidExpense
    | _ when tag = tag_asset                  -> Asset
    | _ when tag = tag_global                 -> Global
    | _ -> failwith <| sprintf "Impossible, unhandled baseTag: %s" tag
