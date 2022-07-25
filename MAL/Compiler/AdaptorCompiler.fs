namespace itu.dk.MAL

open Analysis
open SyntaxUtil

module AdaptorCompiler =

  open AST
  open Microsoft.CodeAnalysis
  open Microsoft.CodeAnalysis.CSharp
  open Microsoft.CodeAnalysis.CSharp.Syntax
  open System

  let specialInputHandling = ["LumpedStateParameters"]

  let var_numbersOfProj = "___NumberOfProjs"

  let genAdapterExecutor (analyser : Analyser<'eA>) (compilerOptions : CompilerOptions) =
    let addDebugTests =
      match compilerOptions.optLevel with
      | ToRelease -> false
      | ToDebug -> true

    let executorAdapter = SyntaxFactory.ParseMemberDeclaration(Template.executorAdapterTemplate addDebugTests)
    let originalInitMethod = (SyntaxUtil.getMethod executorAdapter "InnerInitialize")

    let dataStructure = analyser.DataStructure
    let tagMap = dataStructure.combinedTagMap

    let fieldsFromType typ =
      let rec collecter start res =
        match Map.tryFind start dataStructure.superTypes with
        | None -> failwith <| sprintf "Could not find %s" start
        | Some tmp ->
          match tmp with
          | Some super -> collecter super (super::res)
          | None -> res
      let typeList = collecter typ [typ]
      let folder fields entity = Map.fold (fun s k v -> Map.add k v s) fields tagMap.[entity]
      List.fold folder Map.empty typeList

    ///<summary>
    /// Creates a string which assigns
    /// `paramName` of type `typ` from at index in `source` to `paramName`
    /// The simple output is something like:
    /// paramName = source["paramName"].GetNumber()
    /// The function is a bit more complicated to handle optional parameters and
    /// a debug check of whether a parameter exists.
    /// ParamLevel is only used for error messages
    ///</summary>
    let assignParamOnType paramLevel source paramName typ =
      let directIndex source paramName =

        match compilerOptions.optLevel with
        | ToRelease -> sprintf "%s[\"%s\"]" source paramName
        | ToDebug ->
          // When compiling in debug mode we check if the parameter exists
          // to throw a nice error.
          let paramTest = NameGenerator.wishName "paramTest"
          let error = sprintf "%s is expected to have parameter %s" paramLevel paramName

          let typString = FromIntermediate.genTypString
                            dataStructure.subTypes
                            (ToIntermediate.convertType typ)
                            (FromIntermediate.constructorIdentifierOptions)
          sprintf "!%s.TryGetValue(\"%s\", out var %s) ? ThrowMalException<%s>(\"%s\") : %s" source paramName paramTest typString error paramTest

      let simplePostFix typ =
        match typ with
        | TDouble _ -> ".GetNumber()"
        | TStr _ -> ".GetText()"
        | TBool _ -> ".GetBool()"
        | TGeneric(_,GenericList,_) -> ".GetNumberArray().ToArray()"
        | TReserve _ -> ".GetNumber()"
        | TRec (_, ["TimeSeries"]) -> ".GetTimeSeries()"
        | _ -> failwith <| sprintf "Impossible due to below match. \n %A \n" typ
      let rhs =
          match typ with
          | TDouble _
          | TStr _
          | TBool _
          | TGeneric(_,GenericList,_)
          | TReserve _ -> sprintf "%s%s" (directIndex source paramName) (simplePostFix typ)
          | TGeneric(_,GenericOption , [_,innerTyp]) ->
            let typString = FromIntermediate.genTypString
                              dataStructure.subTypes
                              (ToIntermediate.convertType typ)
                              (FromIntermediate.constructorIdentifierOptions)
            let tempName = NameGenerator.wishName "optionalParam"
            sprintf "%s.TryGetValue(\"%s\", out var %s) ? (%s)%s%s : null" source paramName tempName typString tempName (simplePostFix innerTyp)

          | TGeneric(_,GenericOption ,_ )
          | TGeneric(_,GenericSysFun ,_ )
          | TGeneric(_,GenericUnknown _ ,_ ) -> failwith "not possible param"
          | TAct _ | TFun _ | TEnum _ | TGeneric(_,GenericPair ,_ ) -> failwith "not possible param"
          | TErr _ | TMissing _ | TVoid -> failwith "Should not happen"
          | TGeneric(_,GenericMap ,_ )
          | TRec _ -> sprintf "new %s()" <| FromIntermediate.genTypString Map.empty (ToIntermediate.convertType typ) FromIntermediate.malNameOptions
          | TModule (_) -> failwith <| sprintf "%A\n" typ

      sprintf "%s = %s" paramName rhs

    let genSimpParam dictionaryLocation entity =
      let genParam source entity =
        let fields = fieldsFromType entity
        let methods = Map.map (assignParamOnType entity source) fields
        String.Join(",", Seq.map snd <| Map.toSeq methods)

      let transformedLocation = NameGenerator.wishName "customParameters"
      let paramTransform = sprintf "var %s = %s.ConvertParams();" transformedLocation dictionaryLocation
      paramTransform, genParam transformedLocation entity

    let genInput kvp entity =
      let fields = fieldsFromType entity
      let fields = fields.Remove "Transfers"
      let assignVal name _ =
        if List.contains name specialInputHandling
        then ""
        else sprintf "%s = %s.Value.%s" name kvp name
      let methods = Map.map assignVal fields
      String.Join(",", Seq.map snd <| Map.toSeq methods)

    let initType name typ =
      let typToString = FromIntermediate.genTypString Map.empty << ToIntermediate.convertType
      let rhs =
          // Consider how this is different from mempty
          match typ with
          | TDouble _ | TReserve _ -> "0"
          | TStr _ -> "null"
          | TBool _ -> "false"
          | TAct _ -> failwith "hopefully no functions on objects"
          | TGeneric(_,GenericSysFun,_)
          | TFun _ -> "default(TFunction)"
          | TErr _ | TMissing _ | TVoid -> failwith "error"
          | TGeneric(_,GenericList,_) -> "null"
          | TEnum _ -> "0" //failwith "todo no enum init"
          | TGeneric(_,GenericPair ,_ ) -> failwith "todo no pair init"
          | TGeneric(_,GenericMap ,_ )
          | TRec _ -> "null" //sprintf "new %s()" <| typToString typ FromIntermediate.constructorIdentifierOptions
          | TGeneric(_,GenericOption ,_ ) -> "null"
          | TGeneric(_,GenericUnknown _ ,_ ) -> failwith "impossible"
      sprintf "%s = %s" name rhs

    let baseTypes = ["Equity"; "Asset"; "Group"; "Policy"; "Cashflow" ; "Expense" ; "Risk" ; "Interest" ]

    ///<summary>
    /// Generates an object for `entity` initialized with data found on the string `kvp`
    /// `assignString` is the assignment we need to lift out of
    ///</summary>
    let genObject kvp entity assignString =
      // inner is just used to lift a possible preStmt out of the initialization.
      let rec inner kvp entity =
        let fields = fieldsFromType entity
        let decider field typ =
            match field with
            | _ when ASTUtil.isSubOfTyp dataStructure.superTypes typ (nos_TRec ["Param"]) ->
                let typStr = FromIntermediate.genTypString Map.empty (ToIntermediate.convertType typ) FromIntermediate.constructorIdentifierOptions
                let malName = FromIntermediate.genTypString Map.empty (ToIntermediate.convertType typ) FromIntermediate.malNameOptions
                let dictionaryLocation = sprintf "%s.Value.CustomParameters" kvp
                let preStmt, prm = genSimpParam dictionaryLocation malName
                Some preStmt, sprintf "Param = new %s(){%s}" typStr <|prm
            | _ when ASTUtil.isSubOfTyp analyser.DataStructure.superTypes typ (nos_TRec ["CashFlowInput"]) ->
                let typStr = FromIntermediate.genTypString Map.empty (ToIntermediate.convertType typ) FromIntermediate.constructorIdentifierOptions
                let malName = FromIntermediate.genTypString Map.empty (ToIntermediate.convertType typ) FromIntermediate.malNameOptions
                None, sprintf "Input = new %s(){%s}" typStr <| genInput kvp malName
            | _ when field = "Input" ->
                None, sprintf "Input = null"
            | _ when field = "Result" ->
                let typStr = FromIntermediate.genTypString Map.empty (ToIntermediate.convertType typ) FromIntermediate.constructorIdentifierOptions
                None,sprintf "Result = null"
                //None,sprintf "Result = new %s()" typStr
            | _ when field = "Transfers" ->
                None,sprintf "Transfers = null"
            | _ when field = "AssetInput" ->
                None,sprintf "Transfers = null"
            | _ ->
              //let malName = FromIntermediate.genTypString Map.empty (ToIntermediate.convertType typ) FromIntermediate.malNameOptions
              //if Map.containsKey malName tagMap && not <| List.contains malName baseTypes
              //then let typStr = FromIntermediate.genTypString Map.empty (ToIntermediate.convertType typ) FromIntermediate.constructorIdentifierOptions
              //     let preStmt, objInit = (inner kvp malName)
              //     preStmt, sprintf "%s = new %s(){%s}" field typStr objInit
              //else
              None, initType field typ
        let methods = Map.map decider fields
        let entries = Map.toSeq methods
        let preStmts = Seq.choose (fst << snd) entries
        Some <| String.Join("", preStmts), String.Join(",", Seq.map (snd << snd) <| entries)
      let pre, init = inner kvp entity
      let pre =
        match pre with
        | None -> ""
        | Some v -> v
      sprintf "%s%s{%s};" pre assignString init

    let initMethod =
      let preStmt, prm = genSimpParam "input.GlobalCustomParameters" "GlobalParam"
      let globalParam = sprintf "%sexecutorState.Global.Param = new GlobalParam<TFunction>(){%s};" preStmt prm
      let globalParmSyntax = SyntaxFactory.ParseStatement(globalParam)
      originalInitMethod.AddBodyStatements globalParmSyntax

      //let lhsPre = "executorState.Global.Param"
      //let source = "input.GlobalCustomParameters"
      //Map.fold
      //  (fun (method' : MethodDeclarationSyntax) id typ ->
      //    let myStatement = SyntaxFactory.ParseStatement(sprintf "%s.%s;" lhsPre <| assignParamOnType "Global" source id typ);
      //    method'.AddBodyStatements(myStatement)
      //  ) (originalInitMethod.AddBodyStatements(SyntaxFactory.ParseStatement("executorState.Global.Param = new GlobalParam<TFunction>();"))) (tagMap.["GlobalParam"])

    let initMethod =
      initMethod.AddBodyStatements(
          SyntaxFactory.ParseStatement(
                  "{
                  executorState.Projection = new Projection<TFunction>();
                  var executorEquities = new Equity<TFunction>[input.EquityIdParameters != null ? input.EquityIdParameters.Count : 0];
                  var executorGroups = new Group<TFunction>[(input.GroupWithReserveIdParameters != null ? input.GroupWithReserveIdParameters.Count : 0) +
                                                            (input.GroupWithoutReserveIdParameters != null ? input.GroupWithoutReserveIdParameters.Count : 0)];

                  var executorPoliciesP1 = new List<OneStatePolicy<TFunction>>(input.PolicyIdParameters.Count);
                  var executorPoliciesP3 = new List<ThreeStatePolicy<TFunction>>(input.PolicyIdParameters.Count);
                  var executorCashFlowsAE = new List<ActualExpense<TFunction>>(input.PolicyIdParameters.Count);
                  var executorCashFlowsPPE = new List<PolicyPaidExpense<TFunction>>(input.PolicyIdParameters.Count);
                  var executorCashFlowsWE = new List<WithExpenses<TFunction>>(input.PolicyIdParameters.Count);
                  var executorCashFlowsWO = new List<WithoutExpenses<TFunction>>(input.PolicyIdParameters.Count);
                  //var p1Id = 0;
                  //var p3Id = 0;
                  //var interestId = 0;
                  //var expenseId = 0;
                  //var riskId = 0;
                  //var mriId = 0;
                  Dictionary<string, IEquityStateParameters> equityParams = new Dictionary<string, IEquityStateParameters>();
                  int counter = 0;
                  foreach (KeyValuePair<string,IEquityParameters> equity in input.EquityIdParameters ?? Enumerable.Empty<KeyValuePair<string, IEquityParameters>>())
                  {
                      var EquityId = equity.Key;
                      var eqParam = equity.Value;
                      var asCount = eqParam.AssetIdAssetWeight.Count();
                      var polCount = input.PolicyIdParameters.Keys.Where(pol => input.PolicyIdParameters[pol].EquityId == EquityId).Count();
                      "+(genObject "equity" "Equity" "Equity<TFunction> eq = new Equity<TFunction>()")+"
                      eq.Name = EquityId;
                      executorEquities[counter] = eq;
                      eq.Assets = createAssets<EquityType, Equity<TFunction>>(equity.Value, policyEquityColAccum, counter++, eq.Name, eq, out var assets);
                      equityParams.Add(eq.Name, (IEquityStateParameters)new VirtualProjectionState.VirtualEquityStateParameters(eq, assets));
                  }

                  Dictionary<string, IGroupStateParameters> groupParams = new Dictionary<string, IGroupStateParameters>();
                  counter = 0;
                  foreach (KeyValuePair<string,IGroupWithReserveParameters> Group in input.GroupWithReserveIdParameters ?? Enumerable.Empty<KeyValuePair<string, IGroupWithReserveParameters>>())
                  {
                      Group<TFunction> g;
                      string GroupWithReserveId = Group.Key;
                      VirtualProjectionState.VirtualGroupStateParameters virtGroup = null;
                      switch (Group.Value.Type)
                          {
                              case GroupType.Interest:
                                  "+(genObject "Group" "Interest" "Interest<TFunction> i = new Interest<TFunction>()")+"
                                  g = i;
                                  //i.Interest_id = interestId++;
                                  virtGroup = new VirtualProjectionState.VirtualInterestGroupStateParameters(i);
                                  break;
                              case GroupType.Expense:
                                  "+(genObject "Group" "Expense" "Expense<TFunction> e = new Expense<TFunction>()")+"
                                  g = e;
                                  //e.Expense_id = expenseId++;
                                  virtGroup = new VirtualProjectionState.VirtualExpenseGroupStateParameters(e);
                                  break;
                              case GroupType.Risk:
                                  "+(genObject "Group" "Risk" "Risk<TFunction> r = new Risk<TFunction>()")+"
                                  g = r;
                                  //r.Risk_id = riskId++;
                                  virtGroup = new VirtualProjectionState.VirtualRiskGroupStateParameters(r);
                                  break;
                              //case GroupType.MarketRateInterest:
                              //    "+(genObject "Group" "MarketRateInterest" "MarketRateInterest<TFunction> mir = new MarketRateInterest<TFunction>()")+"
                              //    g = mir;
                              //    break;
                              default:
                                  throw new ArgumentException(\"Unknown WithReserveGroup type\");
                          }
                          g.Name = GroupWithReserveId;
                          executorGroups[counter] = g;
                          g.Assets = createAssets(Group.Value, policyGroupColAccum, counter++, g.Name, g, out var assetDict);
                          groupParams.Add(g.Name, virtGroup);
                          virtGroup.assetDict = assetDict;
                  }

                  foreach (KeyValuePair<string, IParameters<GroupType>> Group in input.GroupWithoutReserveIdParameters ?? Enumerable.Empty<KeyValuePair<string, IParameters<GroupType>>>())
                  {
                      Group<TFunction> g;
                      string GroupWithReserveId = Group.Key;
                      VirtualProjectionState.VirtualGroupStateParameters virtGroup = null;
                      switch (Group.Value.Type) {
                          case GroupType.MarketRateInterest:
                              "+(genObject "Group" "MarketRateInterest" "MarketRateInterest<TFunction> mri = new MarketRateInterest<TFunction>()")+"
                              g = mri;
                              //mri.MarketRateInterest_id = mriId++;
                              virtGroup = new VirtualProjectionState.VirtualMarketInterestRateGroupStateParameters(mri);
                              break;
                          default:
                              throw new ArgumentException(\"Unknown WithoutReserveGroup  type\");
                      }
                      g.Name = GroupWithReserveId;
                      executorGroups[counter] = g;
                      g.Assets = createAssets(Group.Value, policyGroupColAccum, counter++, g.Name, g, out var assetDict);
                      groupParams.Add(g.Name, virtGroup);
                      virtGroup.assetDict = assetDict;
                  }

                  counter = 0;
                  int cfCounter = 0;
                  Dictionary<string, IPolicyStateParameters<TFunction>> policyParam = new Dictionary<string, IPolicyStateParameters<TFunction>>();
                  foreach (KeyValuePair<string, IPolicyParameters> kvp in input.PolicyIdParameters ?? Enumerable.Empty<KeyValuePair<string, IPolicyParameters>>())
                  {
                      var policy = kvp.Value;
                      Policy<TFunction> p = null;
                      var GroupsCount = policy.GroupCollection.GroupIds.Count;
                      var policyGroups = new Group<TFunction>[GroupsCount];
                      var grpCollection = Stub.MyGroupCollection.FromIGroupCollection(policy.GroupCollection);
                      if (policy.StateModel == StateModel.Lumped1State)
                      {
                        var CashFlowsCount = policy.CashFlowParameters.Count;
                        "+(genObject "kvp" "OneStatePolicy" "var p1 = new OneStatePolicy<TFunction>")+"
                        p1.CashFlows = convertCashFlows(policy.CashFlowParameters, input.CashFlowNameFreePolicyRule, p1,  executorCashFlowsAE, executorCashFlowsPPE, executorCashFlowsWE, executorCashFlowsWO, out var virtCF);
                        p = p1;
                        //p1.OneStatePolicy_id = p1Id++;
                        executorPoliciesP1.Add(p1);
                        policyParam.Add(kvp.Key, new VirtualProjectionState.VirtualOneStatePolicyStateParameters(p1, virtCF,grpCollection));
                      }
                      else {
                          "+(genObject "kvp" "ThreeStatePolicy" "var p3 = new ThreeStatePolicy<TFunction>()")+"

                          //p3.Input.LumpedStateParameters = new Dictionary<LumpedState,LumpedStateParameters<TFunction>>();
                          var lumpedStates = new Dictionary<LumpedState, ThreeStateState<TFunction>>();
                          var virtLumpedStates = new Dictionary<LumpedState, ILumpedStateStateParameters<TFunction>>();
                          foreach (var lumpedState in kvp.Value.LumpedStateParameters)
                          {
                            //  var initialProbabilities = lumpedState.Value.InitialStateProbabilities.Select(d => d).ToDictionary(pair => pair.Key, pair => pair.Value);
                            //  var parm = new LumpedStateParameters<TFunction>()
                            //  {
                            //    InitialProbability = lumpedState.Value.InitialProbability
                            //                                                   ,
                            //    InitialStateProbabilities = initialProbabilities
                            //  };
                            //  p3.Input.LumpedStateParameters.Add(lumpedState.Key, parm);
                            var tss = new ThreeStateState<TFunction>();
                            tss.State = lumpedState.Key;
                            tss.CashFlows = convertCashFlows(lumpedState.Value.CashFlowParameters, input.CashFlowNameFreePolicyRule, p3,  executorCashFlowsAE, executorCashFlowsPPE, executorCashFlowsWE, executorCashFlowsWO, out var virtCF);
                            lumpedStates[lumpedState.Key] = tss;
                            virtLumpedStates[lumpedState.Key] = new VirtualProjectionState.VirtualThreeStatePolicyStateParameters.VirtualLumpedStateStateParameters(tss, virtCF);
                          }
                          p3.LumpedStates = lumpedStates;
                          //p3States[LumpedState.Biometric].PeriodTechnicalExpenses = new Expenses<TFunction>();
                          //p3States[LumpedState.FreePolicy].PeriodTechnicalExpenses = new Expenses<TFunction>();
                          //p3.Result.LumpedStatePeriodResult = p3States;
                          p = p3;
                          //p3.ThreeStatePolicy_id = p3Id++;
                          executorPoliciesP3.Add(p3);
                          policyParam.Add(kvp.Key, new VirtualProjectionState.VirtualThreeStatePolicyStateParameters(p3, virtLumpedStates, grpCollection));
                      }
                      //var GroupsCount = policy.GroupCollection.GroupIds.Count;
                      //var policyGroups = new Group<TFunction>[GroupsCount];
                      p.Name = kvp.Key;
                      int j = 0;
                      foreach (var groupId in policy.GroupCollection.GroupIds)
                      {
                          var tup = policyGroupColAccum[groupId];
                          if (p is OneStatePolicy<TFunction> p1)
                            tup.Item2.Add(p1);
                          if (p is ThreeStatePolicy<TFunction> p3)
                            tup.Item3.Add(p3);
                          policyGroups[j++] = tup.Item1;
                          groupParams[groupId].PolicyIds.Add(p.Name);
                      }

                      var eqId = policy.EquityId;
                      if (eqId != null && eqId != \"\")
                      {
                          var tup = policyEquityColAccum[eqId];
                          if (p is OneStatePolicy<TFunction> p1)
                            tup.Item2.Add(p1);
                          if (p is ThreeStatePolicy<TFunction> p3)
                            tup.Item3.Add(p3);
                          p.Equity = tup.Item1;
                          equityParams[eqId].PolicyIds.Add(p.Name);
                      }

                      p.Groups = new TypeSpan_Group<TFunction>(policyGroups);
                      if (p.Groups.Expense.Count() > 0)
                      {
                          p.ExpenseGroup = p.Groups.Expense[0];
                      }
                      if (p.Groups.Interest.Count() > 0)
                      {
                          p.InterestGroup = p.Groups.Interest[0];
                      }
                      if (p.Groups.MarketRateInterest.Count() > 0)
                      {
                          p.InterestGroup = p.Groups.MarketRateInterest[0];
                      }
                      p.RiskGroups = p.Groups.Filter_Risk();
                  }

               foreach (var eq in executorEquities)
               {
                   var eqPolicies = policyEquityColAccum[eq.Name];
                   eq.Policies = new TypeSpan_Policy<TFunction>(new TypeSpan_OneStatePolicy<TFunction>(eqPolicies.Item2), new TypeSpan_ThreeStatePolicy<TFunction>(eqPolicies.Item3));
               }

               foreach (var grp in executorGroups)
               {
                   var grpPolicies = policyGroupColAccum[grp.Name];
                   grp.Policies = new TypeSpan_Policy<TFunction>(new TypeSpan_OneStatePolicy<TFunction>(grpPolicies.Item2), new TypeSpan_ThreeStatePolicy<TFunction>(grpPolicies.Item3));
               }

              executorState.Groups = new TypeSpan_Group<TFunction>(executorGroups);
              executorState.Policies = new TypeSpan_Policy<TFunction>(new TypeSpan_OneStatePolicy<TFunction>(executorPoliciesP1), new TypeSpan_ThreeStatePolicy<TFunction>(executorPoliciesP3));
              executorState.Equities = new TypeSpan_Equity<TFunction>(executorEquities);
              executorState.CashFlows = new TypeSpan_CashFlow<TFunction>( new TypeSpan_ActualExpense<TFunction>(executorCashFlowsAE)
                                                                        , new TypeSpan_PolicyPaidExpense<TFunction>(executorCashFlowsPPE)
                                                                        , new TypeSpan_WithExpenses<TFunction>(executorCashFlowsWE)
                                                                        , new TypeSpan_WithoutExpenses<TFunction>(executorCashFlowsWO));

              state = new VirtualProjectionState
                ( executorState
                , input.GlobalCustomParameters
                , (pName) => (cfName) => input.PolicyIdParameters[pName].CashFlowParameters[cfName].CustomParameters
                , (pName) => (lumpedState) => (cfName) => input.PolicyIdParameters[pName].LumpedStateParameters[lumpedState].CashFlowParameters[cfName].CustomParameters
                , equityParams
                , groupParams
                , policyParam
                );
              BuiltIns<TFunction>.State = state;
              //int c = 0;

              //int cols = executorState.Policies.Count();
              //for(int i = 0; i < cols; i++)
              //  executorState.Policies.index(i).Policy_id = c++;
              //c = 0;
              //cols = executorState.Groups.Count();
              //for(int i = 0; i < cols; i++)
              //  executorState.Groups.index(i).Group_id = c++;
              //var rGrps = executorState.Groups.Filter_ReserveGroup();
              //c = 0;
              //cols = rGrps.Count();
              //for(int i = 0; i < cols; i++)
              //  rGrps.index(i).ReserveGroup_id = c++;

              executor.Initialize(input, executorState, "+var_numbersOfProj+");
              calculationPeriodEndTimes = executorState.Global.ProjectionTimes.ToList();
              state.CustomStateValue = (executor, executorState);
              if (executorState.Global.TimeZeroOutputIncluded)
                executor.AddOutput(___projNumber++);
              return state;
            }"
      ))
    let executorAdapter = executorAdapter.ReplaceNode(originalInitMethod, initMethod)
    let originalConvertCashFlowsMethod = (SyntaxUtil.getMethod executorAdapter "convertCashFlows")
    let newConvertCashFlows =
      originalConvertCashFlowsMethod.AddBodyStatements
        (SyntaxFactory.ParseStatement
            ("
            virtualCashFlows = new Dictionary<string, ICashFlowStateParameters>();

            var local_executorCashFlowsAE = new List<ActualExpense<TFunction>>();
            var local_executorCashFlowsPPE = new List<PolicyPaidExpense<TFunction>>();
            var local_executorCashFlowsWE = new List<WithExpenses<TFunction>>();
            var local_executorCashFlowsWO = new List<WithoutExpenses<TFunction>>();

            foreach (var kvp_c in cashFlowParams)
            {
                var CashFlowId = kvp_c.Key;
                var values = kvp_c.Value.BiometricScenarioCashFlow.Values;
                var times = kvp_c.Value.BiometricScenarioCashFlow.TimePoints;
                //var trans = new Transfers(times.ToArray(), values.ToArray());
                CashFlow<TFunction> cf = null;
                switch (kvp_c.Value.Type)
                {
                    case CashFlowType.ActualExpense:
                            "+(genObject "kvp_c" "ActualExpense" "ActualExpense<TFunction> ae = new ActualExpense<TFunction>()")+"
                            ae.Result = new Result<TFunction>();
                            //ae.Transfers = trans;
                        cf = ae;
                        executorCashFlowsAE.Add(ae);
                        local_executorCashFlowsAE.Add(ae);
                        virtualCashFlows.Add(CashFlowId, new VirtualProjectionState.VirtualCashFlowStateParameters(cf, null, kvp_c.Value.CustomParameters ));
                        break;
                    case CashFlowType.WithExpenses:
                        "+(genObject "kvp_c" "WithExpenses" "WithExpenses<TFunction> we = new WithExpenses<TFunction>()")+"
                        we.Result = new Result<TFunction>();
                        //we.Transfers = trans;
                        cf = we;
                        virtualCashFlows.Add(CashFlowId, new VirtualProjectionState.VirtualCashFlowStateParameters(cf, new FunctionalDictionary<string, ICashFlow>(() => we.RiskContributions.Keys, k => we.RiskContributions[k]), kvp_c.Value.CustomParameters ));
                        executorCashFlowsWE.Add(we);
                        local_executorCashFlowsWE.Add(we);
                        break;
                    case CashFlowType.WithoutExpenses:
                        "+(genObject "kvp_c" "WithoutExpenses" "WithoutExpenses<TFunction> wo = new WithoutExpenses<TFunction>()")+"
                        wo.Result = new Result<TFunction>();
                        //wo.Transfers = trans;
                        cf = wo;
                        virtualCashFlows.Add(CashFlowId, new VirtualProjectionState.VirtualCashFlowStateParameters(cf, new FunctionalDictionary<string, ICashFlow>(() => wo.RiskContributions.Keys, k => wo.RiskContributions[k]), kvp_c.Value.CustomParameters ));
                        executorCashFlowsWO.Add(wo);
                        local_executorCashFlowsWO.Add(wo);
                        break;
                    case CashFlowType.PolicyPaidExpense:
                        "+(genObject "kvp_c" "PolicyPaidExpense" "PolicyPaidExpense<TFunction> pe = new PolicyPaidExpense<TFunction>()")+"
                        pe.Result = new Result<TFunction>();
                        //pe.Transfers = trans;
                        cf = pe;
                        virtualCashFlows.Add(CashFlowId, new VirtualProjectionState.VirtualCashFlowStateParameters(cf, null, kvp_c.Value.CustomParameters));
                        executorCashFlowsPPE.Add(pe);
                        local_executorCashFlowsPPE.Add(pe);
                        break;
                    default:
                        throw new ArgumentException(\"Unknown cashflow type\");
                }
                cf.Input.Transfers = kvp_c.Value.BiometricScenarioCashFlow;
                cf.Name = CashFlowId;
                cf.Policy = policy;
                }
                return new TypeSpan_CashFlow<TFunction>( new TypeSpan_ActualExpense<TFunction>(local_executorCashFlowsAE)
                                                       , new TypeSpan_PolicyPaidExpense<TFunction>(local_executorCashFlowsPPE)
                                                       , new TypeSpan_WithExpenses<TFunction>(local_executorCashFlowsWE)
                                                       , new TypeSpan_WithoutExpenses<TFunction>(local_executorCashFlowsWO));
              "))
    let executorAdapter = executorAdapter.ReplaceNode(originalConvertCashFlowsMethod, newConvertCashFlows)

    executorAdapter

