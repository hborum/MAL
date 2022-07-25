namespace itu.dk.MAL

module Template =


  let dictExtend =
    "
      public static class DictExtens
      {
          public static TValue Get<TKey, TValue>(
              this IReadOnlyDictionary<TKey, TValue> dictionary,
              TKey key)
          {
              return dictionary[key];
          }

          public static IDictionary<string, TValue> ConvertParams<TValue>(this IReadOnlyDictionary<string, TValue> dictionary)
          {
              var dict = new Dictionary<string, TValue>();
              foreach(var kvp in dictionary) {
                  dict[kvp.Key.Replace(\" \", \"_\").Replace(\"-\", \"_\")] = kvp.Value;
              }
              return dict;
          }
      }

      static class ArExtension
      {
          public static void Add<T>(this T[] array, int i, T o) { array[i] = o; }

          public static T[] Resize<T>(this T[] array, int size) { return array.Take(size).ToArray(); }
      }

      public class MalException : Exception {
          public MalException(string s) : base(s) { }
      }

      public class MutableTuple<T,U>
      {
          public T Item1 { get; set; }
          public U Item2 { get; set; }
          public MutableTuple(T t, U u)
          {
              this.Item1 = t;
              this.Item2 = u;
          }
      }
      "

  let executorStateTemplate =
    "
      public class ExecutorState<TFunction,TOutput>
        where TFunction : IFunction
        where TOutput : IProjectionOutput
      {
          public IProjectionOutputFactory<TOutput> OutputFactory;
          public Global<TFunction>     Global;
          public Projection<TFunction> Projection;
          public TypeSpan_Equity<TFunction>   Equities;
          public TypeSpan_Group<TFunction>    Groups;
          public TypeSpan_Policy<TFunction>   Policies;
          public TypeSpan_CashFlow<TFunction> CashFlows;
          public int ___projNumber;

          //public ConcurrentDictionary<string,int> EquityIds = new ConcurrentDictionary<string,int>();
          //public ConcurrentDictionary<string,int> GroupIds = new ConcurrentDictionary<string,int>();
          //public ConcurrentDictionary<string,int> PolicyIds = new ConcurrentDictionary<string,int>();

          // Output handling
          public IReadOnlyList<double> ProjectionTimes;
          public Func<double, double> RealisedDiscountFactor;
          public IDictionary<string, ISet<ProjectionOutputEntityAnnotation>> annotations = new Dictionary<string, ISet<ProjectionOutputEntityAnnotation>>();
          public IDictionary<string, double[]> globalTimeDependentValue = new Dictionary<string, double[]>();
          public IDictionary<string, IDictionary<string, double[]>> groupAndTimeDependentValue = new Dictionary<string, IDictionary<string, double[]>>();
          public IDictionary<string, IDictionary<string, double[]>> policyAndTimeDependentValue = new Dictionary<string, IDictionary<string, double[]>>();
          public IDictionary<string, IDictionary<string, double>> groupScalars = new Dictionary<string, IDictionary<string, double>>();
          public IDictionary<string, IDictionary<string, double>> policyScalars = new Dictionary<string, IDictionary<string, double>>();
          public IDictionary<string, double> globalScalars = new Dictionary<string, double>();

          public ExecutorState() {}
      }
    "

  let executorTemplate =
    "
      public class GenExecutor<TFunction, TOutput> : FMAExecutor<ExecutorState<TFunction, TOutput>, TFunction, TOutput>
          where TFunction : IFunction
          where TOutput : IProjectionOutput
      {
          ExecutorState<TFunction, TOutput> state;
          static public IProjectionConfiguration config;
          static IInterpretedProjectionInput<TFunction> input;
          static IProjectionReservesResult result;

          private static  void __transferExpense(IDictionary<ExpenseType, double> exp1, Expenses<TFunction> exp2)
          {
              if(exp2.CashFlowExpense.HasValue)
                  exp1[ExpenseType.CashFlow] = exp2.CashFlowExpense.Value;
              if(exp2.ReserveExpense.HasValue)
                  exp1[ExpenseType.Reserve] = exp2.ReserveExpense.Value;
              if(exp2.PolicyFee.HasValue)
                  exp1[ExpenseType.PolicyFee] = exp2.PolicyFee.Value;
              if(exp2.FreePolicyFee.HasValue)
                  exp1[ExpenseType.FreePolicyFee] = exp2.FreePolicyFee.Value;
              if(exp2.SurrenderFee.HasValue)
                  exp1[ExpenseType.SurrenderFee] = exp2.SurrenderFee.Value;
          }

          private static  void __transferExpense(IReadOnlyDictionary<ExpenseType,double> source, Expenses<TFunction> target)
          {
              double outFloat = 0.0;
              target.CashFlowExpense = source.TryGetValue(ExpenseType.CashFlow, out outFloat)      ? (double?) outFloat : null;
              target.ReserveExpense  = source.TryGetValue(ExpenseType.Reserve, out outFloat)       ? (double?) outFloat : null;
              target.FreePolicyFee   = source.TryGetValue(ExpenseType.FreePolicyFee, out outFloat) ? (double?) outFloat : null;
              target.PolicyFee       = source.TryGetValue(ExpenseType.PolicyFee, out outFloat)     ? (double?) outFloat : null;
              target.SurrenderFee    = source.TryGetValue(ExpenseType.SurrenderFee, out outFloat)  ? (double?) outFloat : null;
          }

          public override void Initialize(IInterpretedProjectionInput<TFunction> _input, ExecutorState<TFunction, TOutput> _state, int ___NumberOfProjs) {
            input = _input;
          }

          public override void Update(IProjectionReservesResult _result, ExecutorState<TFunction, TOutput> _state)
          {
            result = _result;
          }

          public override void AddOutput(int ___ProjNumber) {
          }

          public override TOutput Finalize(IProjectionOutputFactory<TOutput> outputFactory, ExecutorState<TFunction, TOutput> _state)
          {
            var PolicySize = _state.Policies.Count();
            Func<int, string> PolicyFun = (i) => _state.Policies.index(i).Name;

            var GroupSize = _state.Groups.Count();
            Func<int, string> GroupFun = (i) => _state.Groups.index(i).Name;

            var reserveGroups = _state.Groups.Filter_ReserveGroup();
            var ReserveGroupSize = reserveGroups.Count();
            Func<int, string> ReserveGroupFun = (i) => reserveGroups.index(i).Name;

            var expense = _state.Groups.Filter_Expense();
            int ExpenseSize = expense.Count();
            Func<int,string> ExpenseFun = (i) => expense.index(i).Name;

            var risk = _state.Groups.Filter_Risk();
            int RiskSize = risk.Count();
            Func<int,string> RiskFun = (i) => risk.index(i).Name;

            var interest = _state.Groups.Filter_Interest();
            int InterestSize = interest.Count();
            Func<int,string> InterestFun = (i) => interest.index(i).Name;

            var mri = _state.Groups.Filter_MarketRateInterest();
            int mriSize = mri.Count();
            Func<int,string> mriFun = (i) => mri.index(i).Name;

            var RealisedDiscountFactorContinuousPayments  = state.Global.RealisedDiscountFactorContinuousPayments;
            var RealisedDiscountFactorContinuousPaymentsInclPolicyEndTimes  = state.Global.RealisedDiscountFactorContinuousPaymentsInclPolicyEndTimes;
            state.RealisedDiscountFactor = state.Global.RealisedDiscountFactor;

            var outputTimes = state.Global.ProjectionTimes;

            //if (state.Global.TimeZeroOutputIncluded) {
              state.Global.ProjectionTimes = state.Global.ProjectionTimes.Prepend(0.0).ToArray();
            //}

            var cashFlowOutput = new List<IOutputValue<double?[]>>();
            var scalarOutput = new List<IOutputValue<double?>>();
          }

          public void Params()
          {
          }
      }
    "

  let executorAdapterTemplate addDebugTests =
    let insert s =
      if addDebugTests
      then s
      else ""

    "
      public class FMAExampleAdapter<TFunction, TOutput> : IProjectionStateHandler<TFunction, TOutput, IProjectionReservesResult>
          where TFunction : IFunction
          where TOutput : IProjectionOutput
      {
        /// <summary>
        /// The VirtualProjectionState wraps a generated ExecutorState in the IProjectionState interface
        /// Only one virtual projectionState is created during a projection, so we need to delay some calculations until after initialization has occurred.
        /// All Virtual*States work by taking in a setteable
        /// Todo: We probably also need to mark collections whos keyset may change during Update.
        /// </summary>
        public class VirtualProjectionState : IProjectionState<TFunction>
        {
          readonly IPolicyState<TFunction> policies;
          readonly IGroupState groups;
          readonly IEquityState equities;
          object custom;
        public VirtualProjectionState(
          ExecutorState<TFunction, TOutput> state
        , IReadOnlyDictionary<string, IParameterValue> customGlobalParam
        , Func<string, Func<string, IReadOnlyDictionary<string, IParameterValue>>> OneStateCashFlowParam
        , Func<string, Func<LumpedState, Func<string, IReadOnlyDictionary<string, IParameterValue>>>> ThreeStateCashFlowParam
        , IReadOnlyDictionary<string, IEquityStateParameters> equityParams
        , IReadOnlyDictionary<string, IGroupStateParameters> groupParams
        , IReadOnlyDictionary<string, IPolicyStateParameters<TFunction>> policyParams
        )
      {
        this.customGlobalParameters = customGlobalParam; // This is used for dynamic lookup in builtins
        policies = new PolicyStateAdaptor { SettableParams = policyParams };
        groups = new GroupStateAdaptor { SettableParams = groupParams };
        equities = new EquityStateAdaptor { SettableParams = equityParams, DelayedBenefitCashFlows = () => state.Global.MAL_BenefitsReceivingCashFlows.ToDictionary(kvp => kvp.Key.MAL_Name, kvp => (IList<string>)kvp.Value.ToList()), DelayedPremiumCashFlows = () => state.Global.MAL_PremiumPayingCashFlows.ToDictionary(kvp => kvp.Key.MAL_Name, kvp => (IList<string>)kvp.Value.ToList()) };
      }

      IPolicyState<TFunction> IProjectionState<TFunction>.PolicyState => policies;
      IGroupState IProjectionState<TFunction>.GroupState => groups;
      IEquityState IProjectionState<TFunction>.EquityState => equities;
      IReadOnlyDictionary<string, IParameterValue> customGlobalParameters;
      IReadOnlyDictionary<string, IParameterValue> IProjectionState<TFunction>.CustomGlobalParameters => customGlobalParameters;
      object IProjectionState<TFunction>.CustomStateValue
      {
        get => custom;
        set => custom = value;
      }

      class PolicyStateAdaptor : IPolicyState<TFunction>
      {
        public IReadOnlyDictionary<string, IPolicyStateParameters<TFunction>> SettableParams
        {
          get;
          set;
        }

        public IReadOnlyDictionary<string, IPolicyStateParameters<TFunction>> Parameters => SettableParams;
      }

      class GroupStateAdaptor : IGroupState
      {
        public IReadOnlyDictionary<string, IGroupStateParameters> SettableParams
        {
          get;
          set;
        }

        public IReadOnlyDictionary<string, IGroupStateParameters> Parameters => SettableParams;
      }

      class EquityStateAdaptor : IEquityState
      {
        public IReadOnlyDictionary<string, IEquityStateParameters> SettableParams
        {
          get;
          set;
        }

        public IReadOnlyDictionary<string, IEquityStateParameters> Parameters => SettableParams;
        public Func<IReadOnlyDictionary<string, IList<string>>> DelayedPremiumCashFlows;
        private IReadOnlyDictionary<string, IList<string>> SettablePremiumCashFlows
        {
          get;
          set;
        }

        public IReadOnlyDictionary<string, IList<string>> PremiumPayingCashFlows
        {
          get
          {
            if (SettablePremiumCashFlows == null)
              SettablePremiumCashFlows = DelayedPremiumCashFlows.Invoke();
            return SettablePremiumCashFlows;
          }
        }

        private IReadOnlyDictionary<string, IList<string>> SettableBenefitCashFlows
        {
          get;
          set;
        }

        public Func<IReadOnlyDictionary<string, IList<string>>> DelayedBenefitCashFlows;
        public IReadOnlyDictionary<string, IList<string>> BenefitsReceivingCashFlows
        {
          get
          {
            if (SettableBenefitCashFlows == null)
              SettableBenefitCashFlows = DelayedBenefitCashFlows.Invoke();
            return SettableBenefitCashFlows;
          }
        }
      }

      public class VirtualCashFlowStateParameters : ICashFlowStateParameters
      {
          CashFlow<TFunction> source;
          readonly IDictionary<string, ICashFlow> risks;
          IReadOnlyDictionary<string, IParameterValue> customParam;
          public VirtualCashFlowStateParameters(CashFlow<TFunction> cashFlow
                                                , IDictionary<string, ICashFlow> risks
                                                , IReadOnlyDictionary<string, IParameterValue> customParam)
          {
              source = cashFlow;
              this.risks = risks;
              this.customParam = customParam;
          }

          public double ScalingFactor
          {
              get => source.ScalingFactor;
              set => source.ScalingFactor = value;
          }

          public IDictionary<string, ICashFlow> RiskGroupIdRiskContribution => risks;
          public IReadOnlyDictionary<string, IParameterValue> CustomParameters => customParam;
      }

      public class VirtualOneStatePolicyStateParameters : IPolicyStateParameters<TFunction>
      {
       private readonly OneStatePolicy<TFunction> source;
       private readonly IReadOnlyDictionary<string, ICashFlowStateParameters> cashflowParams;
       private readonly IStateGroupCollection groups;
        public VirtualOneStatePolicyStateParameters(OneStatePolicy<TFunction> policy, IReadOnlyDictionary<string, ICashFlowStateParameters> cashFlowCustomParam, IStateGroupCollection groups)
         {
             source = policy;
             this.cashflowParams = cashFlowCustomParam;
             this.groups = groups;
         }


        public double Reserve
        {
          get => source.Reserve;
          set => source.Reserve = value;
        }

        public string EquityId => source.Equity?.Name;
        public IStateGroupCollection GroupCollection => groups;
        public IReadOnlyDictionary<string, ICashFlowStateParameters> CashFlowParameters => cashflowParams;
        public IReadOnlyDictionary<LumpedState, ILumpedStateStateParameters<TFunction>> LumpedStateParameters => null;
        public TFunction FreePolicyFraction
        {
          get => default(TFunction);
          set => throw OneStateAccessError(\"FreePolicyFraction\");
        }

        public double Size
        {
          get => 1;
          set => throw OneStateAccessError(\"Size\");
        }

        public IReadOnlyDictionary<string, IParameterValue> CustomParameters => throw new NotImplementedException();
        public void MoveEquity(string equityId)
        {
          throw new NotImplementedException();
        }
      }

      public class VirtualThreeStatePolicyStateParameters : IPolicyStateParameters<TFunction>
      {
        public class VirtualLumpedStateStateParameters : ILumpedStateStateParameters<TFunction>
        {

          ThreeStateState<TFunction> source;
          IReadOnlyDictionary<string, ICashFlowStateParameters> cashflowParams;
          public VirtualLumpedStateStateParameters(ThreeStateState<TFunction> threeStateState, IReadOnlyDictionary<string, ICashFlowStateParameters> cashflowParams)
          {
              source = threeStateState;
              this.cashflowParams = cashflowParams;
          }

          public double Reserve
          {
            get => source.Reserve;
            set => source.Reserve = value;
          }

          public IReadOnlyDictionary<string, ICashFlowStateParameters> CashFlowParameters => cashflowParams;
          public IReadOnlyDictionary<LumpedStateWithSurrender, TFunction> DestinationLumpedStateIntensity
          {
            get => source.DestinationLumpedStateIntensity;
            set => source.DestinationLumpedStateIntensity = value;
          }

          public IReadOnlyDictionary<Tuple<State, State>, TFunction> IntensitiesBiometric
          {
            get => source.IntensitiesBiometric;
            set => source.IntensitiesBiometric = value;
          }
        }

        private readonly ThreeStatePolicy<TFunction> source;
        private readonly IReadOnlyDictionary<LumpedState, ILumpedStateStateParameters<TFunction>> lumpedStateParameters;
        private readonly IStateGroupCollection groups;
        public VirtualThreeStatePolicyStateParameters( ThreeStatePolicy<TFunction> policy
                          , IReadOnlyDictionary<LumpedState, ILumpedStateStateParameters<TFunction>> lumpedStateParameters
                          , IStateGroupCollection groups)
        {
          source = policy;
          this.lumpedStateParameters = lumpedStateParameters;
          this.groups = groups;
        }

        public double Reserve
        {
          get => 0;
          set => throw ThreeStateAccessError(\"Reserve\");
        }

        public string EquityId => source.Equity?.Name;
        public IStateGroupCollection GroupCollection => groups;
        public IReadOnlyDictionary<string, ICashFlowStateParameters> CashFlowParameters => new Dictionary<string, ICashFlowStateParameters>();
        public IReadOnlyDictionary<LumpedState, ILumpedStateStateParameters<TFunction>> LumpedStateParameters => lumpedStateParameters;
        public TFunction FreePolicyFraction
        {
          get => source.FreePolicyFraction;
          set => source.FreePolicyFraction = value;
        }

        public double Size
        {
          get => source.Size;
          set => source.Size = value;
        }

        public IReadOnlyDictionary<string, IParameterValue> CustomParameters => throw new NotImplementedException();
        public void MoveEquity(string equityId)
        {
          throw new NotImplementedException();
        }
      }

      public abstract class VirtualGroupStateParameters : IGroupStateParameters
      {
        readonly Group<TFunction> source;
        public ISet<string> policyNames = new HashSet<string>();
        public IDictionary<string ,Asset<TFunction>> assetDict;
        readonly IDictionary<string, double> assetWeights;
        readonly IDictionary<string, double> assetFees;
        public VirtualGroupStateParameters(Group<TFunction> group)
        {
            source = group;
            assetWeights = new FunctionalDictionary<string, double>(() => assetDict.Keys, assetName => assetDict[assetName].Weight);
            assetFees = new FunctionalDictionary<string, double>(() => assetDict.Keys, assetName => assetDict[assetName].InvestmentFee);
        }

        public IDictionary<string, double> AssetIdAssetWeight => assetWeights;
        public IDictionary<string, double> AssetIdAssetInvestmentFee => assetFees;
        public ISet<string> PolicyIds => policyNames;
        public IReadOnlyDictionary<string, IParameterValue> CustomParameters => throw new NotImplementedException();
        public IReadOnlyDictionary<string, IReadOnlyDictionary<string, IParameterValue>> CustomAssetParameters => throw new NotImplementedException();
      }

      public class VirtualInterestGroupStateParameters : VirtualGroupStateParameters, IInterestGroupStateParameters
      {
        readonly Interest<TFunction> source;
        public VirtualInterestGroupStateParameters(Interest<TFunction> interest) : base(interest)
        {
          source = interest;
        }

        public double SurrenderCharge
        {
          get => source.SurrenderCharge.GetValueOrDefault();
          set => source.SurrenderCharge = value;
        }

        public double DepositRate
        {
          get => source.DepositRate;
          set => source.DepositRate = value;
        }

        public double Reserve
        {
          get => source.Reserve;
          set => source.Reserve = value;
        }
      }

      public class VirtualRiskGroupStateParameters : VirtualGroupStateParameters, IRiskGroupStateParameters
      {
        readonly Risk<TFunction> source;
        IDictionary<LumpedState, double> riskDividends;
        public VirtualRiskGroupStateParameters(Risk<TFunction> risk) : base(risk)
        {
          source = risk;
          riskDividends = new FunctionalDictionary<LumpedState, double>(() => { return source.LumpedRiskDividend.Keys; }, ls => source.LumpedRiskDividend[ls]);
        }

        public double RiskDividend
        {
          get => source.RiskDividend;
          set => source.RiskDividend = value;
        }

        public IDictionary<LumpedState, double> LumpedStateRiskDividend => riskDividends;
        public double Reserve
        {
          get => source.Reserve;
          set => source.Reserve = value;
        }
      }

      public class VirtualExpenseGroupStateParameters : VirtualGroupStateParameters, IExpenseGroupStateParameters
      {
        readonly Expense<TFunction> source;
        IReadOnlyDictionary<LumpedState, IDictionary<ExpenseType, double>> lumpedExpenseDividends;
        IDictionary<ExpenseType, double> technicalExpenses;
        IDictionary<ExpenseType, double> expenseDividends;
        private static IEnumerable<ExpenseType> getTypes(Expenses<TFunction> expense)
        {
          var res = new List<ExpenseType>();
          if (expense.CashFlowExpense.HasValue)
            res.Add(ExpenseType.CashFlow);
          if (expense.ReserveExpense.HasValue)
            res.Add(ExpenseType.Reserve);
          if (expense.PolicyFee.HasValue)
            res.Add(ExpenseType.PolicyFee);
          if (expense.FreePolicyFee.HasValue)
            res.Add(ExpenseType.FreePolicyFee);
          if (expense.SurrenderFee.HasValue)
            res.Add(ExpenseType.SurrenderFee);
          return res;
        }

        private static double readOnType(Expenses<TFunction> exp, ExpenseType typ)
        {
          switch (typ)
          {
            case ExpenseType.CashFlow:
              return exp.CashFlowExpense.Value;
            case ExpenseType.Reserve:
              return exp.ReserveExpense.Value;
            case ExpenseType.PolicyFee:
              return exp.PolicyFee.Value;
            case ExpenseType.FreePolicyFee:
              return exp.FreePolicyFee.Value;
            case ExpenseType.SurrenderFee:
              return exp.SurrenderFee.Value;
            default:
              throw new ArgumentException(\"Unknown expense type.\");
          }
        }

        public VirtualExpenseGroupStateParameters(Expense<TFunction> expense) : base(expense)
        {
          source = expense;
          LumpedExpenseGen =
              () => source.LumpedExpenseDividends.Keys.ToDictionary(ls => ls, ls => (IDictionary<ExpenseType, double>)new FunctionalDictionary<ExpenseType, double>(() => getTypes(source.LumpedExpenseDividends[ls]), typ => readOnType(source.LumpedExpenseDividends[ls], typ)));
          technicalExpenses = new FunctionalDictionary<ExpenseType, double>(() => getTypes(source.TechnicalExpense), typ => readOnType(source.TechnicalExpense, typ));
          expenseDividends = new FunctionalDictionary<ExpenseType, double>(() => getTypes(source.ExpenseDividends), typ => readOnType(source.ExpenseDividends, typ));
        }

        public IDictionary<ExpenseType, double> ExpenseTypeTechnicalExpense => technicalExpenses;
        public IDictionary<ExpenseType, double> ExpenseTypeExpenseDividend => expenseDividends;
        public Func<IReadOnlyDictionary<LumpedState, IDictionary<ExpenseType, double>>> LumpedExpenseGen;
        public IReadOnlyDictionary<LumpedState, IDictionary<ExpenseType, double>> LumpedStateExpenseTypeExpenseDividend
        {
          get
          {
            if (lumpedExpenseDividends == null)
              lumpedExpenseDividends = LumpedExpenseGen();
            return lumpedExpenseDividends;
          }
        }

        public double Reserve
        {
          get => source.Reserve;
          set => source.Reserve = value;
        }
      }

      public class VirtualMarketInterestRateGroupStateParameters : VirtualGroupStateParameters, IMarketRateInterestGroupStateParameters
      {
        public VirtualMarketInterestRateGroupStateParameters(MarketRateInterest<TFunction> mir) : base(mir)
        {
        }
      }

      public class VirtualEquityStateParameters : IEquityStateParameters
      {
        readonly Equity<TFunction> source;
        public ISet<string> policyNames = new HashSet<string>();
        readonly IDictionary<string, double> assetWeights;
        readonly IDictionary<string, double> assetFees;

        public VirtualEquityStateParameters(Equity<TFunction> equity, IDictionary<string, Asset<TFunction>> assetDict)
        {
            source = equity;
            assetWeights = new FunctionalDictionary<string, double>(assetDict.Keys, assetName => assetDict[assetName].Weight);
            assetFees = new FunctionalDictionary<string, double>(assetDict.Keys, assetName => assetDict[assetName].InvestmentFee);
        }

        public double Reserve
        {
          get => source.Reserve;
          set => source.Reserve = value;
        }

        public IDictionary<string, double> AssetIdAssetWeight => assetWeights;
        public IDictionary<string, double> AssetIdAssetInvestmentFee => assetFees;
        public ISet<string> PolicyIds => policyNames;
        public double PremiumScale
        {
          get => source.PremiumScale;
          set => source.PremiumScale = value;
        }

        public double BenefitScale
        {
          get => source.BenefitScale;
          set => source.BenefitScale = value;
        }

        public IReadOnlyDictionary<string, IParameterValue> CustomParameters => throw new NotImplementedException();
        public IReadOnlyDictionary<string, IReadOnlyDictionary<string, IParameterValue>> CustomAssetParameters => throw new NotImplementedException();
      }

      static readonly Func<string, FieldAccessException> OneStateAccessError = field => new FieldAccessException($\"OneStatePolicy does not contain the field: {field}\");
      static readonly Func<string, FieldAccessException> ThreeStateAccessError = field => new FieldAccessException($\"ThreeStatePolicy does not contain the field: {field}\");
    }

          GenExecutor<TFunction, TOutput> executor;
          ExecutorState<TFunction, TOutput> executorState = new ExecutorState<TFunction, TOutput>();
          IProjectionOutputFactory<TOutput> OutputFactory;
          IProjectionStateFactory<TFunction> StateFactory;
          IProjectionState<TFunction> state;

          int ___projNumber = 0;
          public void ResetProjNumber() { ___projNumber = 0; } //This is used in benchmarking, to not overflow output arrays
          private T ThrowMalException<T>(string s)
          {
             throw new MalException(s);
          }

          public FMAExampleAdapter()
          {
              executor = new GenExecutor<TFunction, TOutput>();
          }

          // This is a preprocessing step used by customers.
          // For MAL we use an identity method
          public IProjectionReservesResult PreUpdate(IProjectionReservesResult result, IProjectionState<TFunction> state)
          {
              return result;
          }

          public void SetupDependencies(IProjectionStateHandlerDependencies<TFunction, TOutput> dependencies)
          {
            BuiltIns<TFunction>.CashFlowFactory = dependencies.CashFlowFactory;
            BuiltIns<TFunction>.ReserveSequenceFactory = dependencies.ReserveSequenceFactory;
            BuiltIns<TFunction>.FunctionFactory = dependencies.FunctionFactory;
            BuiltIns<TFunction>.CashFlowCalculator = dependencies.CashFlowCalculator;
            BuiltIns<TFunction>.StateFactory = dependencies.StateFactory;
            OutputFactory = dependencies.OutputFactory;
          }

          public TypeSpan_CashFlow<TFunction> convertCashFlows(
             IReadOnlyDictionary<string, ICashFlowParameters> cashFlowParams
             , IReadOnlyDictionary<string, FreePolicyRule> cashflowRules
             , Policy<TFunction> policy
             , List<ActualExpense<TFunction>> executorCashFlowsAE
             , List<PolicyPaidExpense<TFunction>> executorCashFlowsPPE
             , List<WithExpenses<TFunction>> executorCashFlowsWE
             , List<WithoutExpenses<TFunction>> executorCashFlowsWO
             , out Dictionary<string, ICashFlowStateParameters> virtualCashFlows
             )
          {
          }

          private static void __transferExpense(IDictionary<ExpenseType, double> exp1, Expenses<TFunction> exp2)
          {
              if(exp2.CashFlowExpense.HasValue)
                  exp1[ExpenseType.CashFlow] = exp2.CashFlowExpense.Value;
              if(exp2.ReserveExpense.HasValue)
                  exp1[ExpenseType.Reserve] = exp2.ReserveExpense.Value;
              if(exp2.PolicyFee.HasValue)
                  exp1[ExpenseType.PolicyFee] = exp2.PolicyFee.Value;
              if(exp2.FreePolicyFee.HasValue)
                  exp1[ExpenseType.FreePolicyFee] = exp2.FreePolicyFee.Value;
              if(exp2.SurrenderFee.HasValue)
                  exp1[ExpenseType.SurrenderFee] = exp2.SurrenderFee.Value;
          }

          private static void __transferExpense(IReadOnlyDictionary<ExpenseType,double> source, Expenses<TFunction> target)
          {
              double outFloat = 0.0;
              target.CashFlowExpense = source.TryGetValue(ExpenseType.CashFlow, out outFloat)      ? (double?) outFloat : null;
              target.ReserveExpense  = source.TryGetValue(ExpenseType.Reserve, out outFloat)       ? (double?) outFloat : null;
              target.FreePolicyFee   = source.TryGetValue(ExpenseType.FreePolicyFee, out outFloat) ? (double?) outFloat : null;
              target.PolicyFee       = source.TryGetValue(ExpenseType.PolicyFee, out outFloat)     ? (double?) outFloat : null;
              target.SurrenderFee    = source.TryGetValue(ExpenseType.SurrenderFee, out outFloat)  ? (double?) outFloat : null;
          }

          public void assertKeyExists<T,U>(IReadOnlyDictionary<T,U> dict, T key, string collectionName)
          {
              if (!dict.ContainsKey(key))
                  throw new MalException($\"The dictionary `{collectionName}` was expected to contain `{key}`\");
          }
          public void assertKeyExists<T,U>(IDictionary<T,U> dict, T key, string collectionName)
          {
              if (!dict.ContainsKey(key))
                  throw new MalException($\"The dictionary `{collectionName}` was expected to contain `{key}`\");
          }
          public void assertNotNull<T>(T t, string name)
          {
              if (t == null)
                throw new MalException($\"The object `{name}` is null\");
          }

          public IProjectionState<TFunction> UpdateState()
          {
              for (var j = 0; j < executorState.Equities.Count(); j++)
              {
                  var eq = executorState.Equities[j];
                  " + (insert "assertKeyExists(state.EquityState.Parameters, eq.Name, \"Edlund.EquityState.Parameters\");" ) + "
                  var edl_eq = state.EquityState.Parameters[eq.Name];
                  for (int i = 0; i < eq.Assets.Count(); i++)
                  {
                      var asset = eq.Assets[i];
                  " + (insert "assertKeyExists(edl_eq.AssetIdAssetWeight, asset.Name, \"edl_eq.AssetIdAssetWeight\");" ) + "
                  " + (insert "assertKeyExists(edl_eq.AssetIdAssetInvestmentFee, asset.Name, \"edl_eq.AssetIdAssetInvestmentFee\");" ) + "
                      edl_eq.AssetIdAssetWeight[asset.Name] = asset.Weight;
                      edl_eq.AssetIdAssetInvestmentFee[asset.Name] = asset.InvestmentFee;
                  }
                  edl_eq.BenefitScale = eq.BenefitScale;
                  edl_eq.Reserve = eq.Reserve;
                  edl_eq.PremiumScale = eq.PremiumScale;
              }

              for (var j = 0; j < executorState.Groups.Count(); j++)
              {
                  var group = executorState.Groups[j];
                  " + (insert "assertKeyExists(state.GroupState.Parameters, group.Name, \"state.GroupState.Parameters\");" ) + "
                  var edl_group = state.GroupState.Parameters[group.Name];
                  for (int i = 0; i < group.Assets.Count(); i++)
                  {
                      var asset = group.Assets[i];
                      " + (insert "assertKeyExists(edl_group.AssetIdAssetWeight, asset.Name, \"edl_group.AssetIdAssetWeight\");" ) + "
                      " + (insert "assertKeyExists(edl_group.AssetIdAssetInvestmentFee, asset.Name, \"edl_group.AssetIdAssetInvestmentFee\");" ) + "
                      edl_group.AssetIdAssetWeight[asset.Name] = asset.Weight;
                      edl_group.AssetIdAssetInvestmentFee[asset.Name] = asset.InvestmentFee;
                  }
                  if (edl_group is IInterestGroupStateParameters edl_interest)
                  {
                      var interest = (Interest<TFunction>) group;
                      edl_interest.DepositRate = interest.DepositRate;
                      edl_interest.Reserve = interest.Reserve;
                      edl_interest.SurrenderCharge = interest.SurrenderCharge.GetValueOrDefault();
                  }
                  else if (edl_group is IMarketRateInterestGroupStateParameters edl_market) {
                      var market = (MarketRateInterest<TFunction>)group;
                      // MarketRateInterest is empty atm
                  }
                  else if (edl_group is IRiskGroupStateParameters edl_risk) {
                      var risk = (Risk<TFunction>)group;
                      edl_risk.Reserve = risk.Reserve;
                      edl_risk.RiskDividend = risk.RiskDividend;
                      edl_risk.LumpedStateRiskDividend[LumpedState.Biometric] = risk.LumpedRiskDividend[LumpedState.Biometric];
                      edl_risk.LumpedStateRiskDividend[LumpedState.FreePolicy] = risk.LumpedRiskDividend[LumpedState.FreePolicy];
                  } else if (edl_group is IExpenseGroupStateParameters edl_exp) {
                      var expense = (Expense<TFunction>)group;
                      __transferExpense(edl_exp.ExpenseTypeTechnicalExpense, expense.TechnicalExpense);
                      __transferExpense(edl_exp.ExpenseTypeExpenseDividend, expense.ExpenseDividends);
                      if (expense.LumpedExpenseDividends.ContainsKey(LumpedState.Biometric))
                        __transferExpense(edl_exp.LumpedStateExpenseTypeExpenseDividend[LumpedState.Biometric], expense.LumpedExpenseDividends[LumpedState.Biometric]);
                      if (expense.LumpedExpenseDividends.ContainsKey(LumpedState.FreePolicy))
                        __transferExpense(edl_exp.LumpedStateExpenseTypeExpenseDividend[LumpedState.FreePolicy], expense.LumpedExpenseDividends[LumpedState.FreePolicy]);
                      edl_exp.Reserve = expense.Reserve;
                  } else {
                      throw new Exception(\"Unknown group type when creating state\");
                  }
              }

              for (var j = 0; j < executorState.Policies.Count(); j++)
              {
                  var policy = executorState.Policies[j];
                  " + (insert "assertKeyExists(state.PolicyState.Parameters, policy.Name, \"state.PolicyState.Parameters\");" ) + "
                  var edl_policy = state.PolicyState.Parameters[policy.Name];
                  if (policy is OneStatePolicy<TFunction> p1)
                  {
                      edl_policy.Reserve = p1.Reserve;
                      for (int i = 0; i < p1.CashFlows.Count(); i++)
                      {
                          var cashFlow = p1.CashFlows[i];
                          " + (insert "assertKeyExists(edl_policy.CashFlowParameters, cashFlow.Name, \"edl_policy.CashFlowParameters\");" ) + "
                          var edl_cashFlow = edl_policy.CashFlowParameters[cashFlow.Name];
                          edl_cashFlow.ScalingFactor = cashFlow.ScalingFactor;
                          IReadOnlyDictionary<String, ICashFlow> risks = null;
                          if (cashFlow is ActualExpense<TFunction> actualExpense)
                          {
                          }

                          if (cashFlow is WithExpenses<TFunction> withExpenses)
                          {
                              risks = withExpenses.RiskContributions;
                          }

                          if (cashFlow is WithoutExpenses<TFunction> withoutExpenses)
                          {
                              risks = withoutExpenses.RiskContributions;
                          }

                          if (cashFlow is PolicyPaidExpense<TFunction> policyPaidExpense)
                          {
                          }

                          if (risks != null)
                          {
                              foreach (var key in risks.Keys)
                              {
                                  " + insert "assertNotNull(edl_cashFlow.RiskGroupIdRiskContribution, \"edl_cashFlow.RiskGroupIdRiskContribution\");" + "
                                  edl_cashFlow.RiskGroupIdRiskContribution[key] = (ICashFlow)risks[key];
                              }
                          }
                      }
                  } else if (policy is ThreeStatePolicy<TFunction> policy3)
                  {
                      edl_policy.Size = policy3.Size;
                      foreach (var lumpedState in policy3.LumpedStates)
                      {
                          " + (insert "assertKeyExists(edl_policy.LumpedStateParameters, lumpedState.Key, \"edl_policy.LumpedStateParameters\");" ) + "
                          var edl_lumpedState = edl_policy.LumpedStateParameters[lumpedState.Key];
                          edl_lumpedState.Reserve = lumpedState.Value.Reserve;
                          edl_lumpedState.DestinationLumpedStateIntensity = (IReadOnlyDictionary<LumpedStateWithSurrender, TFunction>) lumpedState.Value.DestinationLumpedStateIntensity;
                          edl_lumpedState.IntensitiesBiometric = (IReadOnlyDictionary<Tuple<State, State>, TFunction>) lumpedState.Value.IntensitiesBiometric;
                          for (int i = 0; i < lumpedState.Value.CashFlows.Count(); i++)
                          {
                              var cashFlow = lumpedState.Value.CashFlows[i];
                              " + (insert "assertKeyExists(edl_lumpedState.CashFlowParameters, cashFlow.Name, \"edl_lumpedState.CashFlowParameters\");" ) + "
                              var edl_cashFlow = edl_lumpedState.CashFlowParameters[cashFlow.Name];
                              edl_cashFlow.ScalingFactor = cashFlow.ScalingFactor;
                              IReadOnlyDictionary<String, ICashFlow> risks = null;
                              if (cashFlow is ActualExpense<TFunction> actualExpense)
                              {
                              }

                              if (cashFlow is WithExpenses<TFunction> withExpenses)
                              {
                                  risks = withExpenses.RiskContributions;
                              }

                              if (cashFlow is WithoutExpenses<TFunction> withoutExpenses)
                              {
                                  risks = withoutExpenses.RiskContributions;
                              }

                              if (cashFlow is PolicyPaidExpense<TFunction> policyPaidExpense)
                              {
                              }

                              if (risks != null)
                              {
                                  foreach (var key in risks.Keys)
                                  {
                                      " + insert "assertNotNull(edl_cashFlow.RiskGroupIdRiskContribution, \"edl_cashFlow.RiskGroupIdRiskContribution\"); " + "
                                      edl_cashFlow.RiskGroupIdRiskContribution[key] = (ICashFlow)risks[key];
                                  }
                              }
                          }
                      }
                  }
              }
              return state;
          }

          public static Type[] typeOfAssets = new[]{typeof(Asset<TFunction>)};

          public TypeSpan_Asset<TFunction> createAssets<T, U>
            ( IParameters<T> grp //or equity
            , Dictionary<string, Tuple<U,List<OneStatePolicy<TFunction>>,List<ThreeStatePolicy<TFunction>>>> policyColAccum, int index, string identifier, U polAccum //Equity or group
            , out IDictionary<string, Asset<TFunction>> assets)
          {
              var asCount = grp.AssetIdAssetWeight.Count();
              var polCount = grp.PolicyIds.Count();
              var groupPolicies = new Policy<TFunction>[polCount];
              policyColAccum[identifier] = Tuple.Create(polAccum, new List<OneStatePolicy<TFunction>>(), new List<ThreeStatePolicy<TFunction>>());
              var groupAssets = new Asset<TFunction>[asCount];
              int j = 0;
            assets = new Dictionary<string, Asset<TFunction>>();
            foreach  (var asset in grp.AssetIdAssetWeight)
            {
                Asset<TFunction> a = new Asset<TFunction>{Name = asset.Key, Weight = asset.Value, Input = new AssetInput<TFunction>{Weight = asset.Value, InvestmentFee = grp.AssetIdAssetInvestmentFee.Get(asset.Key)}, Param = new Param<TFunction>(), Result = new Result<TFunction>()};
                assets.Add(asset.Key, a);
                groupAssets[j++] = a;
            }

            return new TypeSpan_Asset<TFunction>(groupAssets);
          }

          public IProjectionState<TFunction> Initialize(IInterpretedProjectionInput<TFunction> input, IProjectionConfiguration configuration, out IReadOnlyList<double> calculationPeriodEndTimes)
          {
            try {
              return InnerInitialize(input, configuration, out calculationPeriodEndTimes);
            } catch (Exception e) {
              // Get stack trace for the exception with source file information
              var st = new StackTrace(e, true);

              for (int i = 0; i < st.FrameCount; i++) {
                // Get the top stack frame
                var frame = st.GetFrame(i);
                // Get the line number from the stack frame
                var line = frame.GetFileLineNumber();
                Console.WriteLine(\"Error \" + frame.GetMethod() + \" at \" + line + \":\" + frame.GetFileColumnNumber());
              }

              Console.WriteLine(e.ToString());
              throw;
            }
          }

          public IProjectionState<TFunction> Update(IProjectionState<TFunction> state, IProjectionReservesResult result)
          {
            try {
              return InnerUpdate(state, result);
            } catch (Exception e) {
              // Get stack trace for the exception with source file information
              var st = new StackTrace(e, true);

              for (int i = 0; i < st.FrameCount; i++) {
                // Get the top stack frame
                var frame = st.GetFrame(i);
                // Get the line number from the stack frame
                var line = frame.GetFileLineNumber();
                Console.WriteLine(\"Error \" + frame.GetMethod() + \" at \" + line + \":\" + frame.GetFileColumnNumber());
              }

              Console.WriteLine(e.ToString());
              throw;
            }
          }

          /// <summary>
          /// 1. Creates an ExecutorState from IInterpretedProjectionInput.
          /// 2. Calls executor.Init with the ExecutorState
          /// 3. Translates ExecutorState to IProjectionState
          /// </summary>
          /// <param name=\"input\"></param>
          /// <param name=\"configuration\"></param>
          /// <returns></returns>
          public IProjectionState<TFunction> InnerInitialize(IInterpretedProjectionInput<TFunction> input, IProjectionConfiguration configuration, out IReadOnlyList<double> calculationPeriodEndTimes)
          {
              int ___NumberOfProjs = input.EconomicScenarioTimes.Count;
              GenExecutor<TFunction,TOutput>.config = configuration;
              executorState.Global = new Global<TFunction>();
              executorState.Global.Input = new GlobalInput<TFunction>();
              //executorState.Global.Input.DiscountFactors = input.DiscountFactors;
              executorState.Global.Input.EconomicScenarioTimes = input.EconomicScenarioTimes.ToArray();
              //executorState.Global.Input.BiometricScenarioTimes = input.BiometricScenarioTimes.ToArray();
              //executorState.Global.Input.FreePolicyRules = input.CashFlowNameFreePolicyRule;
              var policyGroupColAccum = new Dictionary<string, Tuple<Group<TFunction>,List<OneStatePolicy<TFunction>>,List<ThreeStatePolicy<TFunction>>>>();
              var policyEquityColAccum = new Dictionary<string, Tuple<Equity<TFunction>,List<OneStatePolicy<TFunction>>,List<ThreeStatePolicy<TFunction>>>>();
          }

          public IProjectionState<TFunction> InnerUpdate(IProjectionState<TFunction> state, IProjectionReservesResult result)
          {
              var (_executor, _executorState) = (ValueTuple<GenExecutor<TFunction, TOutput>,ExecutorState<TFunction, TOutput>>) state.CustomStateValue;
              executor = _executor;
              executorState = _executorState;
              _executorState.Projection.PeriodStartTime = result.PeriodStartTime;
              _executorState.Projection.PeriodEndTime = result.PeriodEndTime;
              _executorState.Projection.PeriodLength = result.PeriodEndTime - result.PeriodStartTime;
              //_executorState.___projNumber = ___projNumber++;
              executor.Update(result, executorState);
              executor.AddOutput(___projNumber++);
              return state;
          }

          public TOutput Finalize(IProjectionState<TFunction> state)
          {
            var (_executor, _executorState) = (ValueTuple<GenExecutor<TFunction, TOutput>, ExecutorState<TFunction, TOutput>>)state.CustomStateValue;
            return _executor.Finalize(OutputFactory, _executorState);
          }

      }"