using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

namespace Gen
{
    public static class TypeNameExtensions
    {
        public static string GetFriendlyName(this Type type)
        {
            string friendlyName = type.Name;
            if (type.IsGenericType) {
                int iBacktick = friendlyName.IndexOf('`');
                if (iBacktick > 0) {
                    friendlyName = friendlyName.Remove(iBacktick);
                }
                friendlyName += "<";
                Type[] typeParameters = type.GetGenericArguments();
                for (int i = 0; i < typeParameters.Length; ++i) {
                    string typeParamName = GetFriendlyName(typeParameters[i]);
                    friendlyName += (i == 0 ? typeParamName : "," + typeParamName);
                }
                friendlyName += ">";
            }

            return friendlyName;
        }
    }

    /// <summary>
    /// This attribute defines the internal type names used in MAL.
    /// E.g. a MAL "Reserve" may modeled by a double in C#.
    /// In this case the property should have the customAttribute MalTypeName("Reserve")
    /// </summary>
    public class MalTypeName : System.Attribute
    {
        public string Name { get; set; }

        public MalTypeName(string name)
        {
            this.Name = name;
        }
    }

    /// <summary>
    /// Describes the type of conversion made to match the interface decleration
    /// </summary>
    public enum ConversionType
    {
      PostConversion = 0, // Postfix conversion
      Replace = 1, // Custom conversin replace everything
      Null = 2  // Return null instead
    }

    /// <summary>
    /// This attribute defines how a MAL-value should be converted to the MAL-interfaces.
    /// E.g. since dictionaries is not covarient then we need to convert:
    /// Dictionary<string, Policy> to Dictionary<string, MAL_Policy> when implementing our MAL-interfaces.
    /// </summary>
    public class MalConversionToMalI : System.Attribute
    {
      public string Conversion { get; set; }
      public ConversionType ConversionType { get; set; }

      public MalConversionToMalI(string conversion, ConversionType conversionType)
      {
        this.Conversion = conversion;
        this.ConversionType = conversionType;
      }
    }

    /// <summary>
    /// This attribute marks that the method or tag should not
    /// be considered during reflection
    /// </summary>
    public class MalReflectionIgnore : System.Attribute
    {
    }

    public interface MAL_Param<TFunction>
        where TFunction : IFunction
    {
    }

    public interface MAL_Input<TFunction>
        where TFunction : IFunction
    {
    }

    public interface MAL_Result<TFunction>
        where TFunction : IFunction
    {
    }

    public interface MAL_BaseEntity<TFunction>
        where TFunction : IFunction
    {
        MAL_Param<TFunction> MAL_Param { get; }
        string MAL_Name { get; set; }
    }

    public interface MAL_EquityResult<TFunction>
        where TFunction : IFunction
    {
        double MAL_PeriodBenefits { get; }
        double MAL_PeriodPremiums { get; }
        [MalTypeName("Reserve")]
        double MAL_AccumulatedReserveAtEnd { get; }
        double MAL_PeriodRealisedReturn { get; }
        double MAL_PeriodInvestmentFee { get; }
        bool MAL_IsCalculationEndTimeBeforePeriodEnd { get; }
    }

    public interface MAL_EquityInput<TFunction>
        where TFunction : IFunction
    {
        [MalTypeName("Reserve")]
        double MAL_Reserve { get; set; }
    }

    public interface MAL_Equity<TFunction> : MAL_BaseEntity<TFunction>
        where TFunction : IFunction
    {
        MAL_EquityResult<TFunction> MAL_Result { get; }
        MAL_EquityInput<TFunction> MAL_Input { get; }

        //[MalConversionToMalI(".Filter<MAL_Policy<TFunction>>()")]
        [MalConversionToMalI("", ConversionType.Null)]
        MAL_Policy<TFunction>[] MAL_Policies { get; }


        //[MalConversionToMalI(".Filter<MAL_Asset<TFunction>>()")]
        [MalConversionToMalI("", ConversionType.Null)]
        MAL_Asset<TFunction>[] MAL_Assets { get; }

        [MalTypeName("Reserve")]
        double MAL_Reserve { get; set; }
        double MAL_PremiumScale { get; set; }
        double MAL_BenefitScale { get; set; }
    }

    public interface MAL_GroupResult<TFunction>
        where TFunction : IFunction
    {
        [MalTypeName("Reserve")]
        double MAL_AccumulatedReserveAtEnd { get; set; }
        double MAL_PeriodRealisedReturn { get; set; }
        bool MAL_IsCalculationEndTimeBeforePeriodEnd { get; set; }
        double MAL_PeriodInvestmentFee { get; }
    }

    public interface MAL_ReserveGroupInput<TFunction>
        where TFunction : IFunction
    {
        [MalTypeName("Reserve")]
        double MAL_Reserve { get; set; }
    }

    public interface MAL_Group<TFunction> : MAL_BaseEntity<TFunction>
        where TFunction : IFunction
    {
        //[MalConversionToMalI(".Filter<MAL_Policy<TFunction>>()")]
        [MalConversionToMalI("", ConversionType.Null)]
        MAL_Policy<TFunction>[] MAL_Policies { get; }
        //[MalConversionToMalI(".Filter<MAL_Asset<TFunction>>()")]
        [MalConversionToMalI("", ConversionType.Null)]
        MAL_Asset<TFunction>[] MAL_Assets { get; }
        MAL_Input<TFunction> MAL_Input { get; }
        MAL_GroupResult<TFunction> MAL_Result { get; }
    }

    public interface MAL_Expenses<TFunction>
        where TFunction : IFunction
    {
        [MalTypeName("Option<Float>")]
        double? MAL_CashFlowExpense { get; set; }
        [MalTypeName("Option<Float>")]
        double? MAL_FreePolicyFee { get; set; }
        [MalTypeName("Option<Float>")]
        double? MAL_SurrenderFee { get; set; }
        [MalTypeName("Option<Float>")]
        double? MAL_PolicyFee { get; set; }
        [MalTypeName("Option<Float>")]
        double? MAL_ReserveExpense { get; set; }
    }



    public interface MAL_Expense<TFunction> : MAL_BaseEntity<TFunction>
        where TFunction : IFunction
    {
        [MalTypeName("Reserve")]
        double MAL_Reserve { get; set; }
        MAL_ReserveGroupInput<TFunction> MAL_Input { get; }
        MAL_Expenses<TFunction> MAL_TechnicalExpense { get; }
        MAL_Expenses<TFunction> MAL_ExpenseDividends { get; }

        [MalConversionToMalI(".ToDictionary(kvp =>  kvp.Key, kvp => (MAL_Expenses<TFunction>)kvp.Value)", ConversionType.PostConversion)]
        IDictionary<LumpedState, MAL_Expenses<TFunction>> MAL_LumpedExpenseDividends { get; }
    }

    public interface MAL_Interest<TFunction> : MAL_BaseEntity<TFunction>
        where TFunction : IFunction
    {
        [MalTypeName("Reserve")]
        double MAL_Reserve { get; set; }
        MAL_ReserveGroupInput<TFunction> MAL_Input { get; }
        double? MAL_SurrenderCharge { get; set; }
        double MAL_DepositRate { get; set; }
    }

    public interface MAL_MarketRateInterest<TFunction>
        where TFunction : IFunction
    {

    }

    public interface MAL_Risk<TFunction> : MAL_BaseEntity<TFunction>
        where TFunction : IFunction
    {
        [MalTypeName("Reserve")]
        double MAL_Reserve { get; set; }
        MAL_ReserveGroupInput<TFunction> MAL_Input { get; }
        double MAL_RiskDividend { get; set; }
        [MalConversionToMalI(".ToDictionary(kvp =>  kvp.Key, kvp => kvp.Value)", ConversionType.PostConversion)]
        IDictionary<LumpedState, double> MAL_LumpedRiskDividend { get; }
    }

    public interface MAL_PolicyInput<TFunction>
        where TFunction : IFunction
    {
        double MAL_CalculationEndTime { get; set; }
    }

    public interface MAL_PolicyResult<TFunction>
        where TFunction : IFunction
    {
      bool MAL_IsCalculationEndTimeBeforePeriodEnd { get; set; }
    }

    public interface MAL_OneStateResult<TFunction>
        where TFunction : IFunction
    {
        [MalTypeName("Reserve")]
        double MAL_AccumulatedReserveAtEnd { get; set; }
        MAL_Expenses<TFunction> MAL_PeriodTechnicalExpenses { get; }
        IReadOnlyDictionary<string, double> MAL_PeriodRiskContributionPerGroup { get; }
        double MAL_PeriodRealisedReturn { get; set; }
        double MAL_PeriodInvestmentFee { get; set; }
        double MAL_PeriodActualExpense { get; set; }
    }

    /// <summary>
    /// Corresponds to IPolicyStateParameters.
    /// StateModel is determined through inheritance to either OneStatePolicy or ThreeStatePolicy.
    /// </summary>
    /// <typeparam name="TFunction"></typeparam>
    public interface MAL_Policy<TFunction> : MAL_BaseEntity<TFunction>
        where TFunction : IFunction
    {
        // ´IPolicyStateParameters.EquityId´
        [MalTypeName("Option<Equity>")]
        MAL_Equity<TFunction> MAL_Equity { get; }

        //[MalConversionToMalI(".Filter<MAL_Group<TFunction>>()")]
        [MalConversionToMalI("", ConversionType.Null)]
        // ´IPolicyStateParameters.GroupCollection´
        MAL_Group<TFunction>[] MAL_Groups { get; }

        // Fine grained group division
        [MalTypeName("Option<Expense>")]
        MAL_Expense<TFunction> MAL_ExpenseGroup { get; }
        MAL_Group<TFunction> MAL_InterestGroup { get; }
        //[MalConversionToMalI(".Filter<MAL_Risk<TFunction>>()")]
        [MalConversionToMalI("", ConversionType.Null)]
        MAL_Risk<TFunction>[] MAL_RiskGroups { get; }

        // ´IPolicyStateParameters.CashFlows´
        //[MalConversionToMalI(".Filter<MAL_CashFlow<TFunction>>()")]
        //TypeSpan<MAL_CashFlow<TFunction>> MAL_CashFlows { get; }

        // ´IPolicyStateParameters.CalculationEndTime´
        double MAL_CalculationEndTime { get; set; }

        MAL_PolicyInput<TFunction> MAL_Input { get; }
        MAL_PolicyResult<TFunction> MAL_Result { get; }
    }

    public interface MAL_OneStatePolicy<TFunction> : MAL_Policy<TFunction>
        where TFunction : IFunction
    {
        // ´IPolicyStateParameters.Reserve´
        [MalTypeName("Reserve")]
        double MAL_Reserve { get; set; }

        // ´IPolicyStateParameters.CashFlows´
        //[MalConversionToMalI(".Filter<MAL_CashFlow<TFunction>>()")]
        [MalConversionToMalI(@"{
                              var ret = new MAL_CashFlow<TFunction>[CashFlows.Count()];
                              for (int i = 0; i < CashFlows.Count(); i++)
                              {
                                ret[i] = CashFlows[i];
                              }
                              return ret;
                            }
                            ", ConversionType.Replace)]
        MAL_CashFlow<TFunction>[] MAL_CashFlows { get; }
    }

    public interface MAL_ThreeStatePolicy<TFunction> : MAL_Policy<TFunction>
        where TFunction : IFunction
    {
        [MalConversionToMalI(".ToDictionary(kvp =>  kvp.Key, kvp => (MAL_ThreeStateState<TFunction>)kvp.Value)", ConversionType.PostConversion)]
        IReadOnlyDictionary<LumpedState, MAL_ThreeStateState<TFunction>> MAL_LumpedStates { get; }
        double MAL_Size { get; set; }
        TFunction MAL_FreePolicyFraction { get; set; }

	    //MAL_ThreeStateResult<TFunction> MAL_Result { get; } // Todo move result and input from policy to 1 and 3 state
    }

    public interface MAL_ThreeStateState<TFunction>
        where TFunction : IFunction
    {
        [MalTypeName("Reserve")]
        double MAL_Reserve { get; set; }
        [MalConversionToMalI(@"{
                              var ret = new MAL_CashFlow<TFunction>[CashFlows.Count()];
                              for (int i = 0; i < CashFlows.Count(); i++)
                              {
                                ret[i] = CashFlows[i];
                              }
                              return ret;
                            }
                            ", ConversionType.Replace)]
        MAL_CashFlow<TFunction>[] MAL_CashFlows { get; }
        [MalConversionToMalI(".ToDictionary(kvp => kvp.Key, kvp => kvp.Value)", ConversionType.PostConversion)]
        IReadOnlyDictionary<LumpedStateWithSurrender, TFunction> MAL_DestinationLumpedStateIntensity { get; }
        [MalConversionToMalI(".ToDictionary(kvp => kvp.Key, kvp => kvp.Value)", ConversionType.PostConversion)]
        IReadOnlyDictionary<Tuple<State, State>, TFunction> MAL_IntensitiesBiometric { get; }
    }

    public interface MAL_ThreeStateResult<TFunction> where TFunction : IFunction
    {
        IDictionary<LumpedState, MAL_LumpedStateResult<TFunction>> LumpedStateResult { get; }
    }
    public interface MAL_LumpedStateResult<TFunction> where TFunction : IFunction
    {
      double MAL_SurrenderCashFlow { get; set; }
      [MalTypeName("Reserve")]
      double AccumulatedReserveAtEnd { get; set; }
      double BenefitsLessPremiumsCashFlow { get; set; }
      double PeriodRealisedReturn { get; set; }
      IDictionary<string, double> PeriodRiskContributionPerGroup { get; }
      MAL_Expenses<TFunction> PeriodTechnicalExpenses { get; }
      double Probability { get; set; }
      IDictionary<string, double> ScalingFactors { get; }
      IDictionary<State, double> StateProbabilities { get; }
      double PeriodInvestmentFee { get; set; }
      double MAL_PeriodActualExpense { get; set; }
    }

    public interface MAL_CashFlow<TFunction> : MAL_BaseEntity<TFunction>
        where TFunction : IFunction
    {
        MAL_Policy<TFunction> MAL_Policy { get; }
        double MAL_ScalingFactor { get; set; }
        MAL_CashFlowInput<TFunction> MAL_Input { get; }
  }

    public interface MAL_CashFlowInput<TFunction>
        where TFunction : IFunction
    {
        // Todo should contain transfers
    }

    public interface MAL_ActualExpense<TFunction>
        where TFunction : IFunction
    {
        MAL_Policy<TFunction> MAL_Policy { get; }
        double MAL_ScalingFactor { get; set; }
        MAL_CashFlowInput<TFunction> MAL_Input { get; }
    }

    public interface MAL_WithExpenses<TFunction>
        where TFunction : IFunction
    {
        MAL_Policy<TFunction> MAL_Policy { get; }
        double MAL_ScalingFactor { get; set; }
        MAL_CashFlowInput<TFunction> MAL_Input { get; }
        IReadOnlyDictionary<string, ICashFlow> MAL_RiskContributions { get; }
    }
    public interface MAL_WithoutExpenses<TFunction>
        where TFunction : IFunction
    {
        MAL_Policy<TFunction> MAL_Policy { get; }
        double MAL_ScalingFactor { get; set; }
        MAL_CashFlowInput<TFunction> MAL_Input { get; }
        IReadOnlyDictionary<string, ICashFlow> MAL_RiskContributions { get; }
        // Todo should contain RiskContributions
    }
    public interface MAL_PolicyPaidExpense<TFunction>
        where TFunction : IFunction
    {
        MAL_Policy<TFunction> MAL_Policy { get; }
        double MAL_ScalingFactor { get; set; }
        MAL_CashFlowInput<TFunction> MAL_Input { get; }
    }

    public interface MAL_Transfers<TFunction>
        where TFunction : IFunction
    {
        double[] MAL_Values { get; }
        double[] MAL_TimePoints { get; }
    }

    public interface MAL_Asset<TFunction> : MAL_BaseEntity<TFunction>
        where TFunction : IFunction
    {
        MAL_AssetInput<TFunction> MAL_Input { get; }
        double MAL_Weight { get; set; }
        double MAL_InvestmentFee { get; set; }
    }

    public interface MAL_AssetInput<TFunction>
        where TFunction : IFunction
    {
        double MAL_Weight { get; set; }
        double MAL_InvestmentFee { get; set; }
    }

    public interface MAL_Global<TFunction> : MAL_BaseEntity<TFunction>
        where TFunction : IFunction
    {
        double[] MAL_BiometricScenarioTimes { get; }
        double[] MAL_ProjectionTimes { get; }
        Func<double, double> MAL_RealisedDiscountFactor { get; }
        Tuple<double, double>[] MAL_RealisedDiscountFactorContinuousPayments { get; set; }
        Tuple<double, double>[] MAL_RealisedDiscountFactorContinuousPaymentsInclPolicyEndTimes { get; set; }
        double MAL_CalculationTime { get; }
        bool MAL_TimeZeroOutputIncluded { get; }
        MAL_GlobalInput<TFunction> MAL_Input { get; }
        [MalConversionToMalI(".ToDictionary(kvp => (MAL_Policy<TFunction>)kvp.Key, kvp => kvp.Value)", ConversionType.PostConversion)]
        IReadOnlyDictionary<MAL_Policy<TFunction>, String[]> MAL_BenefitsReceivingCashFlows { get; }

        [MalConversionToMalI(".ToDictionary(kvp => (MAL_Policy<TFunction>)kvp.Key, kvp => kvp.Value)", ConversionType.PostConversion)]
        IReadOnlyDictionary<MAL_Policy<TFunction>, String[]> MAL_PremiumPayingCashFlows { get;  }
    }

    public interface MAL_GlobalInput<TFunction>
        where TFunction : IFunction
    {
        double[] MAL_BiometricScenarioTimes { get; }
        double[] MAL_EconomicScenarioTimes { get; }
        Func<double, Func<double, double>> MAL_BiometricScenario { get; }
        //IDictionary<string, MAL_InterpretedDiscountFactors<TFunction>> MAL_DiscountFactors { get; }
    }

    public interface MAL_InterpretedDiscountFactors<TFunction>
        where TFunction : IFunction
    {
        Func<double, Func<double, double>> MAL_DicountFactors { get; }
        Func<double, Func<double, double>> MAL_RealisedDiscountFactors { get; }
    }


    public interface MAL_Projection<TFunction>
        where TFunction : IFunction
    {
        double MAL_PeriodStartTime { get; }
        double MAL_PeriodEndTime { get; }
        double MAL_PeriodLength { get; }
    }

    public static class GenInterfaces
    {
        public static IEnumerable<PropertyInfo> GetPublicProperties(this Type type)
        {
            if (!type.IsInterface)
                return type.GetProperties();

            return (new Type[] { type })
                   .Concat(type.GetInterfaces())
                   .SelectMany(i => i.GetProperties());
        }
    }
}
