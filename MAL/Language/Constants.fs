namespace itu.dk.MAL

module Constants =

  open AST

  let tag_baseEntity   = "BaseEntity"

  let tag_asset        = "Asset"
  let tag_assetInput   = "AssetInput"
  let tag_projection   = "Projection"
  let tag_param        = "Param"

  let tag_input        = "Input"
  let tag_global       = "Global"
  let tag_globalInput  = "GlobalInput"
  let tag_globalParam  = "GlobalParam"

  let tag_equity        = "Equity"
  let tag_equityInput   = "EquityInput"
  let tag_equityResult  = "EquityResult"

  let tag_group       = "Group"
  let tag_groupResult = "GroupResult"
  let tag_groupParam  = "GroupParam"
  let tag_groupInput  = "GroupInput"

  let tag_interest        = "Interest"
  let tag_interestParam   = "InterestParam"
  let tag_risk            = "Risk"
  let tag_riskParam       = "RiskParam"

  let tag_expense         = "Expense"
  let tag_expenseParam    = "ExpenseParam"
  let tag_expenses        = "Expenses"
  let tag_expensesParam   = "ExpensesParam"

  let tag_transfers = "Transfers"

  let tag_marketRateInterest      = "MarketRateInterest"
  let tag_marketRateInterestParam = "MarketRateInterestParam"

  let tag_reserveGroup      = "ReserveGroup"
  let tag_reserveGroupInput = "ReserveGroupInput"
  let tag_reserveGroupParam = "ReserveGroupParam"

  let tag_policy        = "Policy"
  let tag_oneStatePolicy    = "OneStatePolicy"
  let tag_threeStatePolicy  = "ThreeStatePolicy"
  let tag_threeStateState  = "ThreeStateState"
  let tag_policyResult  = "PolicyResult"
  let tag_oneStateResult  = "OneStateResult"
  let tag_policyInput  = "PolicyInput"
  let tag_policyParam   = "PolicyParam"

  let tag_cashFlowInput           = "CashFlowInput"
  let tag_cashFlow                = "CashFlow"
  let tag_cashFlowParam           = "CashFlowParam"
  let tag_actualExpense           = "ActualExpense"
  let tag_actualExpenseParam      = "ActualExpenseParam"
  let tag_withExpenses            = "WithExpenses"
  let tag_withExpensesParam       = "WithExpensesParam"
  let tag_withoutExpenses         = "WithoutExpenses"
  let tag_withoutExpensesParam    = "WithoutExpensesParam"
  let tag_policyPaidExpense      = "PolicyPaidExpense"
  let tag_policyPaidExpensesParam = "PolicyPaidExpenseParam"

  let rel_groupToPolicy = []
  let rel_policyToGroup = [("Interest", ToMany);("Risk", ToMany);("Expense", ToMany)]

  let var_input = "Input"
  let var_global = "Global"
  let var_param = "Param"
  let var_result = "Result"
  let var_projection = "Projection"
  let var_id = "Id"

  let var_periodStartTime = "PeriodStartTime"
  let var_periodEndTime = "PeriodEndTime"
  let var_periodLength = "PeriodLength"

  let var_assets = "Assets"
  let var_weight = "Weight"
  let var_economicScenario = "EconomicScenario"

  let var_biometricScenarioTimes = "BiometricScenarioTimes"
  let var_economicScenarioTimes = "EconomicScenarioTimes"
  let var_projectionTimes = "ProjectionTimes"
  let var_discountFactors = "DiscountFactors"
  let var_realisedDiscountFactors = "RealisedDiscountFactors"
  let var_outputDiscountFactor = "OutputDiscountFactor"

  let var_reserve = "Reserve"

  let var_equities                    = "Equities"
  let var_equity                      = "Equity"
  let var_premiumScale                = "PremiumScale"
  let var_benefitScale                = "BenefitScale"
  let var_periodBenefits        = "PeriodBenefits"
  let var_periodPremiums = "PeriodPremiums"
  let var_accumulatedReserveAtEnd = "AccumulatedReserveAtEnd"
  let var_isCalculationEndTimeBeforePeriodEnd = "IsCalculationEndTimeBeforePeriodEnd"
  let var_periodActualExpense = "PeriodActualExpense"
  let var_periodRealisedReturn = "PeriodRealisedReturn"

  let var_premiumPayingCashFlowNames = "PremiumPayingCashFlowNames"
  let var_benefitReceivingCashFlowNames = "BenefitReceivingCashFlowNames"

  let var_groups = "Groups"
  let var_depositRate = "DepositRate"
  let var_surrenderCharge = "SurrenderCharge"
  let var_reimbursement = "Reimbursement"
  let var_K = "K"
  let var_KExpense = "KExpense"
  let var_KRiskDividend = "KRiskDividend"
  let var_K2 = "K2"
  let var_K2Expense = "K2Expense"
  let var_K2Risk = "K2Risk"
  let var_riskMarginProxies = "RiskMarginProxies"
  let var_presentValueFutureProfitsProxies = "PresentValueFutureProfitsProxies"
  let var_technicalExpense = "TechnicalExpense"
  let var_expenseDividend = "ExpenseDividend"

  // Expenses
  let var_cashFlowExpense = "CashFlowExpense"
  let var_reserveExpense = "ReserveExpense"
  let var_policyFee = "PolicyFee"
  let var_surrenderFee = "SurrenderFee"
  let var_freePolicyFeeFee = "FreePolicyFee"

  let var_riskDividend = "RiskDividend"
  let var_riskWeight = "RiskWeight"

  let var_policies = "Policies"
  let var_policy = "Policy"
  let var_bonusAccount = "BonusAccount"
  let var_calculationEndTime = "CalculationEndTime"
  let var_periodCashFlow = "PeriodCashFlow"
  let var_periodTechnicalExpenses = "PeriodTechnicalExpenses"
  let var_periodRiskContributionPerGroup = "PeriodRiskContributionPerGroup"
  let var_riskContributions = "RiskContributions"
  let var_periodCashflowExpenseTechnicalExpensePayments = "PeriodCashflowExpenseTechnicalExpensePayments"
  let var_periodReserveExpenseTechnicalExpensePayments = "PeriodReserveExpenseTechnicalExpensePayments"

  let var_cashFlows = "CashFlows"
  let var_isScaled = "IsScaled"
  let var_technicalInterestRate = "TechnicalInterestRate"
  let var_distributedReserve = "DistributedReserve"
  let var_biometricScenario = "BiometricScenario"
  let var_interestRate = "InterestRate"
  let var_scalingFactor = "ScalingFactor"
  let var_contributionFunctions = "RiskContributions"
  let var_transfers = "Transfers"
  let var_name = "Name"
  let var_times = "Times"
  let var_amounts = "Amounts"




  // Constants used by the compiler
  // These interfaces are generated without a <TFunction> postfix
  let nonGenericNames =
                     [ "ICashFlow"
                     ; "ICashFlowParameters"
                     ; "IReserveSequence"
                     ; "ITimeSeries"
                     ; "IInterpretedDiscountFactors"
                     ; "IDiscountFunctionCalculator"
                     ; "IDiscountFunction"
                     ; "ILumpedStateParameters"
                     ; "ILumpedStatePeriodResult"]

  // List of types used internally in MAL.
  // E.g., when the user writes Transfers we internally in C# generate an "ICashFlow"
  let typeNameConversion =
      [ "Transfers","ICashFlow"
      ; "ReserveSequence", "IReserveSequence"
      ; "TimeSeries", "ITimeSeries"
      ; "InterpretedDiscountFactors", "IInterpretedDiscountFactors"
      ; "DiscountFunctionCalculator", "IDiscountFunctionCalculator"
      ; "DiscountFunction", "IDiscountFunction"
      ; "LumpedStateParameters", "ILumpedStateParameters"
      ; "LumpedStatePeriodResult", "ILumpedStatePeriodResult"
      ]

  let typeNameConversionMALtoEdlund =
    Map.ofList typeNameConversion

  let typeNameConversionEdlundtoMAL =
    Map.ofList <| List.map (fun (a,b) -> b,a) typeNameConversion

