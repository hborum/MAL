module TestDataDef

let dataDefinition ="

data BaseEntity
	Input  : Input
	Result : Result
	Param  : Param
	Name   : String
end

data Input
end

data Result
end

data Param
end

data AssetInput extends Input
	Weight  : Float
end

data Asset extends BaseEntity
	Weight  : Float
	Input   : AssetInput
  InvestmentFee : Float
end

data Global extends BaseEntity
	OutputDiscountFactor         : Func<Float, Float>
	ProjectionTimes              : List<Float>
	BiometricScenarioTimes       : List<Float>
	Input 				         : GlobalInput
	Param                        : GlobalParam

	timeSinceObservationDate     : Float

	BaseIntensities_male             : BaseIntensities
	BaseIntensities_female           : BaseIntensities

	FreePolicyRules              : Map<String, FreePolicyRule>

	CapitalTransferEquity 		 : Equity
	InterestPolicies     		 : List<Policy>

	BalanceDiscountFunctions 	 		 : Func<Float, Func<Float, Float>>
	PreviousMarketInterestRateBalance 	 : MarketInterestRateBalance
	MarketInterestRateBalance 	 		 : MarketInterestRateBalance
	FinancialReport              		 : FinancialReport

	WithProfitGroups         : List<ReserveGroup>
	MarketRateInterestGroups : List<Group>

	PalTaxRate               : Float

	ActualExpensesReserve        		 : Float, Output as PresentValueEnd
	BaseCapitalPALPaymentReserve 		 : Float, Output as PresentValueEnd
	ActualExpenses				         : Float, Output as CashFlow
	BaseCapital						     : Float, Output as CashFlow
	BaseCapitalInvestmentReturnTaxAsset  : Float, Output as CashFlow
	BaseCapitalPALPayment                : Float, Output as CashFlow

  maybe : Option<Float>
  TestReserve : Float
end

data MarketInterestRateBalance
	RiskMargin               : Float
	BonusPotential           : Float
	LifeInsuranceLiabilities : Float
	ProfitMargin             : Float
	InsuranceLiabilities     : Float
end

data BaseIntensities
	Level             : List<Float>
	Trend             : List<Float>

	Exp_Model_beta1       : Float
	Exp_Model_beta2       : Float
	Exp_Model_beta3       : Float

	Gompertz_Makeham_A    : Float
	Gompertz_Makeham_B    : Float
	Gompertz_Makeham_C    : Float

	Gompertz_Makeham_scaling_Disability    : Float

	Exp_value_Reactivation  : Float
	Exp_value_FreePolicy    : Float
	Exp_value_Surrender     : Float
end

data GlobalParam extends Param
	DiscountFactorName : String

	// Benchmark_times        : List<Float>
	// Level_male             : List<Float>
	// Level_female           : List<Float>
	// Trend_male             : List<Float>
	// Trend_female           : List<Float>

	// Observation_date           : Float
	// Exp_Model_beta1_male       : Float
	// Exp_Model_beta1_female     : Float
	// Exp_Model_beta2_male       : Float
	// Exp_Model_beta2_female     : Float
	// Exp_Model_beta3_male       : Float
	// Exp_Model_beta3_female     : Float

	// Gompertz_Makeham_A_male    : Float
	// Gompertz_Makeham_A_female  : Float
	// Gompertz_Makeham_B_male    : Float
	// Gompertz_Makeham_B_female  : Float
	// Gompertz_Makeham_C_male    : Float
	// Gompertz_Makeham_C_female  : Float

	// Gompertz_Makeham_scaling_Disability_male    : Float
	// Gompertz_Makeham_scaling_Disability_female  : Float

	// Exp_value_Reactivation_male   : Float
	// Exp_value_Reactivation_female : Float
	// Exp_value_FreePolicy_male     : Float
	// Exp_value_FreePolicy_female   : Float
	// Exp_value_Surrender_male      : Float
	// Exp_value_Surrender_female    : Float

	// RiskMarginProxies                 : Float
	// PresentValueFutureProfitsProxies  : Float
	// BaseFeeParameter                  : Float
	// FeeThreshold                      : Float
	// LowFeeParameter                   : Float

	// MarketInterestRateRiskMargin                 : Float
	// MarketInterestRateBonusPotential             : Float
	// MarketInterestRateLifeInsuranceLiabilities   : Float
	// MarketInterestRateProfitMargin               : Float
	// MarketInterestRateInsuranceLiabilities       : Float

	// InvestmentReturnTaxRate : Float
	// KRiskDividend           : Float
	// KExpense                : Float
end

data GlobalInput extends Input
	BiometricScenarioTimes     : List<Float>
	EconomicScenarioTimes      : List<Float>
	BiometricScenario          : Func<Float, Func<Float, Float>>
	DiscountFactors            : Map<String, InterpretedDiscountFactors>
	CalculationTime            : Float
	FreePolicyRules            : Map<String, FreePolicyRule>
end

data InterpretedDiscountFactors
	DiscountFactors         : Func<Float, Func<Float, Float>>
	RealisedDiscountFactors : Func<Float, Func<Float, Float>>
end

data EquityParam extends Param
  //  PAL_asset : Float
  //  ScalingFactorPremiumsToEquity : Float
  //  ScalingFactorBenefitsFromEquity : Float
end

data Equity extends BaseEntity
	Param                         : EquityParam
	Input 						  : EquityInput
	Result 						  : EquityResult
	Assets                        : List<Asset>
	Policies                      : List<Policy>
	Reserve                       : Reserve
	PremiumScale                  : Float
	BenefitScale                  : Float
	PAL                    		  : PALTax
end

data EquityInput extends Input
	Reserve : Reserve
end

data EquityResult extends Result
	PeriodPremiums : Float
	PeriodBenefits : Float
	IsCalculationEndTimeBeforePeriodEnd : Bool
	PeriodActualExpense                 : Float
	PeriodRealisedReturn                : Float
	AccumulatedReserveAtEnd             : Reserve
  PeriodInvestmentFee                 : Float
end

data Transfers
	Values      : List<Float>
	TimePoints  : List<Float>
end

data CashFlow  extends BaseEntity
	Input               : CashFlowInput
	Param               : CashFlowParam
	Policy              : Policy
	ScalingFactor       : Float
	IsScaled            : Bool
end

data CashFlowInput extends Input
	Transfers : Transfers
end

data CashFlowParam extends Param
	// Cash_flow_equity_type : String
end

data ActualExpense extends CashFlow

end

data WeParam extends CashFlowParam
	// Technical_interest_rate 	: Float
	// Technical_reserve        	: Float
	// IsScaled                  : Bool
end

data WithExpenses extends CashFlow
	TechnicalInterestRate 	: Float
	RiskContributions   	: Map<String, Transfers>
	Param               	: WeParam
	TechnicalReserveSequence : IFunction
	TechnicalReserve         : Float
end

data WithoutExpenses extends CashFlow
	TechnicalInterestRate 	: Float
	RiskContributions   	: Map<String, Transfers>
	IsScaled            	: Bool
	Param               	: WeParam
	TechnicalReserveSequence : IFunction
	TechnicalReserve         : Float
end

data PolicyPaidExpense extends CashFlow
end

data PolicyParam extends Param
	// PAL_asset : Float
end

// Expenses of Policy is found in the Expense group
data Policy extends BaseEntity
	Result : PolicyResult
	Input  : PolicyInput
	Reserve : Reserve
	Param   : PolicyParam
	CalculationEndTime : Float
	CashFlows :  List<CashFlow>

	PremiumPayingCashFlows : List<String>
	BenefitReceivingCashFlows : List<String>

	Groups       : List<Group>
	RiskGroups   : List<Risk>
	InterestGroup : Group
	ExpenseGroup : Option<Expense>

	Equity : Option<Equity>

	PolicyReserve          : Float, Output as CashFlow
	ScaledCFTechReserve    : Float, Output as CashFlow
	CFScalingFactor_Scaled : Float, Output as CashFlow
	BenefitsLessPremiumsReserves   : Float, Output as PresentValueMid
	BenefitsLessPremiums   		   : Float, Output as CashFlow

	PAL                    : PALTax

	TechnicalReservePerCashFlow : Map<String, Float>

	// These three are only used by market rate interest Policies
	ShareHolderFee : Float
	TotalReserve   : Float
	RiskMargin     : Float
end

data PALTax
	InvestmentReturnTaxAsset : Float
	InvestmentReturnTaxPaymentForPeriod : Float
end

data OneStateParam extends PolicyParam
	// Policy_Reserve : Float
	// Bonus_account : Float
end

data OneStatePolicy extends Policy
	Param : OneStateParam
end

data ThreeStateParam extends PolicyParam
	// These should all be options
	// Age : Float
	// Gender : String
	// Policyholder_options_expiry_age : Float
	// Policy_Reserve_FreePolicy : Reserve
	// Policy_Reserve_Biometric : Reserve
	// Size : Float
end

data ThreeStateInput extends PolicyInput
	LumpedStateParameters  : Map<LumpedState, LumpedStateParameters>
end

data LumpedStateParameters
	InitialProbability : Float
	InitialStateProbabilities : Map<State, Float>
end

data ThreeStateResult extends PolicyResult
	LumpedStateResult  : Map<LumpedState, LumpedStateResult>
end

data LumpedStateResult
	AccumulatedReserveAtEnd           : Reserve
	BenefitsLessPremiumsCashFlow      : Float
	SurrenderCashFlow                 : Float
	PeriodRealisedReturn              : Float
	PeriodRiskContributionPerGroup    : Map<String, Float>
	PeriodTechnicalExpenses           : Expenses
	ScalingFactors                    : Map<String, Float>
	Probability                       : Float
	StateProbabilities                : Map<State, Float>
end

data ThreeStatePolicy extends Policy
	Param : ThreeStateParam
	Input : ThreeStateInput
	Result : ThreeStateResult
	Size : Float
	FreePolicyFraction : IFunction

	ExpiryTime : Float
	LumpedStates : Map<LumpedState, ThreeStateState>
end

data ThreeStateState
	State   : LumpedState
	Reserve : Reserve
	CashFlows : List<CashFlow>
	DestinationLumpedStateIntensity : Map<LumpedStateWithSurrender, IFunction>
	IntensitiesBiometric : Map<Pair<State, State>, IFunction>

	AverageScalingFactor : Float
	WeightedTechinalReserve 		 : Float

	Probability  : Float
end

data PolicyResult extends Result
	IsCalculationEndTimeBeforePeriodEnd : Bool
	PeriodRiskContributionPerGroup : Map<String, Float>
	PeriodTechnicalExpenses  : Expenses
	PeriodCashFlow : Float
	AccumulatedReserveAtEnd : Reserve
	RealisedReturn : Float
  PeriodActualExpense : Float
  PeriodInvestmentFee : Float
end

data Expenses
	CashFlowExpense : Option<Float>
	FreePolicyFee   : Option<Float>
	SurrenderFee    : Option<Float>
	PolicyFee       : Option<Float>
	ReserveExpense  : Option<Float>
end

data PolicyInput extends Input
	CalculationEndTime : Float
end

data Group extends BaseEntity
	Result   : GroupResult
	Assets   : List<Asset>
	Policies : List<Policy>
end

data ReserveParam extends Param
  // PAL_asset 				 : Float
  // RiskMargin               : Float
	// IndividualBonusPotential : Float
	// CollectiveBonusPotential : Float
	// LifeInsuranceLiabilities : Float
	// GuaranteedBenefit        : Float
	// ProfitMargin             : Float
	// InsuranceLiabilities     : Float
	// Reimbursement            : Float
end

data ReserveGroup extends Group
	Param               : ReserveParam
	Reserve             : Reserve
	PAL                 : PALTax
	ReserveInclIRTA     : Float
	PreviousBalance     : SingleBalance
	Balance             : SingleBalance

	TransferToCapitalBase         	: Float, Output as CashFlow
	TransferToCapitalBaseReserves 	: Float, Output as PresentValueEnd
	TransferFromCapitalBase         : Float, Output as CashFlow
	TransferFromCapitalBaseReserves : Float, Output as PresentValueEnd
	PALPayment        				: Float, Output as CashFlow
	PALPaymentReserve 				: Float, Output as PresentValueEnd

	InvestmentReturnTaxAsset        : Float, Output as CashFlow
	GroupReserve					: Float, Output as CashFlow
end

data SingleBalance
	RiskMargin : Float
	IndividualBonusPotential  : Float
	CollectiveBonusPotential  : Float
	LifeInsuranceLiabilities  : Float
	BaseCapitalFee  : Float
	BaseCapitalInjectionForPeriod  : Float
	GuaranteedBenefit  : Float
	ProfitMargin  : Float
	InsuranceLiabilities  : Float
	Reimbursement  : Float
end

data FinancialReport
	PremiumsLessBenefits 				: Float
	RealisedReturn 						: Float
	ActualExpenses 						: Float
	ChangeInLifeInsuranceLiabilities 	: Float
	ChangeInProfitMargin 				: Float
	AnnualResult                        : Float
end

data InterestParam extends ReserveParam
	// K 				  : Float
	// K2 				  : Float
	// Surrender_charge  : Float
	// RiskMarginProxies : Float
	// PresentValueFutureProfitsProxies : Float
end

data Interest extends ReserveGroup
	Input           			: ReserveGroupInput
	Param           			: InterestParam
	SurrenderCharge 			: Float, Output as CashFlow
	DepositRate     			: Float, Output as CashFlow
	ScalingFactor   			: Float
	SumOfPolicyReserves         : Float

	GY								: Float, Output as CashFlow
	RiskMargin						: Float, Output as CashFlow
	ProfitMargin					: Float, Output as CashFlow
	IndividualBonusPotential		: Float, Output as CashFlow
	CollectiveBonusPotential		: Float, Output as CashFlow
	Reimbursement					: Float, Output as CashFlow
end

data RiskParam extends ReserveParam
	// RiskContributionWeights : Float
	// K2Risk                  : Float
end

data Risk extends ReserveGroup
	Input 			       : ReserveGroupInput
	RiskDividend           : Float, Output as CashFlow
	LumpedRiskDividend     : Map<LumpedState, Float>
	RiskContributionWeight : Float
	Param                  : RiskParam
end

data ExpenseParam extends ReserveParam
	// TechnicalReserveExpense  : Float
	// TechnicalCashFlowExpense : Float
	// K2Expense				 : Float
	// Technical_reserve_expense : Float
	// Technical_cash_flow_expense : Float
	// Technical_free_policy_fee_expense : Float
	// Technical_surrender_fee_expense : Float
end

data Expense extends ReserveGroup
	Input                 : ReserveGroupInput
	ExpenseDividends      : Expenses
	LumpedExpenseDividends : Map<LumpedState, Expenses>

	TechnicalExpense      : Expenses
	Param                 : ExpenseParam

	ExpenseDividend       : Float, Output as CashFlow
end

data ReserveGroupInput extends Input
	Reserve 			: Reserve
end

data MarketRateInterest extends Group
end

data GroupResult extends Result
	IsCalculationEndTimeBeforePeriodEnd : Bool
	PeriodActualExpense                 : Float
	PeriodRealisedReturn                : Float
	AccumulatedReserveAtEnd             : Reserve
  PeriodInvestmentFee                 : Float
end

data Projection
	PeriodStartTime  : Float
	PeriodEndTime 	 : Float
	PeriodLength 	 : Float
end
"

//RiskContributions missing
let dataDefinitionMissing1 =
  "
 data BaseEntity
 	Input  : Input
 	Result : Result
 	Param  : Param
 	Name   : String
 end

 data Input
 end

 data Result
 end

 data Param
 end

 data AssetInput extends Input
 	Weight  : Float
 end

 data Asset extends BaseEntity
 	Weight  : Float
 	Input   : AssetInput
  InvestmentFee : Float
 end

 data Global extends BaseEntity
 	OutputDiscountFactor         : Func<Float, Float>
 	ProjectionTimes              : List<Float>
 	BiometricScenarioTimes       : List<Float>
 	Input 				         : GlobalInput
 	Param                        : GlobalParam

 	timeSinceObservationDate     : Float

 	BaseIntensities_male             : BaseIntensities
 	BaseIntensities_female           : BaseIntensities

 	FreePolicyRules              : Map<String, FreePolicyRule>

 	CapitalTransferEquity 		 : Equity
 	InterestPolicies     		 : List<Policy>

 	BalanceDiscountFunctions 	 		 : Func<Float, Func<Float, Float>>
 	PreviousMarketInterestRateBalance 	 : MarketInterestRateBalance
 	MarketInterestRateBalance 	 		 : MarketInterestRateBalance
 	FinancialReport              		 : FinancialReport

 	WithProfitGroups         : List<ReserveGroup>
 	MarketRateInterestGroups : List<Group>

 	PalTaxRate               : Float

 	ActualExpensesReserve        		 : Float, Output as PresentValueEnd
 	BaseCapitalPALPaymentReserve 		 : Float, Output as PresentValueEnd
 	ActualExpenses				         : Float, Output as CashFlow
 	BaseCapital						     : Float, Output as CashFlow
 	BaseCapitalInvestmentReturnTaxAsset  : Float, Output as CashFlow
 	BaseCapitalPALPayment                : Float, Output as CashFlow
 end

 data MarketInterestRateBalance
 	RiskMargin               : Float
 	BonusPotential           : Float
 	LifeInsuranceLiabilities : Float
 	ProfitMargin             : Float
 	InsuranceLiabilities     : Float
 end

 data BaseIntensities
 	Level             : List<Float>
 	Trend             : List<Float>

 	Exp_Model_beta1       : Float
 	Exp_Model_beta2       : Float
 	Exp_Model_beta3       : Float

 	Gompertz_Makeham_A    : Float
 	Gompertz_Makeham_B    : Float
 	Gompertz_Makeham_C    : Float

 	Gompertz_Makeham_scaling_Disability    : Float

 	Exp_value_Reactivation  : Float
 	Exp_value_FreePolicy    : Float
 	Exp_value_Surrender     : Float
 end

 data GlobalParam extends Param
 	DiscountFactorName : String

 	// Benchmark_times        : List<Float>
 	// Level_male             : List<Float>
 	// Level_female           : List<Float>
 	// Trend_male             : List<Float>
 	// Trend_female           : List<Float>

 	// Observation_date           : Float
 	// Exp_Model_beta1_male       : Float
 	// Exp_Model_beta1_female     : Float
 	// Exp_Model_beta2_male       : Float
 	// Exp_Model_beta2_female     : Float
 	// Exp_Model_beta3_male       : Float
 	// Exp_Model_beta3_female     : Float

 	// Gompertz_Makeham_A_male    : Float
 	// Gompertz_Makeham_A_female  : Float
 	// Gompertz_Makeham_B_male    : Float
 	// Gompertz_Makeham_B_female  : Float
 	// Gompertz_Makeham_C_male    : Float
 	// Gompertz_Makeham_C_female  : Float

 	// Gompertz_Makeham_scaling_Disability_male    : Float
 	// Gompertz_Makeham_scaling_Disability_female  : Float

 	// Exp_value_Reactivation_male   : Float
 	// Exp_value_Reactivation_female : Float
 	// Exp_value_FreePolicy_male     : Float
 	// Exp_value_FreePolicy_female   : Float
 	// Exp_value_Surrender_male      : Float
 	// Exp_value_Surrender_female    : Float

 	// RiskMarginProxies                 : Float
 	// PresentValueFutureProfitsProxies  : Float
 	// BaseFeeParameter                  : Float
 	// FeeThreshold                      : Float
 	// LowFeeParameter                   : Float

 	// MarketInterestRateRiskMargin                 : Float
 	// MarketInterestRateBonusPotential             : Float
 	// MarketInterestRateLifeInsuranceLiabilities   : Float
 	// MarketInterestRateProfitMargin               : Float
 	// MarketInterestRateInsuranceLiabilities       : Float

 	// InvestmentReturnTaxRate : Float
 	// KRiskDividend           : Float
 	// KExpense                : Float
 end

 data GlobalInput extends Input
 	BiometricScenarioTimes     : List<Float>
 	EconomicScenarioTimes      : List<Float>
 	BiometricScenario          : Func<Float, Func<Float, Float>>
 	DiscountFactors            : Map<String, InterpretedDiscountFactors>
 	CalculationTime            : Float
 	FreePolicyRules            : Map<String, FreePolicyRule>
 end

 data InterpretedDiscountFactors
 	DiscountFactors         : Func<Float, Func<Float, Float>>
 	RealisedDiscountFactors : Func<Float, Func<Float, Float>>
 end

 data EquityParam extends Param
   //  PAL_asset : Float
   //  ScalingFactorPremiumsToEquity : Float
   //  ScalingFactorBenefitsFromEquity : Float
 end

 data Equity extends BaseEntity
 	Param                         : EquityParam
 	Input 						  : EquityInput
 	Result 						  : EquityResult
 	Assets                        : List<Asset>
 	Policies                      : List<Policy>
 	Reserve                       : Reserve
 	PremiumScale                  : Float
 	BenefitScale                  : Float
 	PAL                    		  : PALTax
 end

 data EquityInput extends Input
 	Reserve : Reserve
 end

 data EquityResult extends Result
 	PeriodPremiums : Float
 	PeriodBenefits : Float
 	IsCalculationEndTimeBeforePeriodEnd : Bool
 	PeriodActualExpense                 : Float
 	PeriodRealisedReturn                : Float
 	AccumulatedReserveAtEnd             : Reserve
 end

 data Transfers
 	Values      : List<Float>
 	TimePoints  : List<Float>
 end

 data CashFlow  extends BaseEntity
 	Input               : CashFlowInput
 	Param               : CashFlowParam
 	Policy              : Policy
 	ScalingFactor       : Float
 	IsScaled            : Bool
 end

 data CashFlowInput extends Input
 	Transfers : Transfers
 end

 data CashFlowParam extends Param
 	// Cash_flow_equity_type : String
 end

 data ActualExpense extends CashFlow

 end

 data WeParam extends CashFlowParam
 	// Technical_interest_rate 	: Float
 	// Technical_reserve        	: Float
 	// IsScaled                  : Bool
 end

 data WithExpenses extends CashFlow
 	TechnicalInterestRate 	: Float
 	Param               	: WeParam
 	TechnicalReserveSequence : IFunction
 	TechnicalReserve         : Float
 end

 data WithoutExpenses extends CashFlow
 	TechnicalInterestRate 	: Float
 	RiskContributions   	: Map<String, Transfers>
 	IsScaled            	: Bool
 	Param               	: WeParam
 	TechnicalReserveSequence : IFunction
 	TechnicalReserve         : Float
 end

 data PolicyPaidExpense extends CashFlow
 end

 data PolicyParam extends Param
 	// PAL_asset : Float
 end

 // Expenses of Policy is found in the Expense group
 data Policy extends BaseEntity
 	Result : PolicyResult
 	Input  : PolicyInput
 	Reserve : Reserve
 	Param   : PolicyParam
 	CalculationEndTime : Float
 	CashFlows :  List<CashFlow>

 	PremiumPayingCashFlows : List<String>
 	BenefitReceivingCashFlows : List<String>

 	Groups       : List<Group>
 	RiskGroups   : List<Risk>
 	InterestGroup : Group
 	ExpenseGroup : Option<Expense>

 	Equity : Option<Equity>

 	PolicyReserve          : Float, Output as CashFlow
 	ScaledCFTechReserve    : Float, Output as CashFlow
 	CFScalingFactor_Scaled : Float, Output as CashFlow
 	BenefitsLessPremiumsReserves   : Float, Output as PresentValueMid
 	BenefitsLessPremiums   		   : Float, Output as CashFlow

 	PAL                    : PALTax

 	TechnicalReservePerCashFlow : Map<String, Float>

 	// These three are only used by market rate interest Policies
 	ShareHolderFee : Float
 	TotalReserve   : Float
 	RiskMargin     : Float
 end

 data PALTax
 	InvestmentReturnTaxAsset : Float
 	InvestmentReturnTaxPaymentForPeriod : Float
 end

 data OneStateParam extends PolicyParam
 	// Policy_Reserve : Float
 	// Bonus_account : Float
 end

 data OneStatePolicy extends Policy
 	Param : OneStateParam
 end

 data ThreeStateParam extends PolicyParam
 	// These should all be options
 	// Age : Float
 	// Gender : String
 	// Policyholder_options_expiry_age : Float
 	// Policy_Reserve_FreePolicy : Reserve
 	// Policy_Reserve_Biometric : Reserve
 	// Size : Float
 end

 data ThreeStateInput extends PolicyInput
 	LumpedStateParameters  : Map<LumpedState, LumpedStateParameters>
 end

 data LumpedStateParameters
 	InitialProbability : Float
 	InitialStateProbabilities : Map<State, Float>
 end

 data ThreeStateResult extends PolicyResult
 	LumpedStateResult  : Map<LumpedState, LumpedStateResult>
 end

 data LumpedStateResult
 	AccumulatedReserveAtEnd           : Reserve
 	BenefitsLessPremiumsCashFlow      : Float
 	SurrenderCashFlow                 : Float
 	PeriodRealisedReturn              : Float
 	PeriodRiskContributionPerGroup    : Map<String, Float>
 	PeriodTechnicalExpenses           : Expenses
 	ScalingFactors                    : Map<String, Float>
 	Probability                       : Float
 	StateProbabilities                : Map<State, Float>
 end

 data ThreeStatePolicy extends Policy
 	Param : ThreeStateParam
 	Input : ThreeStateInput
 	Result : ThreeStateResult
 	Size : Float
 	FreePolicyFraction : IFunction

 	ExpiryTime : Float
 	LumpedStates : Map<LumpedState, ThreeStateState>
 end

 data ThreeStateState
 	State   : LumpedState
 	Reserve : Reserve
 	CashFlows : List<CashFlow>
 	DestinationLumpedStateIntensity : Map<LumpedStateWithSurrender, IFunction>
 	IntensitiesBiometric : Map<Pair<State, State>, IFunction>

 	AverageScalingFactor : Float
 	WeightedTechinalReserve 		 : Float

 	Probability  : Float
 end

 data PolicyResult extends Result
 	IsCalculationEndTimeBeforePeriodEnd : Bool
 	PeriodRiskContributionPerGroup : Map<String, Float>
 	PeriodTechnicalExpenses  : Expenses
 	PeriodCashFlow : Float
 	AccumulatedReserveAtEnd : Reserve
 	RealisedReturn : Float
 end

 data Expenses
 	CashFlowExpense : Option<Float>
 	FreePolicyFee   : Option<Float>
 	SurrenderFee    : Option<Float>
 	PolicyFee       : Option<Float>
 	ReserveExpense  : Option<Float>
 end

 data PolicyInput extends Input
 	CalculationEndTime : Float
 end

 data Group extends BaseEntity
 	Result   : GroupResult
 	Assets   : List<Asset>
 	Policies : List<Policy>
 end

 data ReserveParam extends Param
   // PAL_asset 				 : Float
   // RiskMargin               : Float
 	// IndividualBonusPotential : Float
 	// CollectiveBonusPotential : Float
 	// LifeInsuranceLiabilities : Float
 	// GuaranteedBenefit        : Float
 	// ProfitMargin             : Float
 	// InsuranceLiabilities     : Float
 	// Reimbursement            : Float
 end

 data ReserveGroup extends Group
 	Param               : ReserveParam
 	Reserve             : Reserve
 	PAL                 : PALTax
 	ReserveInclIRTA     : Float
 	PreviousBalance     : SingleBalance
 	Balance             : SingleBalance

 	TransferToCapitalBase         	: Float, Output as CashFlow
 	TransferToCapitalBaseReserves 	: Float, Output as PresentValueEnd
 	TransferFromCapitalBase         : Float, Output as CashFlow
 	TransferFromCapitalBaseReserves : Float, Output as PresentValueEnd
 	PALPayment        				: Float, Output as CashFlow
 	PALPaymentReserve 				: Float, Output as PresentValueEnd

 	InvestmentReturnTaxAsset        : Float, Output as CashFlow
 	GroupReserve					: Float, Output as CashFlow
 end

 data SingleBalance
 	RiskMargin : Float
 	IndividualBonusPotential  : Float
 	CollectiveBonusPotential  : Float
 	LifeInsuranceLiabilities  : Float
 	BaseCapitalFee  : Float
 	BaseCapitalInjectionForPeriod  : Float
 	GuaranteedBenefit  : Float
 	ProfitMargin  : Float
 	InsuranceLiabilities  : Float
 	Reimbursement  : Float
 end

 data FinancialReport
 	PremiumsLessBenefits 				: Float
 	RealisedReturn 						: Float
 	ActualExpenses 						: Float
 	ChangeInLifeInsuranceLiabilities 	: Float
 	ChangeInProfitMargin 				: Float
 	AnnualResult                        : Float
 end

 data InterestParam extends ReserveParam
 	// K 				  : Float
 	// K2 				  : Float
 	// Surrender_charge  : Float
 	// RiskMarginProxies : Float
 	// PresentValueFutureProfitsProxies : Float
 end

 data Interest extends ReserveGroup
 	Input           			: ReserveGroupInput
 	Param           			: InterestParam
 	SurrenderCharge 			: Float, Output as CashFlow
 	DepositRate     			: Float, Output as CashFlow
 	ScalingFactor   			: Float
 	SumOfPolicyReserves         : Float

 	GY								: Float, Output as CashFlow
 	RiskMargin						: Float, Output as CashFlow
 	ProfitMargin					: Float, Output as CashFlow
 	IndividualBonusPotential		: Float, Output as CashFlow
 	CollectiveBonusPotential		: Float, Output as CashFlow
 	Reimbursement					: Float, Output as CashFlow
 end

 data RiskParam extends ReserveParam
 	// RiskContributionWeights : Float
 	// K2Risk                  : Float
 end

 data Risk extends ReserveGroup
 	Input 			       : ReserveGroupInput
 	RiskDividend           : Float, Output as CashFlow
 	LumpedRiskDividend     : Map<LumpedState, Float>
 	RiskContributionWeight : Float
 	Param                  : RiskParam
 end

 data ExpenseParam extends ReserveParam
 	// TechnicalReserveExpense  : Float
 	// TechnicalCashFlowExpense : Float
 	// K2Expense				 : Float
 	// Technical_reserve_expense : Float
 	// Technical_cash_flow_expense : Float
 	// Technical_free_policy_fee_expense : Float
 	// Technical_surrender_fee_expense : Float
 end

 data Expense extends ReserveGroup
 	Input                 : ReserveGroupInput
 	ExpenseDividends      : Expenses
 	LumpedExpenseDividends : Map<LumpedState, Expenses>

 	TechnicalExpense      : Expenses
 	Param                 : ExpenseParam

 	ExpenseDividend       : Float, Output as CashFlow
 end

 data ReserveGroupInput extends Input
 	Reserve 			: Reserve
 end

 data MarketRateInterest extends Group
 end

 data GroupResult extends Result
 	IsCalculationEndTimeBeforePeriodEnd : Bool
 	PeriodActualExpense                 : Float
 	PeriodRealisedReturn                : Float
 	AccumulatedReserveAtEnd             : Reserve
 end

 data Projection
 	PeriodStartTime  : Float
 	PeriodEndTime 	 : Float
 	PeriodLength 	 : Float
 end
"