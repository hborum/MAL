using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace Gen
{
  public interface ICountable
  {
    int Count();
  }
  public interface IIndexable<T>
  {
    T index(int i);
  }

  public interface IUniquable<T>
  {
    T unique();
  }

  public class Transfers : ITimeSeries
  {
      public double[] Amounts {
          get;
          set;
      }

      public double[] Times {
          get;
          set;
      }

      public IReadOnlyList<double> Values { get { return Amounts; } }
      public IReadOnlyList<double> TimePoints { get { return Times; } }

      public Transfers() { }
      public Transfers(double[] times, double[] amounts) { this.Times = times; this.Amounts = amounts; }

  }

  /// <summary>
  /// This class defines all monomorphic builtin functions available in MAL.
  /// Note:
  ///   1. These definitions are used with reflection in BuiltIns.fs
  ///   2. BuiltIns.fs ignores methods starting with "___"
  ///   3. Some type translations takes place in BuiltIns.fs such as ICashFlow -> Transfers
  /// </summary>
  /// <typeparam name="TFunction"></typeparam>
  public static class BuiltIns<TFunction>
      where TFunction : IFunction
  {
    public static IEnumerable<Tuple<string, IEnumerable<string>, string>> _Methods()
    {
      var methods =
        typeof(BuiltIns<IFunLambda>)
          .GetTypeInfo()
          .DeclaredMethods
          .Where(method => !(method.Name.StartsWith("_") || method.GetCustomAttribute<MalReflectionIgnore>() != null ))
          .Select(method => new Tuple<string, IEnumerable<string>, string>
                  ( method.Name
                  , method.GetParameters().Select(param => param.ParameterType.FullName)
                  , method.ReturnType.FullName ));
      return methods;
    }

    // These fields are set by the generated statehandler
    static bool simd = false;
    public static IFunctionFactory<TFunction> FunctionFactory;
    public static ICashFlowCalculator<TFunction> CashFlowCalculator;
    public static ICashFlowFactory CashFlowFactory;
    public static IReserveSequenceFactory ReserveSequenceFactory;
    public static IProjectionStateFactory<TFunction> StateFactory;
    public static IProjectionState<TFunction> State;

    // These are only used by generated code and are not available to the user
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double?[] ___NewArr(int n){
      return new double?[n];
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double?[] ___NewArrOpt(int n, double? decider){
      return (decider.HasValue) ? new double?[n] : null;
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double?[][] ___NewArr(int n, int m)
    { var res = new double?[n][];
      for (int i = 0; i < n; i++)
        res[i] = new double?[m];
      return res;
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double?[][] ___NewArrOpt(int n, int m, Func<int, double?> decider)
    {
      var res = new double?[n][];
      for (int i = 0; i < n; i++)
          res[i] = (decider(i).HasValue) ? new double?[m] : null;
      return res;
    }
    public static void ___AddToArr<T>(T[] arr, int i, T d){
      if(arr != null)
        arr[i] = d;
    }
    public static void ___AddToArr(double[] arr, int i, double? d){
      if(arr != null && d.HasValue)
        arr[i] = (double)d;
    }

    public static void ___AddToArr<T>(T[][] arr, int i, int j, T d){
      if(arr[i] != null)
        arr[i][j] = d;
    }

    public static void ___AddToArr(double[][] arr, int i, int j, double? d){
      if(arr[i] != null && d.HasValue)
        arr[i][j] = (double)d;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool ___IsNull(Object o) { return o == null; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double vOrNumber(double? d, double n) { return d ?? n; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double vOrZero(double? d) { return d ?? 0; }

    public static double inOrderSum(IEnumerable<double> xs)
    {
      return xs.OrderByDescending(d => d).Sum();
    }

    //public static void ___AddTimeDependentGlobalOutput
    //(IDictionary<string, double[]> outputCol
    //, string key
    //, double?[] value
    //, IDictionary<string, ISet<ProjectionOutputEntityAnnotation>> annotations)
    //{
    //  if (value == null) {
    //    annotations.Remove(key);
    //    return;
    //  }
    //  outputCol.Add(key, Array.ConvertAll(value, x => (double)x));
    //  //int dCount = 0;

    //  //for (int i = 0; i < value.Length; i++)
    //  //  if (value[i].HasValue)
    //  //    dCount++;

    //  //if (dCount == 0)
    //  //{
    //  //  annotations.Remove(key);
    //  //  return;
    //  //} else if (dCount == value.Length)
    //  //{
    //  //  outputCol.Add(key, value.Cast<double>().ToArray());
    //  //} else
    //  //{
    //  //  outputCol.Add(key, value.Where(d => d.HasValue).Cast<double>().ToArray());
    //  //}
    //}

    public static void ___AddTimeDependentGlobalOutput
    ( List<IOutputValue<double?[]>> outputCol
    , string key
    , double?[] value
    , IDictionary<string, ISet<ProjectionOutputEntityAnnotation>> annotations)
    {
      if (value == null)
      {
        annotations.Remove(key);
        return;
      }
      //outputCol.Add(OutputValueFactory.CreateGlobal(key, value));
    }

    public static void ___AddScalarGlobalOutput
    ( List<IOutputValue<double?>> outputCol
    , string key
    , double? value
    , IDictionary<string, ISet<ProjectionOutputEntityAnnotation>> annotations)
    {
      //if (value == null)
      //  annotations.Remove(key);
      //else
      //  outputCol.Add(OutputValueFactory.CreateGlobal(key, value));
    }

    public static void ___AddTimeDependentGroupOutput
        ( List<IOutputValue<double? []>> outputCol
        , string key
        , double?[][] value
        , int length
        , Func<int,string> items
        , IDictionary<string, ISet<ProjectionOutputEntityAnnotation>> annotations)

    {
      //___AddTimeDependentPolicyGroupOutput(outputCol, key, value, length, items, annotations, OutputValueFactory.CreateGroupDependent);
    }

    public static void ___AddTimeDependentPolicyOutput
        ( List<IOutputValue<double? []>> outputCol
        , string key
        , double?[][] value
        , int length
        , Func<int,string> items
        , IDictionary<string, ISet<ProjectionOutputEntityAnnotation>> annotations)
    {
      //___AddTimeDependentPolicyGroupOutput(outputCol, key, value, length, items, annotations, OutputValueFactory.CreatePolicyDependent);
    }

    private static void ___AddTimeDependentPolicyGroupOutput
        (List<IOutputValue<double?[]>> outputCol
        , string key
        , double?[][] value
        , int length
        , Func<int,string> items
        , IDictionary<string, ISet<ProjectionOutputEntityAnnotation>> annotations
        , Func<string, string, double?[], IOutputValue<double?[]>> f
        )
    {
      var nothingAdded = true;
      for (int i = 0; i < length; i++) {
        if (value[i] != null) {
          outputCol.Add(f(key, items(i), value[i]));
          nothingAdded = false;
        }
      }
      if (nothingAdded) {
        annotations.Remove(key);
        return;
      }
    }

    public static void ___AddScalarPolicyOutput
        ( List<IOutputValue<double?>> outputCol
        , string key
        , double?[] value
        , int length
        , Func<int,string> items
        , IDictionary<string, ISet<ProjectionOutputEntityAnnotation>> annotations)
    {
      //___AddScalarPolicyGroupOutput(outputCol, key, value, length, items, annotations, OutputValueFactory.CreatePolicyDependent);
    }
    public static void ___AddScalarGroupOutput
        ( List<IOutputValue<double?>> outputCol
        , string key
        , double?[] value
        , int length
        , Func<int,string> items
        , IDictionary<string, ISet<ProjectionOutputEntityAnnotation>> annotations)
    {
      //___AddScalarPolicyGroupOutput(outputCol, key, value, length, items, annotations, OutputValueFactory.CreateGroupDependent);
    }

    private static void ___AddScalarPolicyGroupOutput
        ( List<IOutputValue<double?>> outputCol
        , string key
        , double?[] value
        , int length
        , Func<int,string> items
        , IDictionary<string, ISet<ProjectionOutputEntityAnnotation>> annotations
        , Func<string, string, double?, IOutputValue<double?>> f)
    {
      var nothingAdded = true;
      for (int i = 0; i < length; i++)
      {
        var vs = value[i];
        if (vs != null) {
          outputCol.Add(f(key, items(i), vs));
          nothingAdded = false;
        }
      }
      if (nothingAdded)
      {
        annotations.Remove(key);
      }
    }

    public static void ___AddTimeDependentOutput
        (IDictionary<string, IDictionary<string, double[]>> outputCol
        , string key
        , double?[][] value
        , List<string> items
        , IDictionary<string, ISet<ProjectionOutputEntityAnnotation>> annotations)
    {
      bool nothingAdded = true;

      if (!outputCol.ContainsKey(key) )
        outputCol.Add(key,new Dictionary<string, double[]>(value.Length));
      var dict = outputCol[key];
      for (int i = 0; i < items.Count; i++)
      {
        var vs = value[i];
        if(vs == null)
          continue;
        nothingAdded = false;
        dict[items[i]] = Array.ConvertAll(vs, x => (double)x);
      }
      if (nothingAdded) {
        annotations.Remove(key);
        outputCol.Remove(key);
      }

      //bool nothingAdded = true;
      //for (int i = 0; i < items.Count; i++)
      //{
      //  var vs = value[i];
      //  var dCount = 0;
      //  for (int j = 0; j < vs.Length; j++)
      //    if (vs[j] != null)
      //      dCount++;
      //  if (dCount > 0)
      //  {
      //    nothingAdded = false;
      //    var ds = new double[dCount];
      //    dCount = 0;
      //    for (int j = 0; j < vs.Length; j++)
      //      if (vs[j] != null)
      //        ds[dCount++] = (double)vs[j];

      //    if (!outputCol.ContainsKey(key))
      //    {
      //      outputCol.Add(key, new Dictionary<string, double[]>());
      //    }
      //    var dict = outputCol[key];

      //    dict[items[i]] = ds;
      //  }
      //}
      //if (nothingAdded)
      //  annotations.Remove(key);
    }

    public static double? ___FinalizeDiscountValue(
      double[] timesArr,
      double?[] valuesArr,
      Func<double, double> realisedDiscountFactor,
      int enumMethod,
      Tuple<double, double>[] realisedDiscountFactorContinuousPaymentsArr,
      Tuple<double, double>[] realisedDiscountFactorContinuousPaymentsInclPolicyEndTimesArr)
    {
      if (valuesArr == null) {
        return null;
      }
      return ___FinalizeDiscountValue(timesArr, Array.ConvertAll(valuesArr, x => (double)x), realisedDiscountFactor, enumMethod, realisedDiscountFactorContinuousPaymentsArr, realisedDiscountFactorContinuousPaymentsInclPolicyEndTimesArr);
      //var dCount = 0;
      //for (int i = 0; i < valuesArr.Length; i++)
      //{
      //  if (valuesArr[i] != null)
      //    dCount++;
      //}
      //if (dCount == 0)
      //  return null;
      //var filtered_values = new double[dCount];
      //var filtered_times = new double[dCount + 1];
      //filtered_times[0] = timesArr[0];
      //dCount = 0;
      //for (int i = 0; i < valuesArr.Length; i++)
      //{
      //  if (valuesArr[i] != null) {
      //    filtered_values[dCount] = (double)valuesArr[i];
      //    filtered_times[++dCount] = timesArr[i + 1];
      //  }
      //}

      //return ___FinalizeDiscountValue(filtered_times, filtered_values, realisedDiscountFactor, enumMethod, realisedDiscountFactorContinuousPaymentsArr, realisedDiscountFactorContinuousPaymentsInclPolicyEndTimesArr);
    }

    public static double ___FinalizeDiscountValue(
    double[] timesArr,
    double[] valuesArr,
    Func<double, double> realisedDiscountFactor,
    int enumMethod,
    Tuple<double, double>[] realisedDiscountFactorContinuousPaymentsArr,
    Tuple<double, double>[] realisedDiscountFactorContinuousPaymentsInclPolicyEndTimesArr)
    {
      if (valuesArr == null)
      {
        return float.NaN;
      }
      var values = new List<double>(valuesArr);
       var times = new List<double>( timesArr );
       var realisedDiscountFactorContinuousPayments = new List<Tuple<double,double>>(realisedDiscountFactorContinuousPaymentsArr);
       var realisedDiscountFactorContinuousPaymentsInclPolicyEndTimes = new List<Tuple<double,double>>(realisedDiscountFactorContinuousPaymentsInclPolicyEndTimesArr);

       switch (enumMethod)
        {
          case 1: //End
            return Double.NaN; //CashFlowCalculator.DiscountCashFlowByEndPointSummation(values, realisedDiscountFactor, times);
          case 0: //mid
            {
              if(times == null)
              {
                var res = 0.0;
                for (var i = 0; i < valuesArr.Length; i++)
                {
                  res += valuesArr[i] * (realisedDiscountFactorContinuousPayments[i].Item2);
                }
                return res;
              }
              else
              {
                IEnumerable<double> discounts = _GetRealisedDiscountsForNonStandardContinuousPayments(times, realisedDiscountFactorContinuousPayments, realisedDiscountFactorContinuousPaymentsInclPolicyEndTimes);
                var res = 0.0;
                var enumerator = discounts.GetEnumerator();
                for (var i = 0; i < valuesArr.Length; i++)
                {
                  enumerator.MoveNext();
                  res += valuesArr[i] * enumerator.Current;
                }
                return res;
              }
            }
          default:
            throw new ArgumentException("The given output format does not support a discount method.");
        }
    }

    private static IEnumerable<double> _GetRealisedDiscountsForNonStandardContinuousPayments(
        List<double> cashFlowTimePoints
      , List<Tuple<double, double>> realisedDiscountFactorContinuousPayments
      , List<Tuple<double, double>> realisedDiscountFactorContinuousPaymentsInclPolicyEndTimes)
    {
      var times = cashFlowTimePoints;

      // We only need to change the discount value for the last period by calculating it from the relevant
      // discounts in the extended discounts.
      var relevantTimePointsWithDiscounts =
        realisedDiscountFactorContinuousPayments.GetRange(0, times.Count() - 1);

      var lastCalculationTimePointBeforePolicyEndTime = relevantTimePointsWithDiscounts.Last().Item1;
      var discounts = relevantTimePointsWithDiscounts.Select(tup => tup.Item2);

      var lastCashFlowTimePoint = times.Last();
      var relevantNonStandardDiscounts = realisedDiscountFactorContinuousPaymentsInclPolicyEndTimes
        .SkipWhile(tup => tup.Item1 <= lastCalculationTimePointBeforePolicyEndTime)
        .TakeWhile(tup => tup.Item1 < lastCashFlowTimePoint);

      // To handle that there can be more time points between the last time point in the standard discount list
      // and the last time point in cashFlowTimePoints, we add the discount values up with proper weighting when
      // calculating the last discount value.
      var discountValue = 0.0;
      var lastEndPoint = lastCalculationTimePointBeforePolicyEndTime;
      foreach (var timeAndDiscountValue in relevantNonStandardDiscounts)
      {
        discountValue += timeAndDiscountValue.Item2 * (timeAndDiscountValue.Item1 - lastEndPoint);
        lastEndPoint = timeAndDiscountValue.Item1;
      }

      return discounts.Concat(new[] { discountValue / (lastEndPoint - lastCalculationTimePointBeforePolicyEndTime) });
    }

    public static double?[] ___FinalizeDiscount(
      double[] times,
      double?[][] inp,
      Func<double, double> realisedDiscountFactor,
      int enumMethod,
      Tuple<double, double>[] realisedDiscountFactorContinuousPayments
      , Tuple<double, double>[] realisedDiscountFactorContinuousPaymentsInclPolicyEndTimes)
    {
      var res = new double?[inp.Length];
      for (int i = 0; i < inp.Length; i++)
      {
        var vs = inp[i];
        if (vs == null) {
          res[i] = null;
          continue;
        }

        var toDiscount = Array.ConvertAll(vs, x => (double)x);
        var toDiscountTime = times;

        res[i] = ___FinalizeDiscountValue(toDiscountTime, toDiscount, realisedDiscountFactor, enumMethod, realisedDiscountFactorContinuousPayments, realisedDiscountFactorContinuousPaymentsInclPolicyEndTimes);
      }
      return res;
      //var res = new double?[inp.Length];
      //for (int i = 0; i < inp.Length; i++)
      //{
      //  var dCount = 0;
      //  var vs = inp[i];
      //  for (int j = 0; j < vs.Length; j++)
      //  {
      //    if (vs[j].HasValue)
      //      dCount++;
      //  }
      //  if (dCount == 0) {
      //    res[i] = null;
      //    continue;
      //  }

      //  double[] toDiscount;
      //  double[] toDiscountTime;
      //  if (dCount == vs.Length) {
      //    toDiscount = vs.Cast<double>().ToArray();
      //    toDiscountTime = times;
      //  }
      //  else
      //  {
      //    toDiscount = new double[dCount];
      //    toDiscountTime = new double[dCount + 1];
      //    toDiscountTime[0] = times[0];
      //    dCount = 0;
      //    for (int j = 0; j < vs.Length; j++)
      //    {
      //      if (vs[j].HasValue)
      //      {
      //        toDiscount[dCount] = (double)vs[j];
      //        toDiscountTime[++dCount] = times[j + 1];
      //      }
      //    }
      //  }
      //  res[i] = ___FinalizeDiscountValue(toDiscountTime, toDiscount, realisedDiscountFactor, enumMethod, realisedDiscountFactorContinuousPayments, realisedDiscountFactorContinuousPaymentsInclPolicyEndTimes);
      //}
      //return res;
    }

    public static double?[] ___FinalizeDiscount(
      double[] times,
      double[][] inp,
      Func<double, double> realisedDiscountFactor,
      int enumMethod,
      Tuple<double, double>[] realisedDiscountFactorContinuousPayments
      , Tuple<double, double>[] realisedDiscountFactorContinuousPaymentsInclPolicyEndTimes)
    {
      var res = new double?[inp.Length];
      for (int i = 0; i < inp.Length; i++) {
        var d = ___FinalizeDiscountValue(times, inp[i], realisedDiscountFactor, enumMethod, realisedDiscountFactorContinuousPayments, realisedDiscountFactorContinuousPaymentsInclPolicyEndTimes);
        if (!double.IsNaN(d))
          res[i] = d;
      }
      return res;
    }

    // General accessible
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double abs(double v) { return Math.Abs(v); }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double accumulateCashFlowValues(ICashFlow cf1, double startTime, double endTime)
    {
      return Double.NaN; //CashFlowCalculator.AccumulateCashFlowValues(cf1, startTime, endTime);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static ICashFlow addCashFlows(ICashFlow cf1, ICashFlow cf2)
    {
      return sumCashFlows(new ICashFlow[] { cf1, cf2 });
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool all(IEnumerable<bool> vals)
    {
      return vals.All(i => i);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool any(IEnumerable<bool> vals)
    {
      return vals.Any(i => i);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T[] append<T>(T[] xs1, T[] xs2)
    {
      var ys = new T[xs1.Length + xs2.Length];
      Array.Copy(xs1,ys,xs1.Length);
      Array.Copy(xs2,0, ys,xs1.Length, xs2.Length);
      return ys;
    }

    public static void assert(bool b)
    {
      if (!b) {
        throw new Exception("Assertion in MAL, does not hold");
      }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double avg(double[] xs)
    {
      //double sum = 0;
      //int simdLength = Vector<double>.Count;
      //var sumV = new Vector<double>(0);
      //int i = 0;
      //if (simd)
      //{
      //  for (; i < xs.Length - simdLength + 1; i += simdLength)
      //  {
      //    var v = new Vector<double>(xs, i);
      //    sumV += v;
      //  }
      //  for (int j = 0; j < simdLength; j++)
      //  {
      //    sum += sumV[j];
      //  }
      //}
      //for (; i < xs.Length; i++)
      //{
      //  sum += xs[i];
      //}
      //return sum / xs.Length;

      return xs.Average();
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double bound(double value, double lowerBound, double upperBound)
    {
      return Math.Min(Math.Max(value, lowerBound), upperBound);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static IReserveSequence calculateReserveSequence(ICashFlow cf, Func<double, double> discounter)
    {
      return null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int counti<T, U>(IDictionary<T, U> xs) { return xs.Count; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double count<T, U>(IDictionary<T, U> xs) { return xs.Count; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double count(ICountable xs) { return xs.Count(); }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int counti(ICountable xs) { return xs.Count(); }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int counti<T>(TypeSpan<T> xs) where T : class { return xs.Length; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double count<T>(TypeSpan<T> xs) where T : class  { return xs.Length; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int counti<T>(T[] xs) { return xs.Length; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double count<T>(T[] xs) { return xs.Length; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double count<T>(IEnumerable<T> xs) { return xs.Count(); }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int counti<T>(IEnumerable<T> xs) { return xs.Count(); }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static ICashFlow createCashFlow(double[] values)
    {
      return null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static ICashFlow createCashFlowFromTimeSeries(ITimeSeries timeSeries)
    {
      return null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static ICashFlow createEmptyCashFlow()
    {
      return null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static ITimeSeries dynamicGlobalParamTimeSeries(string name)
    {
      return null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static ITimeSeries dynamicCashFlowParamTimeSeries(MAL_CashFlow<TFunction> cf, string name)
    {
      if (cf.MAL_Policy is MAL_OneStatePolicy<TFunction> p1)
      {
        return null;
      }
      else if (cf.MAL_Policy is MAL_ThreeStatePolicy<TFunction> p3)
      {
        foreach (var ls in p3.MAL_LumpedStates)
        {
          foreach (var other_cf in ls.Value.MAL_CashFlows)
          {
            if (other_cf == cf)
              return null;
          }
        }
        throw new Exception("Impossible: Could not find CashFlow");
      }
      throw new Exception("Impossible: Policy is neither one or three state");
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool dynamicCashFlowParamExists(MAL_CashFlow<TFunction> cf, string name)
    {
      if (cf.MAL_Policy is MAL_OneStatePolicy<TFunction> p1)
      {
        return false;
      }
      else if (cf.MAL_Policy is MAL_ThreeStatePolicy<TFunction> p3)
      {
        foreach (var ls in p3.MAL_LumpedStates)
        {
          foreach (var other_cf in ls.Value.MAL_CashFlows)
          {
            if (other_cf == cf)
              return false;
          }
        }
        throw new Exception("Impossible: Could not find CashFlow");
      }
      throw new Exception("Impossible: Policy is neither one or three state");
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool dynamicGlobalParamBool(string name)
    {
      return false;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool dynamicGlobalParamExists(string name)
    {
      return false;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Tuple<double, double>[] realisedDiscountFactorContinuousPayments(IInterpretedDiscountFactors calc, double time, double[] timePoints)
    {
      return null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static IDiscountFunctionCalculator createDiscountFunctionCalculator(IInterpretedDiscountFactors calc, Func<IDiscountFunction, Func<double, double>> modifier)
    {
      return null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static TFunction createIntervalRestrictedFunction(double limit, TFunction fun)
    {
      return default(TFunction);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static IReserveSequence createReserveSequence(ITimeSeries tr)
    {
      return null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static IReserveSequence createEmptyReserveSequence()
    {
      return null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static IReadOnlyDictionary<T, U> createMap<T, U>(Tuple<T, U>[] entries)
    {
      return entries.ToDictionary(x => x.Item1, x => x.Item2);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static IFunction createPiecewiseConstant(Transfers transfers)
    {
      return null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static IFunction createSum(IFunction[] funs)
    {
      return null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double discount(ICashFlow transfers, double interest, double start)
    {
      return Double.NaN;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double discountSubTax(ICashFlow transfers, Func<double, double> discountFactor, double currentTime, double palRate)
    {
      return Double.NaN;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T fst<T,U>(Tuple<T,U> pair)
    {
      return pair.Item1;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double? firstOrNone(double[] xs)
    {
      if (xs.Length > 0)
        return xs[0];
      else
        return null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T firstOrNone<T>(T[] xs)
    {
      return xs.First();
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T getValue<T>(T v)
    {
      if (v == null)
        throw new Exception("MAL: call to getValue on a None(null) value");
      return v;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double getValue(double? v)
    {
      if (v == null)
        throw new Exception("MAL: call to getValue on a None(null) value");
      return v.Value;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool getValue(bool? v)
    {
      if (v == null)
        throw new Exception("MAL: call to getValue on a None(null) value");
      return v.Value;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static ICashFlow guaranteedBenefitCashFlowLumped1State(MAL_OneStatePolicy<TFunction> p1, double periodStartTime)
    {
      return null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static ICashFlow guaranteedBenefitCashFlowLumped3State
      ( MAL_ThreeStatePolicy<TFunction> p3
      , double periodStartTime
      , IReadOnlyDictionary<LumpedState, double> lumpedStateProbabilities
      , IReadOnlyDictionary<LumpedState, IReadOnlyDictionary<State, double>> lumpedStateBiometricProbabilities
      , IReadOnlyDictionary<LumpedState, IReadOnlyDictionary<string, IReserveSequence>> lumpedStateCashFlowNameTechnicalReserveSequence
      , double policyHolderOptionExpiryTime
      )
    {
      return null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T index<T>(T[] xs, double i)
    {
      return xs[(int) i];
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool isEmpty<T>(T[] xs)
    {
      return xs.Length == 0;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool isEmpty(ICountable xs)
    {
      return xs.Count() == 0;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool isEmpty<T>(TypeSpan<T> xs) where T : class
    {
      return xs.Length == 0;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool isPermutation<T>(T[] xs, T[] ys)
    {
      if (xs.Length != ys.Length) {
        return false;
      }
      foreach(var x in xs) {
        if(!ys.Contains(x))
          return false;
      }
      return true;
    }

    //[MethodImpl(MethodImplOptions.AggressiveInlining)]
    //public static U[] mapToValues<T,U>(IDictionary<T,U> dict) => dict.Values.ToArray();

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double log(double v)
    {
      return Math.Log(v);
    }

    public static double min(double v1, double v2) { return Math.Min(v1, v2); }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double maxOfList(double[] xs)
    {
      return xs.Max();
      //var simdLength = Vector<double>.Count;
      //var maxV = new Vector<double>(double.MinValue);
      //int i = 0;
      //double max = double.MinValue;
      //if (simd)
      //{
      //  for (; i < xs.Length - simdLength + 1; i += simdLength)
      //  {
      //    maxV = Vector.Max(maxV, new Vector<double>(xs, i));
      //  }
      //  for (int j = 0; j < simdLength; j++)
      //  {
      //    max = Math.Min(max, maxV[j]);
      //  }
      //}
      //for (; i < xs.Length; i++)
      //    max = Math.Max(xs[i], max);
      //return max;
    }

    public static double max(double v1, double v2) { return Math.Max(v1, v2); }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double minOfList(double[] xs)
    {
      return xs.Min();
      //var simdLength = Vector<double>.Count;
      //var minV = new Vector<double>(double.MaxValue);
      //int i = 0;
      //double min = double.MaxValue;
      //if (simd) {
      //  for (; i < xs.Length - simdLength + 1; i += simdLength) {
      //      minV = Vector.Min(minV, new Vector<double>(xs, i));
      //  }
      //  for (int j = 0; j < simdLength; j++) {
      //      min = Math.Min(min, minV[j]);
      //  }
      //}
      //for (; i < xs.Length; i++)
      //  min = Math.Min(xs[i], min);
      //return min;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static TFunction linearInterpolation(double[] times, double[] values)
    {
      return default(TFunction);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double pow(double x, double y)
    {
      return Math.Pow(x, y);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T single<T>(IEnumerable<T> source)
    {
      return source.Single();
    }


    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T single<T>(IIndexable<T> source)
    {
      return source.index(0);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double[] range(double start, double end)
    {
      start = Math.Floor(start);
      end = Math.Floor(end);
      double[] ret = new double[(int)end - (int)start + 1];
      int count = 0;
      for (double i = start; i <= end; i++) {
        ret[count++] = i;
      }
      return ret;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static ICashFlow scale(ICashFlow transfers, double scale)
    {
      return null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double[] scaleCashFlow(double[] cf, double scale)
    {
      double[] sCf = new double[cf.Length];
      int i = 0;

      for (; i < cf.Length; i++)
      {
        sCf[i] = cf[i] * scale;
      }

      return sCf;
      //double[] sCf = new double[cf.Length];
      //int simdLength = Vector<double>.Count;
      //int i = 0;
      //if (simd)
      //  for (; i < cf.Length - simdLength + 1; i += simdLength) {
      //      var vector = new Vector<double>(cf, i);
      //      (vector * scale).CopyTo(sCf, i);
      //  }

      //for (; i < cf.Length; i++) {
      //  sCf[i] = cf[i] * scale;
      //}

      //return sCf;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T[] skip<T>(double d, T[] xs)
    {
      int toSkip = (int)d;
      T[] ret = new T[xs.Length - toSkip];
      for (int i = toSkip; i < xs.Length; i++)
        ret[i - toSkip] = xs[i];
      return ret;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T[] skipTail<T>(double d, T[] xs)
    {
      int toSkip = (int)d;
      T[] ret = new T[xs.Length - toSkip];
      for (int i = 0; i < xs.Length - toSkip; i++)
        ret[i] = xs[i];
      return ret;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static U snd<T,U>(Tuple<T,U> pair)
    {
      return pair.Item2;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double[] sort(double[] xs)
    {
      var ys = new double[xs.Length];
      Array.Copy(xs,ys,xs.Length);
      Array.Sort(ys);
      return ys;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double[] appendUniqueSort(double[] xs, double[] ys)
    {
      var sortedSet = new SortedSet<double>();
      for (int i = 0; i < xs.Length; i++) {
        sortedSet.Add(xs[i]);
      }
      for (int i = 0; i < ys.Length; i++) {
        sortedSet.Add(ys[i]);
      }
      return sortedSet.ToArray();
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double sqr(double x) => x * x;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double sqrt(double x) => Math.Sqrt(x);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Func<IDiscountFunction, Func<double, double>> subtractInvestmentFeeAndPal(double investmentFee, double palPercent)
    {
      Func<Func<double, double> , Func<double, double> > subInvest =
        (discountFunction => t => Math.Pow(Math.Pow(discountFunction(t), -1 / t) * (1 - investmentFee), -t));
      Func<Func<double, double> , Func<double, double> > subPal =
        (discountFunction => t => Math.Pow(1 + (Math.Pow(discountFunction(t), -1 / t) - 1) * (1 - palPercent), -t));

      return discountFunction => subPal(subInvest(f => f));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double sum(double[] xs)
    {
      //int simdLength = Vector<double>.Count;
      //var sumV = new Vector<double>(0);
      double sum = 0;
      for (int i = 0; i < xs.Length; i++)
      {
        sum += xs[i];
      }
      return sum;
    }

    [MalReflectionIgnore]
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Dictionary<V,W> TypeSpanToDictionary<T,U,V,W>(T xs, Func<U, V> key, Func<U,W> value)
      where T : ICountable, IIndexable<U>
    {
      var dict = new Dictionary<V,W>();
      for (int i = 0; i < xs.Count(); i++) {
        var el = xs.index(i);
        dict.Add(key(el), value(el));
      }
      return dict;
    }

    [MalReflectionIgnore]
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static List<W> TypeSpanToList<T,U,W>(T xs, Func<U,W> value)
      where T : ICountable, IIndexable<U>
    {
      var ls = new List<W>();
      for (int i = 0; i < xs.Count(); i++) {
        var el = xs.index(i);
        ls.Add(value(el));
      }
      return ls;
    }

    [MalReflectionIgnore]
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static ICashFlow sumCashFlows<T>(T xs)
      where T : ICountable, IIndexable<ICashFlow>
    {
      List<ICashFlow> cfs = new List<ICashFlow>();
      for (int i = 0; i < xs.Count(); i++)
        cfs.Add(xs.index(i));
      return null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static ICashFlow sumCashFlows(ICashFlow[] xs)
    {
      return null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Func<double, double> timeFun(double v)
    {
      return (time => Math.Pow(v, time));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Func<double, double> timeNegFun(double v)
    {
      return (time => Math.Pow(v, -time));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T[] unique<T>(T[] xs)
    {
      return xs.Distinct().ToArray();
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T unique<T>(IUniquable<T> xs)
    {
      return xs.unique();
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T[] unique<T,U>(U xs)
      where U : ICountable, IIndexable<T>
    {
      HashSet<T> arr = new HashSet<T>();
      for (int i = 0; i < xs.Count(); i++) {
        arr.Add(xs.index(i));
      }
      return arr.ToArray();
    }

    [MalReflectionIgnore]
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static TypeSpan<T> unique<T>(TypeSpan<T> xs) where T : class
    {
        var ranges = (xs.Ranges != null) ? xs.Ranges?.Keys.ToArray() : null;
        return new TypeSpan<T>(xs.Distinct().Select(i => (object)i).ToArray(), ranges);
    }

    public static TFunction gompertzMakeham(double A, double B, double C, double age)
    {
      return default(TFunction);
    }

    public static TFunction scaleIFun(TFunction f, double scale)
    {
      return default(TFunction);
    }

    public static TFunction expModel(double beta1, double beta2, double beta3, double age)
    {
      return default(TFunction);
    }

    public static TFunction multiply(IFunction f, IFunction g)
    {
      return default(TFunction);
    }

    public static TFunction benchmark(double[] benchmark_times, double[] level, double[] trend, double age, double timeSinceObservationDate)
    {
      return default(TFunction);
    }

    public static TFunction createPiecewiseExponential(double[] times, double[] values)
    {
      return default(TFunction);
    }

    public static double exp(double v)
    {
      return Math.Exp(v);
    }

    public static double discountByIDiscountFunction(ICashFlow transfers, IDiscountFunction f, double periodEndTime)
    {
      return Double.NaN;
    }
    public static double discountByFun(ICashFlow transfers, Func<double, double> f, double periodEndTime)
    {
      return Double.NaN;
    }

    public static double discountSubtractPalAndInvestment(ICashFlow transfers, Func<double, double> f, double palRate, double investmentFee, double periodEndTime)
    {
      Func<double, double> discountSubtractInvestment = x => Math.Pow(Math.Pow(f(x), -1 / x) * (1 - investmentFee), -x);
      Func<double, double> discountSubtractPal = (x => Math.Pow(1 + (Math.Pow(discountSubtractInvestment(x), -1 / x) - 1) * (1 - palRate), -x));
      return discountByFun(transfers, discountSubtractPal, periodEndTime);
    }

    private static IReadOnlyDictionary<string, ICashFlowStateParameters> __convertCashFlows(TypeSpan<MAL_CashFlow<TFunction>> cfs)
    {
      return null;
    }

    public static T[] mapToKeys<T, U>(IDictionary<T, U> dict)
    {
      return dict.Keys.ToArray();
    }

    public static double print(double d)
    {
      Console.Write(d);
      return d;
    }

    public static double[] print(double[] ds)
    {
      Console.WriteLine("[");
      foreach(var d in ds)
        Console.WriteLine(print(d) + ",");
      Console.WriteLine("]");
      return ds;
    }

    public static T print<T>(T o)
    {
      if (o != null)
        Console.Write(o.ToString());
      else
        Console.Write("None");
      return o;
    }

    public static double println(double d)
    {
      print(d);
      Console.WriteLine();
      return d;
    }

    public static double[] println(double[] ds)
    {
      print(ds);
      Console.WriteLine();
      return ds;
    }

    public static T println<T>(T o)
    {
      print(o);
      Console.WriteLine();
      return o;
    }
  }
}
