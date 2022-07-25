using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Gen
{
  public interface IFunction {}
  public interface ITimeSeries {}
  public interface ICashFlow {}
  public interface IReserveSequence {}
  public interface IInterpretedDiscountFactors {}
  public interface IDiscountFunctionCalculator {}
  public interface IDiscountFunction {}
  public interface LumpedState {}
  public interface State {}
  public interface ICashFlowStateParameters {}
  public interface IProjectionOutput {}
  public interface LumpedStateWithSurrender {}
  public interface IProjectionReservesResult {}
  public interface IInterpretedProjectionInput<T> {}
  public interface IProjectionOutputFactory<T> {}
  public interface IOutputValue<T> {}
  public interface ProjectionOutputEntityAnnotation {}
  //public interface
  public interface IFunLambda : IFunction {}
  public interface IFunctionFactory<TFunction> {}
  public interface ICashFlowCalculator<TFunction> {}
  public interface ICashFlowFactory {}
  public interface IReserveSequenceFactory {}
  public interface IProjectionStateFactory<TFunction> {}
  public interface IProjectionState<TFunction> {}
}
