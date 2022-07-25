namespace Gen
{
    public abstract class FMAExecutor<TState, TFunction, TOutput>
        where TFunction : IFunction
        where TOutput : IProjectionOutput
    {
        /// <summary>
        /// Takes an uninitialized state and initilizes it
        /// </summary>
        /// <param name="state"></param>
        /// <returns></returns>
        abstract public void Initialize(IInterpretedProjectionInput<TFunction> input, TState state, int ___NumberOfProjs);

        /// <summary>
        /// Performs an update step on an initilized state
        /// </summary>
        /// <param name="state"></param>
        /// <returns></returns>
        abstract public void Update(IProjectionReservesResult _result, TState state);

        /// <summary>
        /// Adds output to output collections
        /// </summary>
        /// <param name="outputNumber"></param>
        /// <returns></returns>
        abstract public void AddOutput(int outputNumber);

        abstract public TOutput Finalize(IProjectionOutputFactory<TOutput> outputFactory, TState state);
    }
}
