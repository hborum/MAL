using System;

namespace Gen
{
    public class ExternalActions
    {
        public static void ExtAction<TFunction>(TypeSpan<MAL_Equity<TFunction>> equities, TypeSpan<MAL_Group<TFunction>> groups, TypeSpan<MAL_Policy<TFunction>> policies)
            where TFunction : IFunction
        {
            Console.WriteLine("External action");
        }

        public static void TestInput<TFunction>( TypeSpan<MAL_Equity<TFunction>> equities
                                                , TypeSpan<MAL_Group<TFunction>> groups
                                                , TypeSpan<MAL_Policy<TFunction>> policies)
            where TFunction : IFunction
        {

        }


    }
}
