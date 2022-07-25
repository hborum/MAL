using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace Gen
{
    public static class TypeSpanStats
    {
        public static int expensiveConst = 0;
        public static int cheapConst = 0;
        public static int resizeConst = 0;
    }

    //public interface ITypeSpan<out T> : IEnumerable<T>
    //{
    //  T this[int index] { get; }
    //  TypeSpan<U> Filter<U>();
    //}

    public class TypeSpan<T> : IEnumerable<T> where T : class
    {
        // Invariant:
        // Ranges[`type`] is an exclusive range ] l; u [
        // s.t. if i is in the range ] l; u [  then elems[i] is of `type`
        public readonly object[] Elems;
        public readonly IDictionary<Type, Tuple<int, int>> Ranges;
        private readonly Func<int, int> Offset = i => i;
        public readonly int Length;

        // We will at most consider 5 different types.
        // This could be automatically infered and generated
        private const int maxTypes = 5;
        private readonly static int[] lowTypeCounter = new int[maxTypes];
        private readonly static int[] typeCounter = new int[maxTypes];

        //Create indexer
        public T this[int index]
        {
            get => (T)(Ranges is null ? Elems[index] : Elems[Offset(index)]);
        }

        public TypeSpan(object[] elems, IDictionary<Type, Tuple<int, int>> ranges, int count, Func<int,int> offsetter)
        {
            TypeSpanStats.cheapConst++;
            this.Elems = elems;
            this.Ranges = ranges;
            this.Length = count;
            this.Offset = offsetter;
        }

        public TypeSpan<U> Filter<U>() where U : class
        {
            if (this.Ranges == null)
              return SimpleFilter<U>();
            else
              return ConstantFilter<U>();
        }

        private TypeSpan<U> SimpleFilter<U>() where U : class
        {
            var newElems = new object[Length];
            var count = 0;
            for (int i = 0; i < Length; i++) {
                var elem = Elems[i];
                if(elem is U)
                {
                    newElems[count++] = elem;
                }
            }
            return new TypeSpan<U>(newElems, (IDictionary<Type, Tuple<int, int>>) null, count, null);
        }

        private TypeSpan<U> ConstantFilter<U>() where U : class
        {
            var newRanges = new Dictionary<Type, Tuple<int, int>>();
            var newCount = 0;
            var offsets = new List<(int, int)>();
            foreach (var kvp in Ranges) {
                var segLength = kvp.Value.Item2 - kvp.Value.Item1 - 1;
                if (typeof(U).IsAssignableFrom(kvp.Key))
                {
                    newRanges[kvp.Key] = kvp.Value;
                    newCount += segLength;
                }
                else offsets.Add((newCount, segLength));
            }
            int offset_func(int i) { var j = i; foreach (var kvp in offsets) if (j >= kvp.Item1) i += kvp.Item2; return i; }
            return new TypeSpan<U>(Elems, newRanges, newCount, offset_func);
        }

        public TypeSpan(object[] elems, Type[] types) : this(elems, types, elems.Length)
        {
            TypeSpanStats.resizeConst++;
            TypeSpanStats.expensiveConst--;
        }

        /// <summary>
        /// Creates a TypeSpan of elements
        /// </summary>
        /// <param name="elems">The elements in the span</param>
        /// <param name="types">The types of the elements in the span</param>
        /// <param name="length">We only span the first maxCount elems</param>
        public TypeSpan(object[] elems, Type[] types, int length)
        {
            this.Length = length;

            if (types == null || length / types.Length < 20)
            {
                this.Elems = elems;
            }
            else
            {
                this.Ranges = new Dictionary<Type, Tuple<int, int>>();
                TypeSpanStats.expensiveConst++;
                Array.Clear(typeCounter, 0, maxTypes);

                for (int i = 0; i < length; i++) {
                    var elem = elems[i];
                    for (int j = 0; j < types.Length; j++) {
                        if (types[j].IsInstanceOfType(elem)) {
                            typeCounter[j]++;
                            break;
                        }
                    }
                }
                int accCount = 0;
                for (int j = 0; j < types.Length; j++) {
                    var endRange = accCount + typeCounter[j];
                    Ranges.Add(types[j], new Tuple<int, int>(accCount - 1, endRange));
                    typeCounter[j] = accCount;
                    lowTypeCounter[j] = accCount;
                    accCount = endRange;
                }

                this.Elems = elems;
                int curTypeBucket = 0;
                // In-array sorting
                for (int i = 0; i < length;) {
                    var elem = elems[i];
                    for (int j = 0; j < types.Length; j++) {
                        if (types[j].IsInstanceOfType(elem)) {
                            int target = typeCounter[j]++;
                            if (i == target) {
                                i++;
                                while (curTypeBucket < types.Length - 1 && i >= lowTypeCounter[curTypeBucket + 1]) {
                                    i = typeCounter[++curTypeBucket];
                                }
                            } else {
                                swap(i, target);
                            }
                            break;
                        }
                    }

                }

            }
        }
        public void swap(int i1, int i2)
        {
            object temp = this.Elems[i1];
            this.Elems[i1] = this.Elems[i2];
            this.Elems[i2] = temp;
        }

        public IEnumerator<T> GetEnumerator()
        {
            if (this.Ranges == null) {
                return new TypeSpanSimpleEnumerator<T>(this);
            } else {
                return new TypeSpanEnumerator<T>(this);
            }
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }

    /// <summary>
    /// A small inheritance hierachy for testing TypeSpan
    /// </summary>
    public class Animal<T>
    {
        public int Id;
    }
    public interface Wolf_Lion<T> { }
    public class Carnivore<T> : Animal<T>
    {
        public override string ToString()
        {
            return "Carn " + Id;
        }
    }
    public class Wolf<T> : Carnivore<T>, Wolf_Lion<T>
    {
        public override string ToString()
        {
            return "Wolf " + Id;
        }
    }
    public class Lion<T> : Carnivore<T>, Wolf_Lion<T>
    {
        public override string ToString()
        {
            return "Lion " + Id;
        }
    }
    public class Herbiousvore<T> : Animal<T>
    {
        public override string ToString()
        {
            return "Herb " + Id;
        }
    }
}
