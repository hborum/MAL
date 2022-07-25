using System;
using System.Collections;
using System.Collections.Generic;

namespace Gen
{
    class TypeSpanEnumerator<T> : IEnumerator<T> where T : class
  {
        readonly TypeSpan<T> span;
        readonly IEnumerator<KeyValuePair<Type, Tuple<int, int>>> typeEnum;
        int curIndex = -1;
        public TypeSpanEnumerator(TypeSpan<T> span)
        {
            this.span = span;
            typeEnum = span.Ranges.GetEnumerator();
            if (!typeEnum.MoveNext()) {
                typeEnum.Dispose();
                typeEnum = null;
            } else {
                curIndex = typeEnum.Current.Value.Item1;
            }
        }

        public T Current {
            get { return (T)span.Elems[curIndex]; }
        }

        object IEnumerator.Current => this.Current;

        public void Dispose()
        {
            typeEnum.Dispose();
        }

        public bool MoveNext()
        {
            if (typeEnum == null) {
                return false;
            }
            curIndex++;
            if (curIndex < typeEnum.Current.Value.Item2) {
                return true;
            } else {
                if (typeEnum.MoveNext()) {

                    curIndex = typeEnum.Current.Value.Item1;
                    curIndex++;
                    if (curIndex >= typeEnum.Current.Value.Item2) {
                        return MoveNext();
                    } else {
                        return true;
                    }
                } else {
                    return false;
                }
            }
        }

        public void Reset()
        {
            if (typeEnum != null) {
                typeEnum.Reset();
                curIndex = typeEnum.Current.Value.Item1;
            }
        }
    }
}
