using System;
using System.Collections;
using System.Collections.Generic;

namespace Gen
{
    class TypeSpanSimpleEnumerator<T> : IEnumerator<T> where T : class
  {
        readonly TypeSpan<T> span;
        int curIndex = -1;
        public TypeSpanSimpleEnumerator(TypeSpan<T> span)
        {
            this.span = span;
        }

        public T Current {
            get { return (T)span.Elems[curIndex]; }
        }

        object IEnumerator.Current => this.Current;

        public void Dispose()
        {
        }

        public bool MoveNext()
        {
            return ++curIndex < span.Length;
        }

        public void Reset()
        {
            curIndex = -1;
        }
    }
}
