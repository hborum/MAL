using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Gen
{
  public class FunctionalDictionary<TKey, TVal> : IDictionary<TKey, TVal>
  {
    Func<TKey, TVal> selector;
    ICollection<TKey> keys;
    ICollection<TVal> vals;
    Func<IEnumerable<TKey>> keys_fun; // Allows for delayed computation of keys. This is required by some initialization.

    public FunctionalDictionary(IEnumerable<TKey> keys, Func<TKey, TVal> proj)
    {
      selector = proj;
      this.keys = keys.ToList();
      vals = keys.Select(selector).ToList();
    }

    public FunctionalDictionary(Func<IEnumerable<TKey>> keys, Func<TKey, TVal> proj)
    {
      selector = proj;
      keys_fun = keys;
    }

    public TVal this[TKey key] { get => selector(key); set => throw new MemberAccessException("This Dictionary is readonly, and only posing as a regular Dictionary."); }

    public ICollection<TKey> Keys {
      get {
        if (keys == null)
          keys = this.keys_fun().ToList();
        return keys;
      }
    }

    public ICollection<TVal> Values {
      get {
        return Keys.Select(selector).ToList();
      }
    }

    public int Count => Keys.Count;

    public bool IsReadOnly => true;

    public void Add(TKey key, TVal value)
    {
      throw new MemberAccessException("This Dictionary is readonly, and only posing as a regular Dictionary.");
    }

    public void Add(KeyValuePair<TKey, TVal> item)
    {
      throw new MemberAccessException("This Dictionary is readonly, and only posing as a regular Dictionary.");
    }

    public void Clear()
    {
      throw new MemberAccessException("This Dictionary is readonly, and only posing as a regular Dictionary.");
    }

    public bool Contains(KeyValuePair<TKey, TVal> item)
    {
      return Keys.Contains(item.Key) && selector(item.Key).Equals(item.Value);
    }

    public bool ContainsKey(TKey key)
    {
      return Keys.Contains(key);
    }

    public void CopyTo(KeyValuePair<TKey, TVal>[] array, int arrayIndex)
    {
      foreach (var k in Keys) array[arrayIndex++] = new KeyValuePair<TKey, TVal>(k, selector(k));
    }

    public IEnumerator<KeyValuePair<TKey, TVal>> GetEnumerator()
    {
      return Keys.Select(k => new KeyValuePair<TKey, TVal>(k, selector(k))).GetEnumerator();
    }

    public bool Remove(TKey key)
    {
      throw new MemberAccessException("This Dictionary is readonly, and only posing as a regular Dictionary.");
    }

    public bool Remove(KeyValuePair<TKey, TVal> item)
    {
      throw new MemberAccessException("This Dictionary is readonly, and only posing as a regular Dictionary.");
    }

    public bool TryGetValue(TKey key, out TVal value)
    {
      var res = ContainsKey(key);
      value = res ? selector(key) : default;
      return res;
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
      return GetEnumerator();
    }
  }
}
