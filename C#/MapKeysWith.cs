using System;
using System.Collections.Generic;
using System.Linq;

namespace MapKeysWith
{
    public static class DictionaryE
    {
        public static Dictionary<K2, V> MapKeys<K1, V, K2>(this Dictionary<K1, V> d, Func<K1, K2> f)
        {
            return d.ToDictionary(kvp => f(kvp.Key), kvp => kvp.Value);
        }

        public static Dictionary<K2, V> MapKeysWith<K1, V, K2>(this Dictionary<K1, V> d, Func<V, V, V> c, Func<K1, K2> f)
        {
            return
                d.GroupBy(kvp => f(kvp.Key), kvp => kvp.Value)
                 .ToDictionary(g => g.Key, g => g.Aggregate(c));
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine(
                new Dictionary<int, string>() {
                    { 1, "a" }, { 2, "b" },
                    { 3, "c" }, { 4, "d" }
                }
                    .MapKeysWith((a, b) => a + b, k => k % 2)
                    .Aggregate("", (a, kvp) => a + "(" + kvp.Key + ", " + kvp.Value + "); ")
            ); // => (1, ac); (0, bd); 
            Console.ReadLine();
        }
    }
}
