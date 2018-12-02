using System.Collections.Generic;

namespace AdventOfCode2018 {
  public static class EnumerationExtensions {

    ///<summary>
    ///Iterates <paramref name="this"/> continuously, restarting after reaching the end.
    ///</summary>
    public static IEnumerable<T> Cycle<T>(this IEnumerable<T> @this) {
      while(true) {
        foreach(var item in @this) {
          yield return item;
        }
      }
    }
  }
}
