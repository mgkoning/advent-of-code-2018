using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace AdventOfCode2018 {
  public class Day2 : IDay {
    public string Banner => "Day 2: Inventory Management System";

    public (int x, int y) Add((int x, int y) one, (int x, int y) other) =>
      (one.x + other.x, one.y + other.y);

    public static IEnumerable<(T, T)> Pairs<T>(IEnumerable<T> list) =>
      !list.Any() ?
        new (T, T)[0] :
        (from item in list.Skip(1) select (list.First(), item))
          .Concat(Pairs(list.Skip(1)));

    public static IEnumerable<IEnumerable<T>> Combinations<T>(IEnumerable<T> list, int number) {
      if(number < 1) {
        return new []{ new T[0] };
      }
      if(!list.Any()) {
        return new T[][]{ };
      }
      var first = list.First();
      var rest = list.Skip(1);
      return Combinations(rest, number-1).Select(comb => new[] {first}.Concat(comb))
        .Concat(Combinations(rest, number));
    }


    public async Task Solve() {
      bool hasCount(IEnumerable<IGrouping<char, char>> groupings, int target) =>
        groupings.Any(l => l.Count() == target);

      var boxIDs = await Util.ReadInputFileAsLines("day02.txt");

      Console.WriteLine("Part 1:");
      var counts = from boxID in boxIDs
                   let letterGrouping = from c in boxID group c by c
                   select (hasCount(letterGrouping, 2) ? 1 : 0, hasCount(letterGrouping, 3) ? 1 : 0);
      var (twos, threes) = counts.Aggregate(Add);
      Console.WriteLine(twos * threes);

      Console.WriteLine("Part 2:");
      var boxIDPairs = Pairs(boxIDs);
      var (pair, common) = Enumerable.First(
        from p in boxIDPairs
        let c = CommonPart(p.Item1, p.Item2)
        where c.Length == p.Item1.Length - 1
        select (p, c)
      );
      Console.WriteLine(common);

      Console.WriteLine("Part 2 (faster, 'inspired'):");
      var seen = new HashSet<(int, string)>();
      foreach(var possible in boxIDs.SelectMany(boxID => TakeOneOut(boxID))) {
        if(!seen.Add(possible)) {
          System.Console.WriteLine(possible.Item2);
          break;
        }
      }
    }

    IEnumerable<(int, string)> TakeOneOut(string boxID) =>
      boxID.Select((_, index) => (index, boxID.Remove(index, 1)));

    string CommonPart(string one, string other) =>
      new string(one.Where((letter, index) => other[index] == letter).ToArray());
  }
}
