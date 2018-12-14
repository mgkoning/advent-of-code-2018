using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace AdventOfCode2018.Day14 {
  class Day14 : IDay {
    public string Banner => "Day 14: Chocolate Charts";
    
    public Task Solve() {
      var target = 652601;
      System.Console.WriteLine("Part 1:");
      System.Console.WriteLine(Part1(target));
      System.Console.WriteLine("Part 2:");
      System.Console.WriteLine(Part2(target.ToString()));
      return Task.CompletedTask;
    }

    private static List<int> IterateRecipes(Func<List<int>, bool> done) {
      var recipeList = new List<int>() { 3, 7 };
      var elves = new List<int>() { 0, 1 };
      while (!done(recipeList)) {
        var combined = (from e in elves select recipeList[e]).Sum();
        if (combined > 9) {
          recipeList.Add(1);
        }
        recipeList.Add(combined % 10);
        elves = Enumerable.ToList(from e in elves select (e + recipeList[e] + 1) % recipeList.Count);
      }
      return recipeList;
    }

    internal static string Part1(int target) {
      var minimumListSize = target + 10;
      var recipeList = IterateRecipes(list => list.Count >= minimumListSize);
      return string.Join("", recipeList.GetRange(target, 10));
    }


    internal static int Part2(string target) {
      var targetSequence = Enumerable.ToList(from c in target select c-'0');
      var recipeList = IterateRecipes(list => Contains(list, targetSequence));
      int index = recipeList.Count - targetSequence.Count;
      if (recipeList[index] == targetSequence[0]) {
        return index;
      }
      return index - 1;
    }

    private static bool Contains(List<int> list, List<int> targetSequence) {
      if (list.Count < targetSequence.Count) { return false; }
      var range = list.GetRange(list.Count - targetSequence.Count, targetSequence.Count);
      if (range.SequenceEqual(targetSequence)) { return true; }
      if (list.Count < targetSequence.Count + 1) { return false; }
      range = list.GetRange(list.Count - targetSequence.Count - 1, targetSequence.Count);
      return range.SequenceEqual(targetSequence);
    }
  }
}