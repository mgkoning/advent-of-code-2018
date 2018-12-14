using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace AdventOfCode2018.Day14 {
  class Day14 : IDay {
    public string Banner => "Day 14: Chocolate Charts";
    
    public Task Solve() {
      var target = "652601";
      System.Console.WriteLine("Part 1:");
      System.Console.WriteLine(Part1(int.Parse(target)));
      System.Console.WriteLine("Part 2:");
      System.Console.WriteLine(Part2(target));
      return Task.CompletedTask;
    }

    private static IEnumerable<int> IterateRecipes() {
      var recipeList = new List<int>() { 3, 7 };
      int addToList(int value) {
        recipeList.Add(value);
        return value;
      }

      var elves = new List<int>() { 0, 1 };

      foreach(var i in recipeList) {
        yield return i;
      }

      while (true) {
        var combined = (from e in elves select recipeList[e]).Sum();
        if (combined > 9) {
          yield return addToList(1);
        }
        yield return addToList(combined % 10);
        elves = Enumerable.ToList(from e in elves select (e + recipeList[e] + 1) % recipeList.Count);
      }
    }

    internal static string Part1(int target) =>
      string.Join("", IterateRecipes().Skip(target).Take(10));

    internal static int Part2(string target) {
      var targetSequence = Enumerable.ToList(from c in target select c-'0');
      var lastRecipes = new Queue<int>();
      var read = 0;
      foreach(var recipe in IterateRecipes()) {
        read++;
        lastRecipes.Enqueue(recipe);
        if(lastRecipes.Count < targetSequence.Count) { continue; }
        if(targetSequence.SequenceEqual(lastRecipes)) { return read - targetSequence.Count; }
        lastRecipes.Dequeue();
      }
      return -1; // won't get here
    }
  }
}