using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace AdventOfCode2018 {
  public class Day7 : IDay {
    private bool _showProgress = false;
    public string Banner => "Day 07: The Sum Of Its Parts";

    public async Task Solve() {
      var instructions = await Util.ParseLines("day07.txt", ParseInstruction);
      var prerequisites = MakePrerequisiteMap(instructions);
      var order = "IJLFUVDACEHGRZPNKQWSBTMXOY";
      System.Console.WriteLine("Part 2:");
      Part2(prerequisites, order, 60, 5);
    }

    internal static Dictionary<char, string> MakePrerequisiteMap(
      IEnumerable<(char prerequisite, char step)> instructions
    ) =>
      Enumerable.ToDictionary(
        from i in instructions group i by i.step,
        g => g.Key,
        g => new string((from s in g select s.prerequisite).ToArray())
      );

    internal void Part2(Dictionary<char, string> prerequisites, string order, int baseTime, int numWorkers) {
      var done = new List<char>();
      var idleWorkers = numWorkers;
      var available = new List<char>(order.Take(1));
      var remaining = order.Skip(1).ToList();
      var working = new List<(char step, int doneAt)>();
      int time = 0;
      for (; done.Count != order.Length; time++) {
        if (_showProgress) {
          System.Console.WriteLine(
            $"Time: {time}, idleWorkers: {idleWorkers}, " +
            $"working: {string.Join(",", working.Select(w => w.ToString()))}, " + 
            $"done: {new string(done.ToArray())}"
          );
        }
        var nowDone = working.Where(c => c.doneAt <= time).ToList();
        nowDone.ForEach(c => working.Remove(c));
        idleWorkers += nowDone.Count;
        done.AddRange(from s in nowDone select s.step);
        var newlyAvailable = (from c in remaining where !prerequisites.ContainsKey(c) || prerequisites[c].All(r => done.Contains(r)) select c).ToList();
        newlyAvailable.ForEach(n => {
          available.Add(n);
          remaining.Remove(n);
        });
        available.Sort();
        if (available.Count == 0) { continue; }
        if (idleWorkers < 1) { continue; }
        while(idleWorkers > 0 && available.Count > 0) {
          var next = available[0];
          available.RemoveAt(0);
          working.Add((step: next, doneAt: time + TimeNeeded(baseTime, next)));
          idleWorkers--;
        }
      }
      Console.WriteLine($"Done at: {time - 1}");
    }

    private int TimeNeeded(int baseTime, char next) => baseTime + 1 + (next - 'A');

    (char prerequisite, char step) ParseInstruction(string line) {
      var parts = line.Split(' ');
      // Step C must be finished before step Q can begin.
      return (parts[1][0], parts[7][0]);
    }
  }
}