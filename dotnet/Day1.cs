using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace AdventOfCode2018 {
  public class Day1: IDay {

    public string Banner => "Day 1: Chronal Calibration";

    public async Task Solve() {
      var frequencyChanges = await Util.ParseLines("day01.txt", int.Parse);
      Console.WriteLine("Part 1:");
      Console.WriteLine(frequencyChanges.Sum());

      Console.WriteLine("Part 2:");
      var seen = new HashSet<int>() { 0 };
      var frequency = 0;
      foreach(var change in frequencyChanges.Cycle()) {
        frequency += change;
        if(!seen.Add(frequency)) {
          Console.WriteLine(frequency);
          break;
        }
      }
    }
  }
}
