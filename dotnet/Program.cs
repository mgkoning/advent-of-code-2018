using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace AdventOfCode2018 {
  class Program {
    static IDictionary<int, Type> _runners = (
      from type in typeof(Program).Assembly.GetTypes()
      where typeof(IDay).IsAssignableFrom(type)
      let match = Regex.Match(type.Name, @"(\d+)$")
      where match.Success
      let dayNum = int.Parse(match.Groups[0].Value)
      select (dayNum, type)
    ).ToDictionary(t => t.dayNum, t => t.type);

    static async Task<int> Main(string[] args) {
      var day = 0 < args.Length ? args[0] : Ask("Provide day number:");
      if (int.TryParse(day, out var dayNum) && _runners.ContainsKey(dayNum)) {
        var dayRunner = (IDay)Activator.CreateInstance(_runners[dayNum]);
        ShowBanner(dayRunner.Banner);
        await dayRunner.Solve();
        return 0;
      } else {
        System.Console.WriteLine("Invalid day specified.");
        return 1;
      }
    }

    static void ShowBanner(string banner) {
      const int width = 80;
      var padding = (width - banner.Length) / 2;
      var bannerEdge = new string('*', width);
      Console.WriteLine(bannerEdge);
      Console.WriteLine($"*{banner.PadLeft(padding - 1 + banner.Length).PadRight(width - 2)}*");
      Console.WriteLine(bannerEdge);
    }

    static string Ask(string question) {
      Console.Write($"{question} ");
      return Console.ReadLine();
    }
  }
}
