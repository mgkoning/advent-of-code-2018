using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace AdventOfCode2018.Day03 {
  public class Day3 : IDay {
    public string Banner => "Day 3: No Matter How You Slice It";

    public async Task Solve() {
      var claims = await Util.ParseLines("day03.txt", Claim.Parse);
      var claimMap = BuildClaimMap(claims);
      Console.WriteLine("Part 1:");
      Console.WriteLine(claimMap.Count(c => c.Value > 1));

      Console.WriteLine("Part 2:");
      Console.WriteLine(
        claims
          .First(claim => claim.Coordinates().All(coord => claimMap[coord] == 1))
          .Id
      );
    }

    Dictionary<(int, int), int> BuildClaimMap(IEnumerable<Claim> claims) =>
      claims
        .SelectMany(c => c.Coordinates())
        .Aggregate(new Dictionary<(int, int), int>(), AddOrUpdate);

    Dictionary<T, int> AddOrUpdate<T>(Dictionary<T, int> counts, T val) {
      if(!counts.ContainsKey(val)) {
        counts[val] = 0;
      }
      counts[val] += 1;
      return counts;
    }
  }
}
