using System.Linq;
using NUnit.Framework;

namespace AdventOfCode2018.Day02.Tests {
  class Day2Tests {
    
    [Test]
    public void PairsTest() {
      var pairs = Day2.Pairs(new[] { 1,2,3,4 });
      Assert.That(pairs.Count(), Is.EqualTo(6));
      Assert.That(
        pairs,
        Is.EqualTo(
          new[] { (1,2), (1,3), (1,4), (2,3), (2,4), (3,4) }
        )
      );
    }

    [Test]
    public void CombinationsTest() {
      var options = new[] { 1,2,3,4 };
      Assert.That(Day2.Combinations(options, 0), Is.EqualTo(new [] { Enumerable.Empty<int>() }));
      Assert.That(Day2.Combinations(new int[0], 1).Count(), Is.EqualTo(0));
      var pairs = Day2.Combinations(options, 2);
      Assert.That(pairs.Count(), Is.EqualTo(6));
      Assert.That(
        pairs,
        Is.EqualTo(new[] {
          new[] { 1, 2 },  new[] { 1, 3 }, new[] { 1, 4 },
          new[] { 2, 3 },  new[] { 2, 4 }, new[] { 3, 4 }
        })
      );
    }
  }
}
