using System.Linq;
using NUnit.Framework;

namespace AdventOfCode2018.Tests {
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
  }
}
