using NUnit.Framework;

namespace AdventOfCode2018.Day07.UnitTests {
  [TestFixture]
  public class Day7Test {



    [Test]
    public void TestPart2() {
      new Day7().Part2(
        Day7.MakePrerequisiteMap(new[] { ('C', 'A'), ('C', 'F'), ('A', 'B'), ('A', 'D'), ('B', 'E'), ('D', 'E'), ('F', 'E') }),
        "CABDFE",
        0,
        2
      );
    }
  }
}