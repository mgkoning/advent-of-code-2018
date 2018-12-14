using System.Linq;
using NUnit.Framework;

namespace AdventOfCode2018.Day14.UnitTests {

  [TestFixture]
  public class Day14Tests {

    [TestCase(9, ExpectedResult = "5158916779")]
    [TestCase(5, ExpectedResult = "0124515891")]
    [TestCase(18, ExpectedResult = "9251071085")]
    [TestCase(2018, ExpectedResult = "5941429882")]
    public string TestInputPart1(int target) =>
      Day14.Part1(target);

    [TestCase("51589", ExpectedResult = 9)]
    [TestCase("01245", ExpectedResult = 5)]
    [TestCase("92510", ExpectedResult = 18)]
    [TestCase("59414", ExpectedResult = 2018)]
    public int TestInputPart2(string target) =>
      Day14.Part2(target);
  }
}