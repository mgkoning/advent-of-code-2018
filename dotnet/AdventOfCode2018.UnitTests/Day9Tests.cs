using NUnit.Framework;

namespace AdventOfCode2018.Day09.UnitTests {
  [TestFixture]
  public class Day8Tests {

    [Test]
    public void MarbleScoresTest() {
      var marbleScores = new Day9().MarbleScores(100);
      Assert.That(marbleScores, Is.EquivalentTo(new[] {(23, 32), (46, 63), (69, 80), (92, 107) }));
    }

    [Test]
    public void Part1Test() {
      var day8 = new Day9();
      Assert.That(day8.HighScore(9, 25), Is.EqualTo(32));
      Assert.That(day8.HighScore(10, 1618), Is.EqualTo(8317));
      Assert.That(day8.HighScore(13, 7999), Is.EqualTo(146373));
      Assert.That(day8.HighScore(17, 1104), Is.EqualTo(2764));
      Assert.That(day8.HighScore(21, 6111), Is.EqualTo(54718));
      Assert.That(day8.HighScore(30, 5807), Is.EqualTo(37305));
    }
  }
}