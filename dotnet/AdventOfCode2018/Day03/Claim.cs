using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace AdventOfCode2018.Day03 {
  class Claim {
    public static Claim Parse(string line) {
      //#1317 @ 382,340: 28x29
      var values = line.Split(
        new[] { ':', ' ', 'x', '@', '#', ',' },
        StringSplitOptions.RemoveEmptyEntries
      );
      var intValues = (from val in values.Skip(1) select int.Parse(val)).ToArray();
      return new Claim(values[0], intValues[0], intValues[1], intValues[2], intValues[3]);
    }

    public Claim(string id, int left, int top, int width, int height) =>
      (Id, Left, Top, Width, Height) = (id, left, top, width, height);

    public string Id { get; }
    public int Left { get; }
    public int Top { get; }
    public int Width { get; }
    public int Height { get; }

    internal IEnumerable<(int, int)> Coordinates() =>
      from x in Enumerable.Range(Left, Width)
      from y in Enumerable.Range(Top, Height)
      select (x, y);

  }
}
