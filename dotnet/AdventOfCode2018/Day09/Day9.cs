using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace AdventOfCode2018.Day09 {
  public class Day9 : IDay {
    bool _showProgress = false;
    int _actualPlayers = 412;
    int _actualTarget = 71646;

    public string Banner => "Day 9: Marble Mania";

    public Task Solve() {
      System.Console.WriteLine("Part 1:");
      System.Console.WriteLine(HighScore(_actualPlayers, _actualTarget));
      System.Console.WriteLine("Part 2:");
      System.Console.WriteLine(Util.Time(() => HighScore(_actualPlayers, 100 * _actualTarget)));
      return Task.CompletedTask;
    }

    public long HighScore(int players, int target) {
      var scores = MarbleScores(target);
      var scoresByPlayer = from s in scores group s by s.position % players;
      var sumScores =
        from s in scoresByPlayer
        let sum = s.Sum(m => m.score)
        orderby sum descending
        select sum;
      return sumScores.First();
    }

    internal IEnumerable<(int position, long score)> MarbleScores(int target) {
      var currentNode = Node<int>.Singleton(0);
      var head = currentNode;
      var nodeScores = new List<(int position, long score)>();
      for (var step = 1; step <= target; step++) {
        if (_showProgress) { PrintList(step, head); }
        if (step % 23 == 0) {
          var toRemove = Move(currentNode, -7);
          nodeScores.Add((step, step + toRemove.Value));
          currentNode = toRemove.Next;
          toRemove.Delete();
          continue;
        }
        currentNode = currentNode.Next.InsertAfter(step);
      }
      return nodeScores;
    }

    Node<int> Move(Node<int> current, int steps) {
      var back = steps < 0;
      steps = Math.Abs(steps);
      Node<int> result = current;
      for( ; steps > 0; steps--) {
        result = back ? result.Previous : result.Next;
      }
      return result;
    }

    void PrintList(int step, Node<int> head) {
      System.Console.WriteLine($"Before step {step}:");
      var current = head;
      do {
        System.Console.Write($"{current.Value}, ");
        current = current.Next;
      } while (current != head);
      System.Console.WriteLine();
    }

  }
}