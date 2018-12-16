using System.Threading.Tasks;

namespace AdventOfCode2018 {
  interface IDay {
    string Banner { get; }
    Task Solve();
  }
}
