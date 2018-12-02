using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Threading.Tasks;

namespace AdventOfCode2018 {
  public static class Util {
    static string _basePath = Path.Combine(FindHome(), "dotnet", "inputs");

    static IEnumerable<DirectoryInfo> GetAncestors(FileInfo file) {
      DirectoryInfo directory = file.Directory;
      while(directory != null) {
        yield return directory;
        directory = directory.Parent;
      }
    }

    static string FindHome() =>
      GetAncestors(new FileInfo(typeof(Util).Assembly.Location)).Skip(4).First().FullName;
    
    public static async Task<IEnumerable<string>> ReadInputFileAsLines(string fileName) =>
      await File.ReadAllLinesAsync(Path.Combine(_basePath, fileName));

    public static async Task<IEnumerable<T>> ParseLines<T>(string fileName, Func<string, T> convert) =>
      Enumerable.ToList(
        from line in await ReadInputFileAsLines(fileName)
        select convert(line)
      );
  }
}
