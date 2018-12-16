using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Threading;
using SixLabors.ImageSharp;
using SixLabors.ImageSharp.PixelFormats;

namespace Viz15 {
  class Program {
    static void Main(string[] args) {
      var fileLines = File.ReadAllLines(@"..\..\day-15-beverage-bandits\viz.txt");
      //List<MemoryStream> frames = new List<MemoryStream>();
      for (var imageBlock = 0; imageBlock*33 < fileLines.Length; imageBlock ++) {
        var build = new StringBuilder();
        using (var image = new Image<Rgba32>(32 * _pixelSize, 32 * _pixelSize)) {
          for (var imageLine = 0; imageLine < 32; imageLine++) {
            for (var imageComponent = 0; imageComponent < 32; imageComponent++) {
              PutPixels(image, imageLine, imageComponent, fileLines[imageBlock * 33 + imageLine][imageComponent]);
            }
          }
          var imageStream = new MemoryStream();
          image.Save($"viz15-{imageBlock:0000}.png");
          //frames.Add(imageStream);
        }
      }
      

      //File.WriteAllBytes("viz.png", BuildApng(frames));
    }

    private static byte[] BuildApng(List<MemoryStream> frames) {
      var apngStream = new List<byte>();
      /* acTL */
      apngStream.AddRange(BitConverter.GetBytes((uint)frames.Count));
      apngStream.AddRange(BitConverter.GetBytes((uint)0));

      return apngStream.ToArray();
    }

    static Dictionary<char, Rgba32> _colorMap = new Dictionary<char, Rgba32>() {
      { '+', Rgba32.DarkSlateGray },
      { ' ', Rgba32.LightSlateGray },
      { 'G', Rgba32.ForestGreen },
      { 'E', Rgba32.Firebrick }
    };

    static int _pixelSize = 15;

    private static void PutPixels(Image<Rgba32> image, int line, int column, char it) {
      for(var y = 0; y < _pixelSize; y++) {
        for(var x = 0; x < _pixelSize; x++) {
          image[line * _pixelSize + y, column * _pixelSize + x] = _colorMap[it];
        }
      }
    }
  }
}
