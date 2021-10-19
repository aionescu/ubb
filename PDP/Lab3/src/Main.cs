using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading;
using Microsoft.Toolkit.HighPerformance;

#pragma warning disable 8321

int height(int[,] m) {
  return m.GetLength(0);
}

int width(int[,] m) {
  return m.GetLength(1);
}

void printMatrix(int[,] m) {
  for (var i = 0; i < height(m); ++i) {
    Console.Write("[ ");

    for (var j = 0; j < width(m); ++j)
      Console.Write(m[i, j] + " ");

    Console.Write("]\n");
  }

  Console.WriteLine();
}

int[,] readMatrix(string path) {
  var flat =
    File.ReadAllText(path)
    .Split(new[] { ' ', '\t', '\n' }, StringSplitOptions.RemoveEmptyEntries)
    .Select(int.Parse)
    .ToArray();

  var height = flat[0];
  var width = flat[1];

  var m = new int[height, width];
  Buffer.BlockCopy(flat, sizeof(int) * 2, m, 0, sizeof(int) * (flat.Length - 2));

  return m;
}

var random = new Random();

int[,] randomMatrix(int height, int width, int min, int max) {
  var m = new int[height, width];

  for (var i = 0; i < height; ++i)
    for (var j = 0; j < width; ++j)
      m[i, j] = random.Next(min, max);

  return m;
}

int[,] allocMultiply(int[,] a, int[,] b) {
  Debug.Assert(width(a) == width(b), "Incompatible matrix sizes.");
  return new int[height(a), width(b)];
}

void clear(int[,] m) {
  m.AsSpan2D().Fill(0);
}

int computeElement(int[,] a, int[,] b, int row, int col) {
  var e = 0;

  for (var i = 0; i < width(a); ++i)
    e += a[row, i] * b[i, col];

  return e;
}

void checkConsistency(int[,] a, int[,] b, int[,] c) {
  for (var i = 0; i < height(c); ++i)
    for (var j = 0; j < width(c); ++j)
      if (c[i, j] != computeElement(a, b, i, j))
        goto Incorrect;

  Console.WriteLine("👍");
  return;

Incorrect:
  Console.WriteLine("Incorrect result 👎");
}

void multiplyRows(int[,] a, int[,] b, int id, int rowCount, int leftover, int[,] c) {
  var from = id * rowCount + Math.Min(id, leftover);
  var to = from + rowCount + (id < leftover ? 1 : 0);

  for (var i = from; i < to; ++i)
    for (var j = 0; j < width(c); ++j)
      c[i, j] = computeElement(a, b, i, j);
}

void multiplyCols(int[,] a, int[,] b, int id, int colCount, int leftover, int[,] c) {
  var from = id * colCount + Math.Min(id, leftover);
  var to = from + colCount + (id < leftover ? 1 : 0);

  for (var j = from; j < to; ++j)
    for (var i = 0; i < height(c); ++i)
      c[i, j] = computeElement(a, b, i, j);
}

void multiplyKth(int[,] a, int[,] b, int from, int k, int[,] c) {
  var w = width(c);

  for (var i = from; i < c.Length; i += k) {
    var row = i / w;
    var col = i % w;
    c[row, col] = computeElement(a, b, row, col);
  }
}

void runThreads(int threadCount, Action<int> f) {
  List<Thread> threads = new();

  foreach (var i in Enumerable.Range(0, threadCount))
    threads.Add(new Thread(() => f(i)));

  threads.ForEach(t => t.Start());
  threads.ForEach(t => t.Join());
}

void runThreadPool(int jobCount, Action<int> f) {
  using var countdown = new CountdownEvent(jobCount);

  foreach (var i in Enumerable.Range(0, jobCount))
    ThreadPool.QueueUserWorkItem(o => { f(i); countdown.Signal(); });

  countdown.Wait();
}

void runByRows(Action<int, Action<int>> f, int[,] a, int[,] b, int[,] c, int jobCount) {
  var rowsPerJob = height(c) / jobCount;
  var leftover = height(c) % jobCount;

  timed(() => f(jobCount, i => multiplyRows(a, b, i, rowsPerJob, leftover, c)));
}

void runByCols(Action<int, Action<int>> f, int[,] a, int[,] b, int[,] c, int jobCount) {
  var colsPerJob = width(c) / jobCount;
  var leftover = width(c) % jobCount;

  timed(() => f(jobCount, i => multiplyCols(a, b, i, colsPerJob, leftover, c)));
}

void runByKth(Action<int, Action<int>> f, int[,] a, int[,] b, int[,] c, int jobCount) {
  timed(() => f(jobCount, i => multiplyKth(a, b, i, jobCount, c)));
}

void timed(Action f) {
  Console.WriteLine("Starting stopwatch");
  var sw = Stopwatch.StartNew();
  f();
  Console.WriteLine(sw.Elapsed);
}

void main() {
  var a = randomMatrix(500, 500, 10, 20);
  var b = randomMatrix(500, 500, 10, 20);
  var c = allocMultiply(a, b);

  runByCols(runThreads, a, b, c, 16);
  runByCols(runThreads, a, b, c, 8);
  runByCols(runThreads, a, b, c, 4);
  runByCols(runThreads, a, b, c, 2);
}

main();
