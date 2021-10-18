using System;
using System.Diagnostics;
using System.IO;
using System.Linq;

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

void computeElement(int[,] a, int[,] b, int row, int col, int[,] c) {
  var e = 0;

  for (var i = 0; i < width(a); ++i)
    e += a[row, i] * b[i, col];

  c[row, col] = e;
}

int[,] multiplySingleThreaded(int[,] a, int[,] b) {
  Debug.Assert(width(a) == width(b), "Incompatible matrix sizes.");

  var c = new int[height(a), width(b)];

  for (var i = 0; i < height(a); ++i)
    for (var j = 0; j < width(b); ++j)
      computeElement(a, b, i, j, c);

  return c;
}

void multiplyRows(int[,] a, int[,] b, int rowFrom, int rowTo, int leftoverFrom, int leftoverTo, int[,] c) {
  var i = rowFrom;

  for (; i < rowTo; ++i)
    for (var j = 0; j < width(c); ++j)
      computeElement(a, b, i, j, c);

  for (var j = leftoverFrom; j < leftoverTo; ++j)
    computeElement(a, b, i, j, c);
}

void multiplyCols(int[,] a, int[,] b, int colFrom, int colTo, int leftoverFrom, int leftoverTo, int[,] c) {
  var j = colFrom;

  for (; j < colTo; ++j)
    for (var i = 0; i < height(c); ++i)
      computeElement(a, b, i, j, c);

  for (var i = leftoverFrom; i < leftoverTo; ++i)
    computeElement(a, b, i, j, c);
}

void multiplyKth(int[,] a, int[,] b, int from, int k, int[,] c) {
  var w = width(c);

  for (var i = from; i < c.Length; i += k)
    computeElement(a, b, i / w, i % w, c);
}

var m = readMatrix("Data/A.txt");
printMatrix(m);
