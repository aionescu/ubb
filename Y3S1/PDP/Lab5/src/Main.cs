using System;
using System.Diagnostics;
using System.Linq;
using System.Runtime.CompilerServices;
using Lab5;

static A timed<A>(Func<A> f, [CallerArgumentExpression("f")] string? name = null) {
  var sw = Stopwatch.StartNew();
  var r = f();

  var elapsed = sw.Elapsed;
  Console.WriteLine($"{name ?? "Action"} took {elapsed}");
  return r;
}

static void testInt() {
  var a = Poly<Int>.Random(100000, 1, 1000);
  var b = Poly<Int>.Random(100000, 1, 1000);

  var c1 = timed(() => Poly<Int>.MulNaiveSeq(a, b), "Naive Seq");
  var c2 = timed(() => Poly<Int>.MulNaivePar(a, b), "Naive Par");
  var c3 = timed(() => Poly<Int>.KaratsubaSeq(a, b), "Karatsuba Seq");
  var c4 = timed(() => Poly<Int>.KaratsubaPar(a, b), "Karatsuba Par");

  var ok =
    c1.Coefficients.SequenceEqual(c2.Coefficients)
    && c1.Coefficients.SequenceEqual(c3.Coefficients)
    && c1.Coefficients.SequenceEqual(c4.Coefficients);

  if (!ok)
    Console.WriteLine("Mismatch");
}

static void testBigInt() {
  var a = Poly<BigInt>.Random(10000, int.MaxValue - 100, int.MaxValue - 1);
  var b = Poly<BigInt>.Random(10000, int.MaxValue - 100, int.MaxValue - 1);

  var c1 = timed(() => Poly<BigInt>.MulNaiveSeq(a, b), "Naive Seq");
  var c2 = timed(() => Poly<BigInt>.KaratsubaSeq(a, b), "Karatsuba Seq");
  var c3 = timed(() => Poly<BigInt>.KaratsubaPar(a, b), "Karatsuba Par");

  var ok =
    c1.Coefficients.SequenceEqual(c2.Coefficients)
    && c1.Coefficients.SequenceEqual(c3.Coefficients);

  if (!ok)
    Console.WriteLine("Mismatch");
}

if (args.Length > 0 && args[0].StartsWith("big"))
  testBigInt();
else
  testInt();
