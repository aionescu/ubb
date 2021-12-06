namespace Lab5;

using System;
using System.Linq;
using System.Threading.Tasks;

readonly record struct Poly<A>(params A[] Coefficients): INumber<Poly<A>> where A: struct, INumber<A> {
  public int Degree => Coefficients.Length - 1;
  public A this[int i] => Coefficients[i];
  public A this[Index i] => Coefficients[i];
  public Poly<A> this[Range r] => new(Coefficients[r]);

  public static Poly<A> operator +(Poly<A> a, Poly<A> b) {
    if (a.Degree > b.Degree)
      (a, b) = (b, a);

    var c = new A[b.Degree + 1];
    Array.Copy(b.Coefficients, c, b.Degree + 1);

    for (var i = 0; i <= a.Degree; ++i)
      c[i] += a[i];

    return new(c);
  }

  public static Poly<A> operator -(Poly<A> a) => new(a.Coefficients.Select(a => -a).ToArray());

  public static Poly<A> operator -(Poly<A> a, Poly<A> b) {
    var p = (a + -b);
    var c = p.Coefficients;

    var i = c.Length - 1;
    while (i >= 0 && c[i] == A.FromInt32(0))
      --i;

    return i == -1 ? p : new(c.Take(i + 1).ToArray());
  }

  public static Poly<A> operator *(Poly<A> a, Poly<A> b) => MulNaiveSeq(a, b);

  public static Poly<A> operator <<(Poly<A> a, int offset) {
    if (offset < 0)
      throw new ArgumentOutOfRangeException(nameof(offset), "Offset can't be negative.");

    var c = new A[a.Degree + 1 + offset];
    Array.Copy(a.Coefficients, 0, c, offset, a.Degree + 1);

    return new(c);
  }

  public static Poly<A> FromInt32(int a) => new(new[] { A.FromInt32(a) });

  public static Poly<A> Random(int degree, int min = 1, int max = 100) {
    if (degree < 0)
      throw new ArgumentOutOfRangeException(nameof(degree), "Degree can't be negative.");

    var r = new Random();
    return new(Enumerable.Range(0, degree + 1).Select(_ => A.FromInt32(r.Next(min, max + 1))).ToArray());
  }

  public static Poly<A> MulNaiveSeq(Poly<A> a, Poly<A> b) {
    var c = new A[a.Degree + b.Degree + 1];

    for (var i = 0; i <= a.Degree; ++i)
      for (var j = 0; j <= b.Degree; ++j)
        c[i + j] += a[i] * b[j];

    return new(c);
  }

  public static Poly<B> MulNaivePar<B>(Poly<B> a, Poly<B> b) where B: struct, INumber<B>, IAtomicOps<B> {
    var c = new B[a.Degree + b.Degree + 1];

    Parallel.For(0, a.Degree + 1, i => {
      for (var j = 0; j <= b.Degree; ++j)
        B.AtomicAdd(ref c[i + j], a[i] * b[j]);
    });

    return new(c);
  }

  public static Poly<A> KaratsubaSeq(Poly<A> a, Poly<A> b, int threshold = 100) {
    if (a.Degree <= threshold || b.Degree <= threshold)
      return MulNaiveSeq(a, b);

    var mid = (Math.Max(a.Degree, b.Degree) + 1) / 2;

    Poly<A> aLow = a[..mid];
    Poly<A> aHigh = a[mid..];

    Poly<A> bLow = b[..mid];
    Poly<A> bHigh = b[mid..];

    var z0 = KaratsubaPar(aLow, bLow);
    var z1 = KaratsubaPar(aLow + aHigh, bLow + bHigh);
    var z2 = KaratsubaPar(aHigh, bHigh);

    return (z2 << (2 * mid)) + ((z1 - z2 - z0) << mid) + z0;
  }

  public static Poly<A> KaratsubaPar(Poly<A> a, Poly<A> b, int threshold = 100) {
    if (a.Degree <= threshold || b.Degree <= threshold)
      return MulNaiveSeq(a, b);

    var mid = (Math.Max(a.Degree, b.Degree) + 1) / 2;

    Poly<A> aLow = a[..mid];
    Poly<A> aHigh = a[mid..];

    Poly<A> bLow = b[..mid];
    Poly<A> bHigh = b[mid..];

    var t0 = Task<Poly<A>>.Factory.StartNew(() => KaratsubaPar(aLow, bLow));
    var t1 = Task<Poly<A>>.Factory.StartNew(() => KaratsubaPar(aLow + aHigh, bLow + bHigh));
    var t2 = Task<Poly<A>>.Factory.StartNew(() => KaratsubaPar(aHigh, bHigh));

    Task.WaitAll(t0, t1, t2);

    var z0 = t0.Result;
    var z1 = t1.Result;
    var z2 = t2.Result;

    return (z2 << (2 * mid)) + ((z1 - z2 - z0) << mid) + z0;
  }

  public override string ToString() {
    return "[" + string.Join(", ", Coefficients) + "]";
  }
}
