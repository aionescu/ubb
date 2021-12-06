using System;
using System.Numerics;
using System.Threading;

namespace Lab5;

interface INumber<A> where A: struct, INumber<A> {
  static abstract A operator +(A a, A b);
  static abstract A operator -(A a);
  static abstract A operator -(A a, A b);
  static abstract A operator *(A a, A b);

  static abstract bool operator ==(A a, A b);
  static abstract bool operator !=(A a, A b);

  static abstract A FromInt32(int a);
}

interface IAtomicOps<A> where A: IAtomicOps<A> {
  static abstract A AtomicAdd(ref A a, A b);
}

struct Int: INumber<Int>, IAtomicOps<Int> {
  private int _value;

  public int Value => _value;

  public Int(int value) => _value = value;

  public static Int operator +(Int a, Int b) => new(a._value + b._value);
  public static Int operator -(Int a) => new(-a._value);
  public static Int operator -(Int a, Int b) => new(a._value - b._value);
  public static Int operator *(Int a, Int b) => new(a._value * b._value);

  public static bool operator ==(Int a, Int b) => a._value == b._value;
  public static bool operator !=(Int a, Int b) => a._value != b._value;

  public static Int FromInt32(int a) => new(a);

  public static Int AtomicAdd(ref Int a, Int b) => new(Interlocked.Add(ref a._value, b._value));

  public override bool Equals(object? o) => o is Int i && this == i;
  public override int GetHashCode() => _value.GetHashCode();
  public override string ToString() => _value.ToString();
}

readonly record struct Double(double Value): INumber<Double> {
  public static Double operator +(Double a, Double b) => new(a.Value + b.Value);
  public static Double operator -(Double a) => new(-a.Value);
  public static Double operator -(Double a, Double b) => new(a.Value - b.Value);
  public static Double operator *(Double a, Double b) => new(a.Value * b.Value);

  public static Double FromInt32(int a) => new(a);

  public override string ToString() => Value.ToString();
}

readonly record struct BigInt(BigInteger Value): INumber<BigInt> {
  public static BigInt operator +(BigInt a, BigInt b) => new(a.Value + b.Value);
  public static BigInt operator -(BigInt a) => new(-a.Value);
  public static BigInt operator -(BigInt a, BigInt b) => new(a.Value - b.Value);
  public static BigInt operator *(BigInt a, BigInt b) => new(a.Value * b.Value);

  public static BigInt FromInt32(int a) => new(a);

  public override string ToString() => Value.ToString();
}
