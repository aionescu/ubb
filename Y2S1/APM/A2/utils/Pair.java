package utils;

public final class Pair<A, B> {
  public final A fst;
  public final B snd;

  public static <A, B> Pair<A, B> of(A fst, B snd) {
    return new Pair<>(fst, snd);
  }

  public Pair(A fst, B snd) {
    this.fst = fst;
    this.snd = snd;
  }
}
