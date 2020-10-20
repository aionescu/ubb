package utils;

import java.util.function.BiConsumer;
import java.util.function.BiFunction;

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

  public A fstF() {
    return fst;
  }

  public B sndF() {
    return snd;
  }

  public <C> C match(BiFunction<A, B, C> f) {
    return f.apply(fst, snd);
  }

  public void matchDo(BiConsumer<A, B> f) {
    f.accept(fst, snd);
  }

  @Override
  public String toString() {
    return String.format("(%s, %s)", fst, snd);
  }
}
