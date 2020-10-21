package utils;

import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;

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

  public A fst_() {
    return fst;
  }

  public B snd_() {
    return snd;
  }

  public static <A, B, C> Function<Pair<A, B>, C> match(BiFunction<A, B, C> f) {
    return p -> f.apply(p.fst, p.snd);
  }

  public static <A, B> Consumer<Pair<A, B>> matchDo(BiConsumer<A, B> f) {
    return p -> f.accept(p.fst, p.snd);
  }

  @Override
  public String toString() {
    return String.format("(%s, %s)", fst, snd);
  }
}
