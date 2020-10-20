package utils.collections.list;

import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Stream;

import utils.Pair;

public abstract class List<T> {
  public abstract <E> E match(Supplier<E> nil, BiFunction<T, List<T>, E> cons);
  public abstract void matchDo(Runnable nil, BiConsumer<T, List<T>> cons);

  public static <T> List<T> nil() {
    return new Nil<>();
  }

  public static <T> List<T> cons(T head, List<T> tail) {
    return new Cons<>(head, tail);
  }

  public static <T> List<T> singleton(T value) {
    return cons(value, nil());
  }

  public static <T> List<T> ofStream(Stream<T> stream) {
    return stream.reduce(List.<T>nil(), (l, a) -> cons(a, l), List::append).reverse();
  }

  public final List<T> append(List<T> b) {
    return match(() -> b, (h, t) -> List.cons(h, t.append(b)));
  }

  public final <U> List<U> map(Function<T, U> f) {
    return match(() -> nil(), (h, t) -> List.cons(f.apply(h), t.map(f)));
  }

  public final void iter(Consumer<T> f) {
    matchDo(() -> { }, (h, t) -> { f.accept(h); t.iter(f); });
  }

  public final <S> S foldl(BiFunction<S, T, S> f, S s) {
    return match(() -> s, (h, t) -> t.foldl(f, f.apply(s, h)));
  }

  public final Optional<Pair<T, List<T>>> uncons() {
    return match(Optional::empty, (h, t) -> Optional.of(Pair.of(h, t)));
  }

  public final List<T> reverse() {
    return match(() -> this, (h, t) -> t.reverse().append(List.cons(h, List.nil())));
  }

  public final boolean empty() {
    return match(() -> true, (h, t) -> false);
  }

  @Override
  public final String toString() {
    return match(
      () -> "[]",
      (h, t) -> "[" + h + t.foldl((s, a) -> s + ", " + a, "") + "]");
  }
}
