package utils.uparsec;

import java.util.function.Function;

@FunctionalInterface
public interface TriFunction<A, B, C, D> {
  D apply(A a, B b, C c);

  public default Function<A, Function<B, Function<C, D>>> curried() {
    return a -> b -> c -> this.apply(a, b, c);
  }
}
