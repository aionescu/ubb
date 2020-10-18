package utils.collections.list;

import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Supplier;

public final class Nil<T> extends List<T> {
  public Nil() { }

  @Override
  public <E> E match(Supplier<E> nil, BiFunction<T, List<T>, E> cons) {
    return nil.get();
  }

  @Override
  public void matchDo(Runnable nil, BiConsumer<T, List<T>> cons) {
    nil.run();
  }
}
