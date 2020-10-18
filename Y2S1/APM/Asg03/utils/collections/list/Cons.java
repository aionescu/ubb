package utils.collections.list;

import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Supplier;

public final class Cons<T> extends List<T> {
  private final T _head;
  private final List<T> _tail;

  public Cons(T head, List<T> tail) {
    _head = head;
    _tail = tail;
  }

  @Override
  public <E> E match(Supplier<E> nil, BiFunction<T, List<T>, E> cons) {
    return cons.apply(_head, _tail);
  }

  @Override
  public void matchDo(Runnable nil, BiConsumer<T, List<T>> cons) {
    cons.accept(_head, _tail);
  }
}
