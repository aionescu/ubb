package ro.arc.packageManager.client.ui;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.function.Function;

import ro.arc.packageManager.common.domain.exceptions.AppException;

// `Future` has no `map` method? FOR REAL!?
public final class MappedFuture<T, U> {
  private final Future<T> future;
  private final Function<T, U> f;

  public MappedFuture(Future<T> future, Function<T, U> f) {
    this.future = future;
    this.f = f;
  }

  public boolean isCompleted() {
    return future.isDone();
  }

  public U force() {
    try {
      return f.apply(future.get());
    } catch (InterruptedException | ExecutionException e) {
      throw new AppException("Exception during future completion: " + e.getMessage());
    }
  }
}
