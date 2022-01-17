package ro.arc.packageManager.client.service;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.stream.Stream;

import ro.arc.packageManager.client.networking.TCPClient;
import ro.arc.packageManager.common.domain.BaseEntity;
import ro.arc.packageManager.common.domain.exceptions.AppException;
import ro.arc.packageManager.common.networking.Message;

public abstract class ServiceClient {
  private final ExecutorService executorService;

  public ServiceClient(ExecutorService executorService) {
    this.executorService = executorService;
  }

  protected final Future<Void> pingPong(String name, Object... args) {
    return executorService.submit(() -> {
      var msg = new Message(name, args);
      var response = TCPClient.sendAndReceive(msg);

      if (response.getHeader().equals("success"))
        return null;
      else
        throw new AppException("Invalid response.");
    });
  }

  protected final <T extends BaseEntity<Long>> Future<Stream<T>> pingPongEntities(Class<T> cla$$, String name, Object... args) {
    return executorService.submit(() -> {
      var msg = new Message(name, args);
      var response = TCPClient.sendAndReceive(msg);

      if (response.getHeader().equals("success"))
        return response.getBody().stream().map(s -> BaseEntity.ofCSVRow(s, cla$$));
      else
        throw new AppException("Invalid response.");
    });
  }
}
