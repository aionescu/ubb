package ro.arc.packageManager.client.service;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.stream.Stream;

import ro.arc.packageManager.common.domain.Maintainer;

public final class MaintainerServiceClient extends ServiceClient {
  public MaintainerServiceClient(ExecutorService executorService) {
    super(executorService);
  }

  public Future<Void> addMaintainer(String userName, String fullName, String email) {
    return pingPong("add maintainer", userName, fullName, email);
  }

  public Future<Void> deleteMaintainer(Long id) {
    return pingPong("delete maintainer", id);
  }

  public Future<Void> updateMaintainer(Long id, String userName, String fullName, String email) {
    return pingPong("update maintainer", id, userName, fullName, email);
  }

  public Future<Stream<Maintainer>> showMaintainers() {
    return pingPongEntities(Maintainer.class, "show maintainers");
  }

  public Future<Stream<Maintainer>> showFilteredMaintainers(String field, String searchTerm) {
    return pingPongEntities(Maintainer.class, "show maintainers", field, searchTerm);
  }
}
