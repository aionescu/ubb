package ro.arc.packageManager.common.service;

import ro.arc.packageManager.common.domain.Maintainer;
import ro.arc.packageManager.common.domain.validators.ValidatorException;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Future;
import java.util.stream.Stream;

public interface MaintainerService {

    void addMaintainer(Maintainer maintainer);

    void updateMaintainer(Maintainer maintainer);

    void deleteMaintainer(Long ID);

    List<Maintainer> getFilteredMaintainers(String type, String input);

    List<Maintainer> getAllMaintainers();
}
