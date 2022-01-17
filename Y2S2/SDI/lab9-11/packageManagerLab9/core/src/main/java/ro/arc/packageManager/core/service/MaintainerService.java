package ro.arc.packageManager.core.service;

import ro.arc.packageManager.core.domain.Maintainer;

import java.util.List;

public interface MaintainerService {
    Maintainer addMaintainer(Maintainer maintainer);

    Maintainer updateMaintainer(Maintainer maintainer);

    void deleteMaintainer(Long ID);

    List<Maintainer> getFilteredMaintainers(String type, String input);

    List<Maintainer> getAllMaintainers();
}
