package ro.arc.packageManager.service;

import ro.arc.packageManager.domain.Maintainer;

import java.util.List;

public interface MaintainerService {
    void addMaintainer(Maintainer maintainer);

    void updateMaintainer(Maintainer maintainer);

    void deleteMaintainer(Long ID);

    List<Maintainer> getFilteredMaintainers(String type, String input);

    List<Maintainer> getAllMaintainers();
}
