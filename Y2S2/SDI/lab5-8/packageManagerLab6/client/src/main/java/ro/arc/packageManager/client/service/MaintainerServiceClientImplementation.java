package ro.arc.packageManager.client.service;

import org.springframework.beans.factory.annotation.Autowired;
import ro.arc.packageManager.common.domain.Maintainer;
import ro.arc.packageManager.common.service.MaintainerService;

import java.util.List;
import java.util.stream.Stream;

public class MaintainerServiceClientImplementation implements MaintainerService {
    @Autowired
    private MaintainerService maintainerService;

    @Override
    public void addMaintainer(Maintainer maintainer) {
        this.maintainerService.addMaintainer(maintainer);
    }

    @Override
    public void updateMaintainer(Maintainer maintainer) {
        this.maintainerService.updateMaintainer(maintainer);
    }

    @Override
    public void deleteMaintainer(Long ID) {
        this.maintainerService.deleteMaintainer(ID);
    }

    @Override
    public List<Maintainer> getFilteredMaintainers(String type, String input) {
        return this.maintainerService.getFilteredMaintainers(type, input);
    }

    @Override
    public List<Maintainer> getAllMaintainers() {
        return this.maintainerService.getAllMaintainers();
    }
}
