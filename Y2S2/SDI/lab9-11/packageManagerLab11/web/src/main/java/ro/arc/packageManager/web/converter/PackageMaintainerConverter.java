package ro.arc.packageManager.web.converter;

import org.springframework.stereotype.Component;
import ro.arc.packageManager.core.domain.Maintainer;
import ro.arc.packageManager.core.domain.Package;
import ro.arc.packageManager.web.dto.PackageMaintainerDto;
import ro.arc.packageManager.core.domain.PackageMaintainer;

import java.util.Collection;
import java.util.Set;
import java.util.stream.Collectors;

@Component
public class PackageMaintainerConverter  {
  public PackageMaintainer convertDtoToModel(PackageMaintainerDto dto) {
    var packageMaintainer = new PackageMaintainer();
    var aPackage = new Package();
    aPackage.setId(dto.getPackageID());
    aPackage.setName(dto.getPackageName());
    var maintainer = new Maintainer();
    maintainer.setId(dto.getMaintainerID());
    packageMaintainer.setApackage(aPackage);
    packageMaintainer.setMaintainer(maintainer);
    packageMaintainer.setImportance(dto.getImportance());
    return packageMaintainer;
  }

  public PackageMaintainerDto convertModelToDto(PackageMaintainer pkg) {
    PackageMaintainerDto dto = PackageMaintainerDto.builder()
            .maintainerID(pkg.getMaintainer().getId())
            .packageID(pkg.getApackage().getId())
            .packageName(pkg.getApackage().getName())
            .importance(pkg.getImportance())
            .build();
    return dto;
  }

  public Set<PackageMaintainerDto> convertModelsToDtos(Collection<PackageMaintainer> models) {
    return models.stream()
            .map(this::convertModelToDto)
            .collect(Collectors.toSet());
  }
}
