package ro.arc.packageManager.web.converter;

import org.springframework.stereotype.Component;
import ro.arc.packageManager.web.dto.PackageMaintainerDto;
import ro.arc.packageManager.core.domain.PackageMaintainer;

@Component
public class PackageMaintainerConverter extends BaseConverter<PackageMaintainer, PackageMaintainerDto> {
  @Override
  public PackageMaintainer convertDtoToModel(PackageMaintainerDto dto) {
    var pkg = new PackageMaintainer();
    pkg.setId(dto.getId());
    pkg.setMaintainerID(pkg.getMaintainerID());
    pkg.setPackageID(pkg.getPackageID());
    return pkg;
  }

  @Override
  public PackageMaintainerDto convertModelToDto(PackageMaintainer pkg) {
    PackageMaintainerDto dto = new PackageMaintainerDto(pkg.getMaintainerID(), pkg.getPackageID());
    dto.setId(pkg.getId());
    return dto;
  }
}
