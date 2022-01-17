package ro.arc.packageManager.web.converter;

import org.springframework.stereotype.Component;
import ro.arc.packageManager.web.dto.PackageVersionDto;
import ro.arc.packageManager.core.domain.PackageVersion;

@Component
public class PackageVersionConverter extends BaseConverter<PackageVersion, PackageVersionDto> {
    @Override
    public PackageVersion convertDtoToModel(PackageVersionDto dto) {
        var pkg = new PackageVersion();
        pkg.setId(dto.getId());
        pkg.setPackageID(pkg.getPackageID());
        pkg.setVersionNumber(pkg.getVersionNumber());
        return pkg;
    }

    @Override
    public PackageVersionDto convertModelToDto(PackageVersion pkg) {
        PackageVersionDto dto = new PackageVersionDto(pkg.getPackageID(),pkg.getVersionNumber());
        dto.setId(pkg.getId());
        return dto;
    }
}
