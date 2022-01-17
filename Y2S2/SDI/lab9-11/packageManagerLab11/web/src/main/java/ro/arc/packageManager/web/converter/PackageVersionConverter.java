package ro.arc.packageManager.web.converter;

import org.springframework.stereotype.Component;
import ro.arc.packageManager.core.domain.Package;
import ro.arc.packageManager.web.dto.PackageVersionDto;
import ro.arc.packageManager.core.domain.PackageVersion;

@Component
public class PackageVersionConverter extends BaseConverter<PackageVersion, PackageVersionDto> {
    @Override
    public PackageVersion convertDtoToModel(PackageVersionDto dto) {
        var pkg = new Package();
        pkg.setId(dto.getPackageID());
        var packageVersion = new PackageVersion();
        packageVersion.setId(dto.getId());
        packageVersion.setVersionNumber(dto.getVersionNumber());
        packageVersion.setStartingDate(dto.getStartingDate());
        packageVersion.setAPackage(pkg);
        return packageVersion;
    }

    @Override
    public PackageVersionDto convertModelToDto(PackageVersion packageVersion) {
        PackageVersionDto dto = PackageVersionDto.builder()
                .packageID(packageVersion.getAPackage().getId())
                .versionNumber(packageVersion.getVersionNumber())
                .startingDate(packageVersion.getStartingDate())
                .build();
        dto.setId(packageVersion.getId());
        return dto;
    }
}
