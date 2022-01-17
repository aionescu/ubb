package ro.arc.packageManager.web.converter;

import org.springframework.stereotype.Component;
import ro.arc.packageManager.web.dto.PackageDto;
import ro.arc.packageManager.core.domain.Package;

@Component
public class PackageConverter extends BaseConverter<Package, PackageDto> {
    @Override
    public Package convertDtoToModel(PackageDto dto) {
        var pkg = new Package();
        pkg.setId(dto.getId());
        pkg.setName(dto.getName());
        pkg.setDescription(dto.getDescription());
        pkg.setSourceRepo(dto.getSourceRepo());
        pkg.setLicense(dto.getLicense());
        return pkg;
    }

    @Override
    public PackageDto convertModelToDto(Package pkg) {
        PackageDto dto = new PackageDto(pkg.getName(),pkg.getDescription(),pkg.getSourceRepo(),pkg.getLicense());
        dto.setId(pkg.getId());
        return dto;
    }
}
