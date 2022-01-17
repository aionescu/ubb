package ro.arc.packageManager.web.converter;

import org.springframework.stereotype.Component;
import ro.arc.packageManager.core.domain.Address;
import ro.arc.packageManager.web.dto.MaintainerDto;
import ro.arc.packageManager.core.domain.Maintainer;

@Component
public class MaintainerConverter extends BaseConverter<Maintainer, MaintainerDto> {
    @Override
    public Maintainer convertDtoToModel(MaintainerDto dto) {
        var maintainer = new Maintainer();
        maintainer.setId(dto.getId());
        maintainer.setUserName(dto.getUserName());
        maintainer.setFullName(dto.getFullName());
        maintainer.setEmail(dto.getEmail());
        maintainer.setAddress(dto.getAddress());
        return maintainer;
    }

    @Override
    public MaintainerDto convertModelToDto(Maintainer maintainer) {
        MaintainerDto dto = new MaintainerDto(maintainer.getUserName(), maintainer.getFullName(), maintainer.getEmail(), maintainer.getAddress());
        dto.setId(maintainer.getId());
        return dto;
    }
}
