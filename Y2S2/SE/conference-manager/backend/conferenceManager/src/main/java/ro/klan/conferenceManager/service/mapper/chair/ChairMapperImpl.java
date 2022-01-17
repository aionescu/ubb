package ro.klan.conferenceManager.service.mapper.chair;


import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import ro.klan.conferenceManager.entity.Chair;
import ro.klan.conferenceManager.service.chair.ChairDto;

@Service
@AllArgsConstructor
public class ChairMapperImpl implements ChairMapper {
    @Override
    public ChairDto toService(Chair entity) {
        return new ChairDto(entity.getId(), entity.getName(),entity.getEmail());
    }

    @Override
    public Chair toEntity(ChairDto dto) {
        return new Chair(dto.getId(),dto.getName(), dto.getEmail());
    }
}
