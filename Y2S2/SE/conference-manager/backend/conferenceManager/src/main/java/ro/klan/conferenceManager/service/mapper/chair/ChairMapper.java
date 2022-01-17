package ro.klan.conferenceManager.service.mapper.chair;

import ro.klan.conferenceManager.entity.Chair;
import ro.klan.conferenceManager.service.chair.ChairDto;

public interface ChairMapper {
    ChairDto toService(Chair entity);
    Chair toEntity(ChairDto dto);
}
