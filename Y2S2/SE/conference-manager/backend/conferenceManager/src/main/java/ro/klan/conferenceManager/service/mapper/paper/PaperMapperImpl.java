package ro.klan.conferenceManager.service.mapper.paper;

import ro.klan.conferenceManager.entity.Paper;
import ro.klan.conferenceManager.service.paper.PaperDto;
import org.springframework.stereotype.Service;

@Service
public
class PaperMapperImpl implements PaperMapper {
    @Override
    public PaperDto toService(Paper entity) {
      return new PaperDto(entity.getId(), entity.getName(), entity.getTopic());
    }

    @Override
    public Paper toEntity(PaperDto dto) {
      return new Paper(dto.getId(), dto.getName(), dto.getTopic(), null);
    }
}
