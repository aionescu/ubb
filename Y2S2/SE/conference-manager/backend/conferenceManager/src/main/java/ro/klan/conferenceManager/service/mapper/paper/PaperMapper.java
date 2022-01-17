package ro.klan.conferenceManager.service.mapper.paper;

import ro.klan.conferenceManager.entity.Paper;
import ro.klan.conferenceManager.service.paper.PaperDto;

public interface PaperMapper {
  PaperDto toService(Paper entity);
  Paper toEntity(PaperDto dto);
}
