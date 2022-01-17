package ro.klan.conferenceManager.service.mapper.conference;

import org.springframework.stereotype.Service;

import ro.klan.conferenceManager.entity.Conference;
import ro.klan.conferenceManager.service.conference.ConferenceDto;

@Service
public class ConferenceMapperImpl implements ConferenceMapper {

  @Override
  public ConferenceDto toService(Conference entity) {
    return new ConferenceDto(entity.getId(), entity.getName(), entity.getStartDate(), entity.getEndDate());
  }

  @Override
  public Conference toEntity(ConferenceDto dto) {
    return new Conference(dto.getId(), dto.getName(), dto.getStartDate(), dto.getEndDate(), null);
  }
}
