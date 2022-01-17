package ro.klan.conferenceManager.service.mapper.conference;

import ro.klan.conferenceManager.entity.Conference;
import ro.klan.conferenceManager.service.conference.ConferenceDto;

public interface ConferenceMapper {
  ConferenceDto toService(Conference entity);
  Conference toEntity(ConferenceDto dto);
}
