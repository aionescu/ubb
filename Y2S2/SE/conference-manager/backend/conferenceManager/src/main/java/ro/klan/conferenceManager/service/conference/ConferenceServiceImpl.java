package ro.klan.conferenceManager.service.conference;

import java.util.List;
import java.util.stream.Collectors;

import org.springframework.stereotype.Service;

import lombok.AllArgsConstructor;
import ro.klan.conferenceManager.repository.ConferenceRepository;
import ro.klan.conferenceManager.service.mapper.conference.ConferenceMapper;

@Service
@AllArgsConstructor
public class ConferenceServiceImpl implements ConferenceService {
  private ConferenceRepository conferenceRepository;
  private ConferenceMapper conferenceMapper;

  @Override
  public List<ConferenceDto> getAll() {
    return conferenceRepository.findAll().stream().map(conferenceMapper::toService).collect(Collectors.toList());
  }
}
