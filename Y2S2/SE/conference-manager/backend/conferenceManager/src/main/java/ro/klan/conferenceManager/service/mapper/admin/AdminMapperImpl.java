package ro.klan.conferenceManager.service.mapper.admin;

import ro.klan.conferenceManager.entity.Admin;
import ro.klan.conferenceManager.service.admin.AdminDto;
import ro.klan.conferenceManager.service.mapper.conference.ConferenceMapper;

import java.util.stream.Collectors;

import org.springframework.stereotype.Service;

import lombok.AllArgsConstructor;

@Service
@AllArgsConstructor
public
class AdminMapperImpl implements AdminMapper {
  private final ConferenceMapper conferenceMapper;

  @Override
  public AdminDto toService(Admin entity) {
    var conferences = entity.getConferences().stream().map(conferenceMapper::toService).collect(Collectors.toList());
    return new AdminDto(entity.getId(), entity.getName(), entity.getEmail(), conferences);
  }

  @Override
  public Admin toEntity(AdminDto dto) {
    var conferences = dto.getConferences().stream().map(conferenceMapper::toEntity).collect(Collectors.toSet());
    return new Admin(dto.getId(), dto.getName(), dto.getEmail(), conferences);
  }
}
