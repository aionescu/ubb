package ro.klan.conferenceManager.service.admin;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Data;
import ro.klan.conferenceManager.service.conference.ConferenceDto;

@Data
@AllArgsConstructor
public class AdminDto {
  public Long id;
  private String name;
  private String email;
  private List<ConferenceDto> conferences;
}
