package ro.klan.conferenceManager.service.author;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Data;
import ro.klan.conferenceManager.service.paper.PaperDto;

@Data
@AllArgsConstructor
public class AuthorDto {
  public Long id;
  private String name;
  private String affiliation;
  private String email;
  private List<PaperDto> papers;
}
