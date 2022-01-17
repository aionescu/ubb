package ro.klan.conferenceManager.service.paper;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class PaperDto {
  private Long id;
  private String name;
  private String topic;
}
