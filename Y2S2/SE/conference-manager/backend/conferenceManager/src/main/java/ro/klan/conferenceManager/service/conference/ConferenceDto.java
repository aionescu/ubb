package ro.klan.conferenceManager.service.conference;

import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class ConferenceDto {
  private Long id;
  private String name;
  private Date startDate;
  private Date endDate;
}
