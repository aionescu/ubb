package ro.klan.conferenceManager.service.chair;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class ChairDto {
    public Long id;
    private String name;
    private String email;
}

