package ro.klan.conferenceManager.service.user;

import lombok.Data;

import java.util.List;

@Data
public class AuthenticatedUserDto {
    private String email;
    private List<String> roles;
}
