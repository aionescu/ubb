package ro.klan.conferenceManager.service.mapper.user;

import ro.klan.conferenceManager.entity.User;
import ro.klan.conferenceManager.service.user.UserDto;

public interface UserMapper {
    UserDto toService(User entity);
    User toEntity(UserDto dto);
}
