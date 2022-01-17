package ro.klan.conferenceManager.service.mapper.user;

import ro.klan.conferenceManager.entity.User;
import ro.klan.conferenceManager.service.user.UserDto;
import org.springframework.stereotype.Service;

@Service
public
class UserMapperImpl implements UserMapper {
    @Override
    public UserDto toService(User entity) {
        return new UserDto(entity.getId(), entity.getEmail(), entity.getPassword(), entity.getRole());
    }

    @Override
    public User toEntity(UserDto dto) {
        return new User(dto.getId(), dto.getEmail(), dto.getPassword(), dto.getRole());
    }
}
