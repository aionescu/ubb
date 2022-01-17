package ro.klan.conferenceManager.service.user;

import ro.klan.conferenceManager.entity.User;
import ro.klan.conferenceManager.repository.UserRepository;
import ro.klan.conferenceManager.service.mapper.user.UserMapper;
import lombok.AllArgsConstructor;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class UserServiceImpl implements UserService {
    private final UserRepository userRepository;
    private final UserMapper userMapper;

    @Override
    public UserDto saveUser(final UserDto user) {
        User userEntity = userMapper.toEntity(user);
        PasswordEncoder passwordEncoder = new BCryptPasswordEncoder();
        userEntity.setPassword(passwordEncoder.encode(userEntity.getPassword()));
        return userMapper.toService(userRepository.save(userEntity));
    }

}
