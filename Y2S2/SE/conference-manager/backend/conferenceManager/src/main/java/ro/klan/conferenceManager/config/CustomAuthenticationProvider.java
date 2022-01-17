package ro.klan.conferenceManager.config;

import ro.klan.conferenceManager.repository.UserRepository;
import ro.klan.conferenceManager.service.user.AuthenticatedUserDto;

import lombok.AllArgsConstructor;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
@AllArgsConstructor
public class CustomAuthenticationProvider implements AuthenticationProvider {

  private final UserRepository userRepository;

  public AuthenticatedUserDto doAuthenticate(Authentication authentication) {
    authenticate(authentication);
    // user authentication is done
    // get the logged in user from securityContextHolder
    // map the suer to a DTO that frontend can use to know username
    // and roles of logged in user to know what data to show to the user
    var loggedInUser = new AuthenticatedUserDto();

    loggedInUser.setEmail(SecurityContextHolder.getContext().getAuthentication().getName());

    loggedInUser.setRoles(
      SecurityContextHolder.getContext()
      .getAuthentication().getAuthorities().stream()
      .map(Object::toString).collect(Collectors.toList()));

    return loggedInUser;
  }

  @Override
  public Authentication authenticate(Authentication authentication) throws AuthenticationException {
    var email = authentication.getName();
    var password = authentication.getCredentials().toString();

    var user = userRepository.findByEmail(email);
    var encoder = new BCryptPasswordEncoder();

    if (!user.isPresent() || !encoder.matches(password, user.get().getPassword()))
      throw new BadCredentialsException("Invalid credentials");

    var roles = List.of(
      new SimpleGrantedAuthority("User"),
      new SimpleGrantedAuthority(user.get().getRole()));

    var auth = new UsernamePasswordAuthenticationToken(email, password, roles);
    SecurityContextHolder.getContext().setAuthentication(auth);

    return auth;
  }

  @Override
  public boolean supports(Class<?> authentication) {
    return authentication.equals(UsernamePasswordAuthenticationToken.class);
  }
}
