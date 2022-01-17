package ro.klan.conferenceManager.controller;

import ro.klan.conferenceManager.config.CustomAuthenticationProvider;
import ro.klan.conferenceManager.service.user.UserDto;
import ro.klan.conferenceManager.service.user.UserService;

import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/user")
@AllArgsConstructor
public class UserController {

  private final CustomAuthenticationProvider customAuthProvider;
  private final UserService userService;

  @PreAuthorize("isAuthenticated()")
  @GetMapping("/hello")
  public String callApp() {
    return "Hello World";
  }

  @PostMapping("/register")
  public ResponseEntity<UserDto> register(@RequestBody UserDto user) {
    UserDto savedUser = userService.saveUser(user);
    return new ResponseEntity<>(savedUser, HttpStatus.CREATED);
  }

  @PostMapping("/login")
  public ResponseEntity<?> login(@RequestBody UserDto user) {
    try {
      var token = new UsernamePasswordAuthenticationToken(user.getEmail(), user.getPassword());
      var loggedInUser = customAuthProvider.doAuthenticate(token);

      return new ResponseEntity<>(loggedInUser, HttpStatus.OK);
    } catch (BadCredentialsException e) {
      return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
    }
  }
}
