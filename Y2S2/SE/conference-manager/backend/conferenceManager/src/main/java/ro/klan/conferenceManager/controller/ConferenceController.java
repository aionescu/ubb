package ro.klan.conferenceManager.controller;

import ro.klan.conferenceManager.service.conference.ConferenceDto;
import ro.klan.conferenceManager.service.conference.ConferenceService;

import lombok.AllArgsConstructor;

import java.util.List;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/conference")
@AllArgsConstructor
public class ConferenceController {
  private final ConferenceService conferenceService;

  @PreAuthorize("isAuthenticated()")
  @GetMapping("/all")
  public ResponseEntity<List<ConferenceDto>> all() {
    var all = conferenceService.getAll();
    return new ResponseEntity<>(all, HttpStatus.OK);
  }
}
