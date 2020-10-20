package asg02.repo;

import java.util.stream.Stream;

import asg02.models.Attendee;

public interface Repository {
  void add(Attendee attendee);
  void remove(String attendeeName);
  Stream<Attendee> attendees();
}
