package edu.asg02.repo;

import java.util.List;
import edu.asg02.models.Attendee;

public interface Repository {
  void add(Attendee attendee);
  void remove(String attendeeName);
  List<Attendee> attendees();
}
