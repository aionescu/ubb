package asg02.repo;

import java.util.Arrays;
import java.util.stream.Stream;

import asg02.models.Attendee;
import asg02.exn.*;

public final class InMemoryRepository implements Repository {
  private int _attendeeCount;
  private final Attendee[] _attendees;

  public InMemoryRepository(int maximumAttendees) {
    _attendeeCount = 0;
    _attendees = new Attendee[maximumAttendees];
  }

  public void add(Attendee attendee) {
    if (attendee == null)
      throw new IllegalArgumentException();

    if (_attendeeCount == _attendees.length)
      throw new RepositoryFullException();

    if (attendees().anyMatch(a -> a.name().equals(attendee.name())))
      throw new DuplicateAttendeeException();

    _attendees[_attendeeCount++] = attendee;
  }

  public void remove(String name) {
    if (name == null || name.isEmpty())
      throw new IllegalArgumentException();

    var i = 0;

    for (; i < _attendeeCount; ++i)
      if (_attendees[i].name().equals(name))
        break;

    if (i == _attendeeCount)
      throw new InexistentAttendeeException();

    for (var j = i + 1; j < _attendeeCount; ++i, ++j)
      _attendees[i] = _attendees[j];

    _attendees[--_attendeeCount] = null;
  }

  public Stream<Attendee> attendees() {
    return Arrays.stream(_attendees, 0, _attendeeCount);
  }
}
