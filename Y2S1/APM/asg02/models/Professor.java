package asg02.models;

import asg02.exn.InvalidAttendeeException;

public final class Professor implements Attendee {
  private final String _name, _teachingSubject;

  public Professor(String name, String teachingSubject) {
    if (name == null || name.isEmpty())
      throw new InvalidAttendeeException();

    if (teachingSubject == null || teachingSubject.isEmpty())
      throw new InvalidAttendeeException();

    _name = name;
    _teachingSubject = teachingSubject;
  }

  @Override
  public final String name() {
    return _name;
  }

  public final String teachingSubject() {
    return _teachingSubject;
  }

  @Override
  public boolean hasPresentedWorks() {
    return true;
  }

  @Override
  public String toString() {
    return String.format(
      "Professor { name = %s, teachingSubject = %s }",
      _name, _teachingSubject);
  }
}
