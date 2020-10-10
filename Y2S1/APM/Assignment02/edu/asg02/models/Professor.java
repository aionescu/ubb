package edu.asg02.models;

import edu.asg02.models.exn.InvalidAttendeeException;

public class Professor implements Attendee {
  private final String _name, _teachingSubject;

  public Professor(String name, String teachingSubject) {
    if (name == null || name.isEmpty())
      throw new InvalidAttendeeException();

    if (teachingSubject == null || teachingSubject.isEmpty())
      throw new InvalidAttendeeException();

    _name = name;
    _teachingSubject = teachingSubject;
  }

  public final String name() {
    return _name;
  }

  public final String teachingSubject() {
    return _teachingSubject;
  }

  public boolean hasPresentedWorks() {
    return true;
  }
}
