package edu.asg02.models;

import edu.asg02.models.exn.InvalidAttendeeException;

public class Student implements Attendee {
  private final String _name, _major;

  public Student(String name, String major) {
    if (name == null || name.isEmpty())
      throw new InvalidAttendeeException();

    if (major == null || major.isEmpty())
      throw new InvalidAttendeeException();

    _name = name;
    _major = major;
  }

  public final String name() {
    return _name;
  }

  public final String major() {
    return _major;
  }

  public boolean hasPresentedWorks() {
    return false;
  }
}
