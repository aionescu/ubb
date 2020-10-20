package asg02.models;

import asg02.exn.InvalidAttendeeException;

public final class Student implements Attendee {
  private final String _name, _major;

  public Student(String name, String major) {
    if (name == null || name.isEmpty())
      throw new InvalidAttendeeException();

    if (major == null || major.isEmpty())
      throw new InvalidAttendeeException();

    _name = name;
    _major = major;
  }

  @Override
  public final String name() {
    return _name;
  }

  public final String major() {
    return _major;
  }

  @Override
  public boolean hasPresentedWorks() {
    return false;
  }

  @Override
  public String toString() {
    return String.format(
      "Student { name = %s, major = %s }",
      _name, _major);
  }
}
