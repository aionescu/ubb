package edu.asg02.models;

import edu.asg02.models.exn.InvalidAttendeeException;

public class IndustrySpecialist implements Attendee {
  private final String _name, _company;
  private boolean _hasPresentedWorks;

  public IndustrySpecialist(String name, String company, boolean hasPresentedWorks) {
    if (name == null || name.isEmpty())
      throw new InvalidAttendeeException();

    if (company == null || company.isEmpty())
      throw new InvalidAttendeeException();

    _name = name;
    _company = company;
    _hasPresentedWorks = hasPresentedWorks;
  }

  public final String name() {
    return _name;
  }

  public final String company() {
    return _company;
  }

  public boolean hasPresentedWorks() {
    return _hasPresentedWorks;
  }
}
