package asg02.models;

import asg02.exn.InvalidAttendeeException;

public final class IndustrySpecialist implements Attendee {
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

  @Override
  public final String name() {
    return _name;
  }

  public final String company() {
    return _company;
  }

  @Override
  public boolean hasPresentedWorks() {
    return _hasPresentedWorks;
  }

  @Override
  public String toString() {
    return String.format(
      "IndustrySpecialist { name = %s, company = %s, hasPresentedWorks = %s }",
      _name, _company, _hasPresentedWorks);
  }
}
