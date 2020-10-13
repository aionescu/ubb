package asg02.controller;

import java.util.stream.Stream;

import asg02.exn.InvalidArgumentException;
import asg02.models.Attendee;
import asg02.models.IndustrySpecialist;
import asg02.models.Professor;
import asg02.models.Student;
import asg02.repo.Repository;

public final class Controller {
  private final Repository _repo;

  public Controller(Repository repository) {
    if (repository == null)
      throw new InvalidArgumentException();

    _repo = repository;
  }

  public void addStudent(String name, String major) {
    var student = new Student(name, major);
    _repo.add(student);
  }

  public void addProfessor(String name, String teachingSubject) {
    var professor = new Professor(name, teachingSubject);
    _repo.add(professor);
  }

  public void addIndustrySpecialist(String name, String company, boolean hasPresentedWorks) {
    var industrySpecialist = new IndustrySpecialist(name, company, hasPresentedWorks);
    _repo.add(industrySpecialist);
  }

  public void remove(String name) {
    _repo.remove(name);
  }

  public Stream<Attendee> attendees() {
    return _repo.attendees();
  }

  public Stream<Attendee> attendeesWithPresentedWorks(boolean value) {
    return
      attendees()
      .filter(a -> a.hasPresentedWorks() == value);
  }
}
