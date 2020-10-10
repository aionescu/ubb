package edu.asg02.controller;

import java.util.List;
import java.util.stream.Collectors;

import edu.asg02.models.Attendee;
import edu.asg02.models.IndustrySpecialist;
import edu.asg02.models.Professor;
import edu.asg02.models.Student;
import edu.asg02.repo.Repository;

public class Controller {
  private final Repository _repo;

  public Controller(Repository repository) {
    if (repository == null)
      throw new IllegalArgumentException();

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

  public List<Attendee> attendees() {
    return _repo.attendees();
  }

  public List<Attendee> attendeesWithPresentedWorks(boolean value) {
    return
      attendees()
      .stream()
      .filter(a -> a.hasPresentedWorks() == value)
      .collect(Collectors.toList());
  }
}
