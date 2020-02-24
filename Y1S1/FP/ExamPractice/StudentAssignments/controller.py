from exn import InvalidOpError, MissingItemError
from domain import Student, Grade
from repo import StudentRepo, GradeRepo

class Controller:
  def __init__(self, filename):
    self.__students = StudentRepo(filename)
    self.__grades = GradeRepo()

    self.__undone = []
    self.__done = []

  def __backup(self, d):
    students = self.__students.to_dict()
    grades = self.__grades.to_dict()

    d.append((students, grades))

  def __reload(self, d):
    (students, grades) = d.pop()
    self.__students.of_dict(students)
    self.__grades.of_dict(grades)
    
  def undo(self):
    if not self.__done:
      raise InvalidOpError()

    self.__backup(self.__undone)
    self.__reload(self.__done)

  def redo(self):
    if not self.__undone:
      raise InvalidOpError()

    self.__backup(self.__done)
    self.__reload(self.__undone)

  def add_student(self, id, name, group):
    self.__backup(self.__done)

    student = Student(id, name, group)
    self.__students.add(student)

    self.__undone.clear()

  def remove_student(self, id):
    self.__backup(self.__done)

    self.__students.remove(id)

    self.__undone.clear()

  def assign_lab(self, student_id, lab, problem):
    self.__backup(self.__done)

    if not self.__students.exists(student_id):
      raise MissingItemError()

    if not self.__grades.exists(student_id):
      self.__grades.add(student_id, {})

    grade = Grade(lab, problem, 0)
    self.__grades.get(student_id)[lab] = grade

    self.__undone.clear()

  def assign_group(self, group, lab):
    self.__backup(self.__done)

    students = filter(lambda s: s.group == group, self.__students.all())
    current_problem = 1

    for student in students:
      if self.__grades.exists(student.id) and lab in self.__grades.get(student.id):
        continue

      self.assign_lab(student.id, lab, current_problem)
      current_problem = 1 if current_problem == 20 else current_problem + 1

    self.__undone.clear()

  def grade_student(self, id, lab, grade):
    self.__backup(self.__done)

    student = self.__students.get(id)
    gr = self.__grades.get(id)

    if lab not in gr:
      raise MissingItemError()

    gr[lab].set_value(grade)

    self.__undone.clear()

  def averages(self):
    def get_average(student):
      try:
        grades = list(map(lambda g: g.value, (filter(lambda g: g.value != 0, self.__grades.get(student.id).values()))))
      except MissingItemError:
        return 0

      return sum(grades) / len(grades)

    return map(lambda s: (s, get_average(s)), self.__students.all())

  def best_students(self):
    avg = self.averages()
    return '\n'.join(map(str, map(lambda x: x[0], sorted(avg, key = lambda x: x[1], reverse = True))))

  def failing_students(self):
    avg = self.averages()
    return '\n'.join(map(str, map(lambda x: x[0], filter(lambda x: x[1] < 5 and x[1] != 0, avg))))