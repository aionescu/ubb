from exn import AlreadyGradedError, DuplicateIdError, InvalidGradeError, InvalidOpError, MissingItemError
from controller import Controller

def read_int(s):
  val = input(s + ": ")

  try:
    val = int(val)
  except ValueError:
    print(s + " must be an integer.")
    return None
  
  return val

class Ui:
  def __init__(self):
    self.__controller = Controller("students.txt")
    self.__cmds = {
      "exit": exit,
      "undo": self.undo,
      "redo": self.redo,
      "add": self.add_student,
      "remove": self.remove_student,
      "assign": self.assign_lab,
      "assign-group": self.assign_group,
      "grade": self.grade_student,
      "best-students": self.best_students,
      "failing-students": self.failing_students
    }

  def handle_cmd(self, cmd):
    if cmd not in self.__cmds:
      print("Command not recognized.")
      return

    self.__cmds[cmd]()

  def undo(self):
    try:
      self.__controller.undo()
    except InvalidOpError:
      print("Nothing to undo.")

  def redo(self):
    try:
      self.__controller.redo()
    except InvalidOpError:
      print("Nothing to redo.")

  def add_student(self):
    id = read_int("ID")
    if id is None:
      return

    name = input("Name: ")
    
    group = read_int("Group")
    if group is None:
      return

    try:
      self.__controller.add_student(id, name, group)
    except DuplicateIdError:
      print("ID already exists.")
      return

  def remove_student(self):
    id = read_int("ID")
    if id is None:
      return

    try:
      self.__controller.remove_student(id)
    except MissingItemError:
      print("Student does not exist.")

  def assign_lab(self):
    student_id = read_int("Student ID")
    if student_id is None:
      return

    lab = read_int("Laboratory")
    if lab is None:
      return
    
    problem = read_int("Problem")
    if problem is None:
      return

    try:
      self.__controller.assign_lab(student_id, lab, problem)
    except MissingItemError:
      print("Student does not exist.")
    except DuplicateIdError:
      print("Laboratory already assigned for that student.")

  def assign_group(self):
    group = read_int("Group")
    if group is None:
      return

    lab = read_int("Lab")
    if lab is None:
      return

    self.__controller.assign_group(group, lab)

  def grade_student(self):
    id = read_int("Student ID")
    if id is None:
      return
    
    lab = read_int("Laboratory")
    if lab is None:
      return

    grade = read_int("Grade")
    if grade is None:
      return

    try:
      self.__controller.grade_student(id, lab, grade)
    except MissingItemError:
      print("Student or Lab does not exist.")
    except AlreadyGradedError:
      print("Student already graded.")
    except InvalidGradeError:
      print("Grade must be between 1 and 10.")

  def best_students(self):
    print(self.__controller.best_students())

  def failing_students(self):
    print(self.__controller.failing_students())