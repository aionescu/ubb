from exn import AlreadyGradedError, InvalidGradeError

class Grade:
  def __init__(self, lab, problem, value):
    self.__lab = lab
    self.__problem = problem
    self.__value = value

  @property
  def lab(self):
    return self.__lab

  @property
  def problem(self):
    return self.__problem

  @property
  def value(self):
    return self.__value

  def set_value(self, value):
    if self.value != 0:
      raise AlreadyGradedError()

    if value <= 1 or value > 10:
      raise InvalidGradeError()

    self.__value = value

  def __eq__(self, rhs):
    return (self.lab == rhs.lab
      and self.problem == rhs.problem
      and self.value == rhs.value)
      
class Student:
  def __init__(self, id, name, group):
    self.__id = id
    self.__name = name
    self.__group = group

  @property
  def id(self):
    return self.__id

  @property
  def name(self):
    return self.__name

  @property
  def group(self):
    return self.__group

  def __eq__(self, rhs):
    return (self.id == rhs.id
      and self.name == rhs.name
      and self.group == rhs.group)

  def __str__(self):
    return f"{self.id},{self.name},{self.group}"

  @staticmethod
  def read(line):
    comp = line.split(",")
    id = int(comp[0])
    name = comp[1]
    group = int(comp[2])
    return Student(id, name, group)