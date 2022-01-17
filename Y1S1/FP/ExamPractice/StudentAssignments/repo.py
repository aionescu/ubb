from exn import DuplicateIdError, InvalidFileError, MissingItemError
from domain import Student

class StudentRepo:
  def __init__(self, filename):
    self.__data = {}
    self.__filename = filename

    with open(self.__filename, 'r') as file:
      lines = file.readlines()
      
      try:
        students = map(lambda s: (s.id, s), map(Student.read, lines))
        self.__data = dict(students)
      except ValueError:
        raise InvalidFileError()

  def save_to_file(self):
    with open(self.__filename, 'w') as file:
      for student in self.__data.values():
        file.write(str(student))

  def add(self, student):
    if student.id in self.__data:
      raise DuplicateIdError()

    self.__data[student.id] = student
    self.save_to_file()

  def exists(self, id):
    return id in self.__data

  def get(self, id):
    if not self.exists(id):
      raise MissingItemError()

    return self.__data[id]

  def remove(self, id):
    if not self.exists(id):
      raise MissingItemError()

    del self.__data[id]
    self.save_to_file()

  def all(self):
    return self.__data.values()

  def to_dict(self):
    return dict(self.__data)
    self.save_to_file()

  def of_dict(self, d):
    self.__data = dict(d)
    self.save_to_file()

class GradeRepo:
  def __init__(self):
    self.__data = {}

  def add(self, id, grade):
    if id in self.__data:
      raise DuplicateIdError()

    self.__data[id] = grade

  def exists(self, id):
    return id in self.__data

  def get(self, id):
    if not self.exists(id):
      raise MissingItemError()

    return self.__data[id]

  def remove(self, id):
    if not self.exists(id):
      raise MissingItemError()

    del self.__data[id]

  def all(self):
    return self.__data.values()

  def to_dict(self):
    return dict(self.__data)

  def of_dict(self, d):
    self.__data = dict(d)