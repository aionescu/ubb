from domain import *
from exn import *

# Class that manages multiple instances of a particular entity class
class Repo:
  def __init__(self):
    self.__data = {}
    self.__crr_id = 0

  def next_id(self):
    self.__crr_id += 1
    return self.__crr_id

  def __must_exist(self, id):
    if id not in self.__data:
      raise InexistentItemError()

  # Method that gets the value with the specified id
  # Input: id - The id to lookup
  # Output: The value with the corresponding id
  # Postconditions: -
  # Raises: InexistentItemError if there no value has the specified id
  def get(self, id):
    self.__must_exist(id)
    return self.__data[id]

  # Method that adds the value to the repository
  # Input: val - The value to add
  # Output: -
  # Postconditions: The value exists in the repository
  # Raises: -
  def add(self, val):
    self.__data[val.id()] = val

  # Method that updates a value in the repository
  # Input: val - The new version of the value to update
  # Output: -
  # Postconditions: The value's information is updated in the repository
  # Raises: InexistentItemError if there is no value with the new value's id already in the repo
  def update(self, val):
    self.__must_exist(val.id())
    self.__data[val.id()] = val

  # Method that removes a value from the repository
  # Input: val - The value to remove from the repository
  # Output: -
  # Postconditions: The value does not exist in the repository anymore
  # Raises: InexistentItemError if the value does not exist in the repo
  def remove(self, val):
    self.__must_exist(val.id())
    del self.__data[val.id()]

  def keys(self):
    for key in self.__data.keys():
      yield key

  def values(self):
    for val in self.__data.values():
      yield val

class ClientRepo(Repo):
  def __init__(self):
    Repo.__init__(self)

  def create(self, name):
    return Client(self.next_id(), name)

class MovieRepo(Repo):
  def __init__(self):
    Repo.__init__(self)

  def create(self, title, desc, genre):
    return Movie(self.next_id(), title, desc, genre)
      
class RentalRepo(Repo):
  def __init__(self):
    Repo.__init__(self)

  def create(self, client_id, movie_id, rented_date, due_date):
    return Rental(self.next_id(), client_id, movie_id, rented_date, due_date, None)