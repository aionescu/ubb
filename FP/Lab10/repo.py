from domain import *
from exn import *

# Class that manages multiple instances of a particular entity class
class Repo:
  def __init__(self, storage):
    self.__storage = storage
    self.__crr_id = 0

  def next_id(self):
    self.__crr_id += 1
    return self.__crr_id

  def must_exist(self, id):
    self.__storage.must_exist(id)

  def get(self, id):
    self.__storage.get(id)

  def add(self, val):
    self.__storage.add(val)

  def update(self, val):
    self.__storage.update(val)

  def remove(self, val):
    self.__storage.remove(val)

  def keys(self):
    return self.__storage.keys()

  def values(self):
    return self.__storage.values()

class ClientRepo(Repo):
  def __init__(self, storage):
    Repo.__init__(self, storage)

  def create(self, name):
    return Client(self.next_id(), name)

class MovieRepo(Repo):
  def __init__(self, storage):
    Repo.__init__(self, storage)

  def create(self, title, desc, genre):
    return Movie(self.next_id(), title, desc, genre)
      
class RentalRepo(Repo):
  def __init__(self, storage):
    Repo.__init__(self, storage)

  def create(self, client_id, movie_id, rented_date, due_date):
    return Rental(self.next_id(), client_id, movie_id, rented_date, due_date, None)