import sys
sys.path.append('../')

from map.map import Map
from domain import *
from exn import *
from map.map import Map

# Class that manages multiple instances of a particular entity class
class Repo:
  def __init__(self, loader):
    self.__loader = loader
    (self.__crr_id, d) = loader.load()
    self.__data = Map(d)

  @property
  def next_id(self):
    self.__crr_id += 1
    return self.__crr_id

  def must_exist(self, id):
    if id not in self.__data:
      raise InexistentItemError()

  def get(self, id):
    self.must_exist(id)
    return self.__data[id]

  def add(self, val):
    self.__data[val.id] = val
    self.__loader.save(self.__crr_id, self.__data.dict)

  def update(self, val):
    self.must_exist(val.id)
    self.__data[val.id] = val
    self.__loader.save(self.__crr_id, self.__data.dict)

  def remove(self, val):
    self.must_exist(val.id)
    del self.__data[val.id]
    self.__loader.save(self.__crr_id, self.__data.dict)

  def keys(self):
    for key in sorted(self.__data.keys):
      yield key

  def values(self):
    def get_id(x):
      return x.id

    for val in sorted(self.__data.values, key = get_id):
      yield val

class ClientRepo(Repo):
  def __init__(self, loader):
    Repo.__init__(self, loader)

  def create(self, name):
    return Client(self.next_id, name)

class MovieRepo(Repo):
  def __init__(self, loader):
    Repo.__init__(self, loader)

  def create(self, title, desc, genre):
    return Movie(self.next_id, title, desc, genre)
      
class RentalRepo(Repo):
  def __init__(self, loader):
    Repo.__init__(self, loader)

  def create(self, client_id, movie_id, rented_date, due_date):
    return Rental(self.next_id, client_id, movie_id, rented_date, due_date, None)