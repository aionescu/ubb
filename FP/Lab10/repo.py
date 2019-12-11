from domain import *
from exn import *

# Class that manages multiple instances of a particular entity class
class Repo:
  def __init__(self, storage):
    self.__storage = storage

  @property
  def storage(self):
    return self.__storage

class ClientRepo(Repo):
  def __init__(self, storage):
    Repo.__init__(self, storage)

  def create(self, name):
    return Client(self.storage.next_id, name)

class MovieRepo(Repo):
  def __init__(self, storage):
    Repo.__init__(self, storage)

  def create(self, title, desc, genre):
    return Movie(self.storage.next_id, title, desc, genre)
      
class RentalRepo(Repo):
  def __init__(self, storage):
    Repo.__init__(self, storage)

  def create(self, client_id, movie_id, rented_date, due_date):
    return Rental(self.storage.next_id, client_id, movie_id, rented_date, due_date, None)