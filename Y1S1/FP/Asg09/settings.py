from loader import *
from exn import *

class Settings:
  def __init__(self, path):
    with open(path, "r") as file: 
      repo = file.readline().replace("repo=", "").replace("\n", "")
      self.__clients = file.readline().replace("clients=", "").replace("\n", "")
      self.__movies = file.readline().replace("movies=", "").replace("\n", "")
      self.__rentals = file.readline().replace("rentals=", "").replace("\n", "")

      if repo == "mem":
        self.__loader = MemLoader
        self.__populate = True
      elif repo == "json":
        self.__loader = JsonLoader
        self.__populate = False
      elif repo == "pickle":
        self.__loader = PickleLoader
        self.__populate = False
      else:
        raise InvalidSettingsError

  @property
  def loader(self):
    return self.__loader

  @property
  def populate(self):
    return self.__populate

  @property
  def clients(self):
    return self.__clients

  @property
  def movies(self):
    return self.__movies

  @property
  def rentals(self):
    return self.__rentals