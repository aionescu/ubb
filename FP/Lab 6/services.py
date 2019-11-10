from domain import populate_rand
from exn import *
from action import *

class Services:
  def __init__(self, populate = False):
    self.__movies, self.__clients, self.__rentals = populate_rand() if populate else {}, {}, {}
    self.__done_actions, self.__undone_actions = [], []

  def __do(self, action):
    action.apply()
    self.__done_actions.append(action)
    self.__undone_actions.clear()

  def add_client(self, client):
    d = self.__clients

    if client.id() in d:
      raise DuplicateIdError()
    else:
      self.__do(AddAction(d, client))

  def add_movie(self, movie):
    d = self.__movies

    if movie.id() in d:
      raise DuplicateIdError()
    else:
      self.__do(AddAction(d, movie))

  def update_client(self, client):
    d = self.__clients

    if client.id() not in d:
      raise InexistentItemError()
    else:
      old = d[client.id()]
      self.__do(UpdateAction(d, old, client))
      
  def update_movie(self, movie):
    d = self.__movies

    if movie.id() not in d:
      raise InexistentItemError()
    else:
      old = d[movie.id()]
      self.__do(UpdateAction(d, old, movie))

  def remove_client(self, client):
    d = self.__clients

    if client.id() not in d:
      raise InexistentItemError()
    else:
      self.__do(RemoveAction(d, client))
      
  def remove_movie(self, movie):
    d = self.__movies

    if movie.id() not in d:
      raise InexistentItemError()
    else:
      self.__do(RemoveAction(d, movie))