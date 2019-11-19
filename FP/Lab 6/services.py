from datetime import datetime
from domain import *
from exn import *
from action import *

class Services:
  def __init__(self, populate: bool = False):
    self.__movies, self.__clients, self.__rentals = populate_rand() if populate else ({}, {}, {})
    self.__done_actions, self.__undone_actions = [], []
    self.__crr_client_id, self.__crr_movie_id, self.__crr_rental_id = 0, 0, 0

  def next_client_id(self):
    self.__crr_client_id += 1
    return self.__crr_client_id

  def next_movie_id(self):
    self.__crr_movie_id += 1
    return self.__crr_movie_id

  def next_rental_id(self):
    self.__crr_rental_id += 1
    return self.__crr_rental_id

  def __do(self, action):
    action.apply()
    self.__done_actions.append(action)
    self.__undone_actions.clear()

  def add_client(self, name):
    d = self.__clients
    client = Client(self.next_client_id(), name)
    self.__do(AddAction(d, client.id(), client))

  def add_movie(self, title, desc, genre):
    d = self.__movies
    movie = Movie(self.next_movie_id(), title, desc, genre)
    self.__do(AddAction(d, movie.id(), movie))

  def update_client(self, id, name):
    d = self.__clients
    client = Client(id, name)

    if client.id() not in d:
      raise InexistentItemError()
    else:
      old = d[client.id()]
      self.__do(UpdateAction(d, client.id(), old, client))
      
  def update_movie(self, id, title, desc, genre):
    d = self.__movies
    movie = Movie(id, title, desc, genre)

    if movie.id() not in d:
      raise InexistentItemError()
    else:
      old = d[movie.id()]
      self.__do(UpdateAction(d, movie.id(), old, movie))

  def remove_client(self, id):
    d = self.__clients

    if id not in d:
      raise InexistentItemError()
    else:
      self.__do(RemoveAction(d, id, d[id]))
      
  def remove_movie(self, id):
    d = self.__movies

    if id not in d:
      raise InexistentItemError()
    else:
      self.__do(RemoveAction(d, id, d[id]))

  def list_clients(self):
    return '\n'.join(map(str, self.__clients.values()))

  def list_movies(self):
    return '\n'.join(map(str, self.__movies.values()))

  def has_late_movies(self, client_id):
    if client_id not in self.__clients:
      raise InexistentItemError()

    client = self.__clients[client_id]

    for _, rental in self.__rentals:
      if rental.client_id() == client.id() and not rental.returned() and rental.is_late():
        return False

    return True

  def rent_movie(self, client_id, movie_id, rented_date, due_date):
    if client_id not in self.__clients or movie_id not in self.__movies:
        raise InexistentItemError()

    if self.has_late_movies(client_id):
      raise InvalidRentalException()

    rental = Rental(self.next_rental_id(), movie_id, client_id, rented_date, due_date, None)
    self.__do(AddAction(self.__rentals, rental.id(), rental))

  def return_movie(self, rental_id):
    if rental_id not in self.__rentals:
      raise InexistentItemError()

    rental = self.__rentals[rental_id]
    new = rental.with_returned(datetime.today())
    self.__do(UpdateAction(self.__rentals, rental_id, rental, new))

  def search_clients(self, field, value):
    def getter(c):
      return getattr(c, field)()

    value = value.lower()

    def predicate(c):
      return value in str(getter(c)).lower()

    return '\n'.join(map(str, filter(predicate, self.__clients.values())))

  def search_movies(self, field, value):
    def getter(c):
      return getattr(c, field)()

    value = value.lower()

    def predicate(c):
      return value in str(getter(c)).lower()

    return '\n'.join(map(str, filter(predicate, self.__movies.values())))

  def stats_most_rented(self):
    def with_snd(x):
      return (x, 0)

    def snd(x):
      return x[1]

    def of_tuple(x):
      return self.__movies[x[0]]

    movie_days = dict(map(with_snd, self.__movies.keys()))

    for _, r in self.__rentals:
      movie_days[r.movie_id()] += r.days_rented()

    movies = map(of_tuple, sorted(movie_days.items(), key = snd, reverse = True))
    return '\n'.join(map(str, movies))

  def stats_most_active(self):
    def with_snd(x):
      return (x, 0)

    def snd(x):
      return x[1]

    def of_tuple(x):
      return self.__clients[x[0]]

    client_days = dict(map(with_snd, self.__clients.keys()))

    for _, r in self.__rentals:
      client_days[r.client_id()] += r.days_rented()

    clients = map(of_tuple, sorted(client_days.items(), key = snd, reverse = True))
    return '\n'.join(map(str, clients))

  def distinct(self, iterable):
    seen = set()
    for item in iterable:
      if item not in seen:
        seen.add(item)
        yield item

  def stats_late(self):
    def is_late(x):
      return x.is_late()

    def days_late(x):
      return x.days_late()

    def movie_id(x):
      return x.movie_id()
    
    def of_id(x):
      return self.__movies[x]

    movies = map(of_id, self.distinct(map(movie_id, sorted(filter(is_late, self.__rentals.values()), key = days_late, reverse = True))))
    return '\n'.join(map(str, movies))