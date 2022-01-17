import sys
sys.path.append('../')

from datetime import datetime
from domain import *
from action import *
from exn import *
from repo import *
from settings import *
from map.map import filter_by

class Services:
  def __init__(self, settings):
    loader = settings.loader

    self.__clients = ClientRepo(loader(settings.clients))
    self.__movies = MovieRepo(loader(settings.movies))
    self.__rentals = RentalRepo(loader(settings.rentals))
    
    self.__done_actions, self.__undone_actions = [], []

    if settings.populate:
      (clients, movies, rents) = populate_rand()

      for client in clients:
        self.add_client(client)

      for (title, desc, genre) in movies:
        self.add_movie(title, desc, genre)

      for (cid, mid, rented, due) in rents:
        self.rent_movie(cid, mid, rented, due)

      self.__done_actions, self.__undone_actions = [], []

  def __do(self, action):
    action.apply()
    self.__done_actions.append(action)
    self.__undone_actions.clear()

  # Function that adds a client to the app's state.
  # Input: name - The name of the client
  # Output: -
  # Postconditions: The client has been added to the app's state.
  # Raises: -
  def add_client(self, name):
    repo = self.__clients
    client = repo.create(name)
    self.__do(AddAction(repo, client))

  # Function that adds a movie to the app's state.
  # Input: title - The title of the movie, desc - The description of the movie, genre - The genre of the movie
  # Output: -
  # Postconditions: The movie has been added to the app's state.
  # Raises: -
  def add_movie(self, title, desc, genre):
    repo = self.__movies
    movie = repo.create(title, desc, genre)
    self.__do(AddAction(repo, movie))

  # Function that updates a client's information
  # Input: id - The ID of the client to update, name - The name of the client
  # Output: -
  # Postconditions: The client's information has been updated.
  # Raises: InexistentItemError if a client with the specified ID does not exist.
  def update_client(self, id, name):
    repo = self.__clients
    client = Client(id, name)
    old = repo.get(client.id)

    self.__do(UpdateAction(repo, old, client))
      
  # Function that updates a movie's information
  # Input: id - The ID of the movie to update, details - The movie's details
  # Output: -
  # Postconditions: The movie's information has been updated.
  # Raises: InexistentItemError if a movie with the specified ID does not exist.
  def update_movie(self, id, title, desc, genre):
    repo = self.__movies
    movie = Movie(id, title, desc, genre)
    old = repo.get(movie.id)

    self.__do(UpdateAction(repo, old, movie))

  # Function that removes a client from the app's state.
  # Input: id - The ID of the client
  # Output: -
  # Postconditions: The client has been removed from the app's state.
  # Raises: InexistentItemError if a client with the specified ID does not exist.
  def remove_client(self, id):
    repo = self.__clients
    client = repo.get(id)
    actions = [RemoveAction(repo, client)]

    for r in self.__rentals.values():
      if r.client_id == id:
        actions.append(RemoveAction(self.__rentals, r))

    self.__do(MultiAction(actions))

  # Function that removes a movie from the app's state.
  # Input: id - The ID of the movie
  # Output: -
  # Postconditions: The movie has been removed from the app's state.
  # Raises: InexistentItemError if a movie with the specified ID does not exist.
  def remove_movie(self, id):
    repo = self.__movies
    movie = repo.get(id)
    actions = [RemoveAction(repo, movie)]

    for r in self.__rentals.values():
      if r.movie_id == id:
        actions.append(RemoveAction(self.__rentals, r))

    self.__do(MultiAction(actions))

  # Function that returns a formatted list of all the clients.
  # Input: -
  # Output: A formatted list containing the string representation of all clients.
  # Postconditions: The app's state is not modified.
  # Raises: -
  def list_clients(self):
    return '\n'.join(map(str, self.__clients.values()))

  # Function that returns a formatted list of all the movies.
  # Input: -
  # Output: A formatted list containing the string representation of all movies.
  # Postconditions: The app's state is not modified.
  # Raises: -
  def list_movies(self):
    return '\n'.join(map(str, self.__movies.values()))

  # Function that returns a formatted list of all the rentals.
  # Input: -
  # Output: A formatted list containing the string representation of all rentals.
  # Postconditions: The app's state is not modified.
  # Raises: -
  def list_rentals(self):
    return '\n'.join(map(str, self.__rentals.values()))

  def __has_late_movies(self, id):
    client = self.__clients.get(id)

    for rental in self.__rentals.values():
      if rental.client_id == client.id and not rental.returned and rental.is_late:
        return True

    return False

  def rent_movie(self, client_id, movie_id, rented_date, due_date):
    self.__clients.must_exist(client_id)
    self.__movies.must_exist(movie_id)

    if self.__has_late_movies(client_id):
      raise InvalidRentalException()

    repo = self.__rentals

    def predicate(r):
      return r.movie_id == movie_id and not r.returned

    if any(map(predicate, repo.values())):
      raise MovieNotAvailableError()

    rental = repo.create(client_id, movie_id, rented_date, due_date)
    self.__do(AddAction(repo, rental))

  def return_movie(self, rental_id):
    repo = self.__rentals
    rental = repo.get(rental_id)

    if rental.returned:
      raise RentalReturnedError()
    
    new = rental.with_returned(datetime.today())
    self.__do(UpdateAction(repo, rental, new))

  def search_clients(self, field, value):
    def getter(c):
      return getattr(c, field)

    value = value.lower()

    def predicate(c):
      return value in str(getter(c)).lower()

    return '\n'.join(map(str, filter_by(predicate, self.__clients.values())))

  def search_movies(self, field, value):
    def getter(c):
      return getattr(c, field)

    value = value.lower()

    def predicate(c):
      return value in str(getter(c)).lower()

    return '\n'.join(map(str, filter_by(predicate, self.__movies.values())))

  def stats_most_rented(self):
    def with_snd(x):
      return (x, 0)

    def snd(x):
      return x[1]

    def of_tuple(x):
      return self.__movies.get(x[0])

    def gt0(x):
      return x[1] > 0

    movie_days = dict(map(with_snd, self.__movies.keys()))

    for r in self.__rentals.values():
      movie_days[r.movie_id] += r.days_rented

    movies = map(of_tuple, filter_by(gt0, sorted(movie_days.items(), key = snd, reverse = True)))
    return '\n'.join(map(str, movies))

  def stats_most_active(self):
    def with_snd(x):
      return (x, 0)

    def snd(x):
      return x[1]

    def of_tuple(x):
      return self.__clients.get(x[0])

    def gt0(x):
      return x[1] > 0

    client_days = dict(map(with_snd, self.__clients.keys()))

    for r in self.__rentals.values():
      client_days[r.client_id] += r.days_rented

    clients = map(of_tuple, filter_by(gt0, sorted(client_days.items(), key = snd, reverse = True)))
    return '\n'.join(map(str, clients))

  def distinct(self, iterable):
    seen = set()
    for item in iterable:
      if item not in seen:
        seen.add(item)
        yield item

  def stats_late(self):
    def is_late(x):
      return x.is_late

    def days_late(x):
      return x.days_late

    def movie_id(x):
      return x.movie_id
    
    def of_id(x):
      return self.__movies.get(x)

    movies = map(of_id, self.distinct(map(movie_id, sorted(filter_by(is_late, self.__rentals.values()), key = days_late, reverse = True))))
    return '\n'.join(map(str, movies))

  def undo(self):
    try:
      last = self.__done_actions.pop()
    except:
      raise InvalidUndoError()
    
    last.roll_back()
    self.__undone_actions.append(last)

  def redo(self):
    try:
      last = self.__undone_actions.pop()
    except:
      raise InvalidRedoError()
    
    last.apply()
    self.__done_actions.append(last)