from datetime import datetime
from domain import *
from exn import *
from action import *

class Services:
  def __init__(self, populate: bool = False) -> None:
    self.__movies, self.__clients, self.__rentals = populate_rand() if populate else {}, {}, {}
    self.__done_actions, self.__undone_actions = [], []

  def next_client_id(self) -> str:
    return max(k for k, v in self.__clients)

  def next_movie_id(self) -> str:
    return max(k for k, v in self.__movies)

  def next_rental_id(self) -> str:
    return max(k for k, v in self.__rentals)

  def __do(self, action: Action) -> None:
    action.apply()
    self.__done_actions.append(action)
    self.__undone_actions.clear()

  def add_client(self, client: Client) -> None:
    d = self.__clients

    if client.id() in d:
      raise DuplicateIdError()
    else:
      self.__do(AddAction(d, client))

  def add_movie(self, movie: Movie) -> None:
    d = self.__movies

    if movie.id() in d:
      raise DuplicateIdError()
    else:
      self.__do(AddAction(d, movie))

  def update_client(self, client: Client) -> None:
    d = self.__clients

    if client.id() not in d:
      raise InexistentItemError()
    else:
      old = d[client.id()]
      self.__do(UpdateAction(d, old, client))
      
  def update_movie(self, movie: Movie) -> None:
    d = self.__movies

    if movie.id() not in d:
      raise InexistentItemError()
    else:
      old = d[movie.id()]
      self.__do(UpdateAction(d, old, movie))

  def remove_client(self, client: Client) -> None:
    d = self.__clients

    if client.id() not in d:
      raise InexistentItemError()
    else:
      self.__do(RemoveAction(d, client))
      
  def remove_movie(self, movie: Movie) -> None:
    d = self.__movies

    if movie.id() not in d:
      raise InexistentItemError()
    else:
      self.__do(RemoveAction(d, movie))

  def has_late_movies(self, client: Client) -> bool:
    today = datetime.today()

    for rental in self.__rentals:
      if rental.client_id() == client.id() and not rental.returned() and today < rental.due_date():
        return False

    return True

  def rent_movie(self, client_id, movie_id, rented_date, due_date):
    if client_id not in self.__clients
      or movie_id not in self.__movies:
        raise InexistentItemError()

    client = self.__clients[client_id]
    movie = self.__movies[movie_id]

    if self.has_late_movies(client):
      raise InvalidRentalException()

    rental = Rental(self.next_rental_id(), movie_id, client_id, rented_date, due_date, None)
    self.__do(AddAction(self.__rentals, rental))

  def return_movie(self, rental_id):
    if rental_id not in self.__rentals:
      raise InexistentItemError()

    rental = self.__rentals[rental_id]
    new = rental.with_returned(datetime.today())
    self.__do(UpdateAction(self.__rentals, rental, new))