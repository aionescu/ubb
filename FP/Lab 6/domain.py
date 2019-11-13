from typing import Dict, Tuple

class HasId:
  def id(self) -> str:
    pass
  
class Movie(HasId):
  def __init__(self, id: str, title: str, description: str, genre: str) -> None:
    self.__id = id
    self.__title = title
    self.__description = description
    self.__genre = genre

  def id(self) -> str:
    return self.__id

  def title(self) -> str:
    return self.__title

  def description(self) -> str:
    return self.__description

  def genre(self) -> str:
    return self.__genre

  def __eq__(self, rhs: object) -> bool:
    if not isinstance(rhs, Movie):
      return NotImplemented

    return self.__id == rhs.__id

class Client(HasId):
  def __init__(self, id: str, name: str) -> None:
    self.__id = id
    self.__name = name

  def id(self) -> str:
    return self.__id

  def name(self) -> str:
    return self.__name

  def __eq__(self, rhs: object) -> bool:
    if not isinstance(rhs, Client):
      return NotImplemented

    return self.__id == rhs.__id

class Rental(HasId):
  def __init__(self, id: str, movie_id: str, client_id: str, rented_date: str, due_date: str, return_date: str) -> None:
    self.__id = id
    self.__movie_id = movie_id
    self.__client_id = client_id

    self.__rented_date = rented_date
    self.__due_date = due_date
    self.__return_date = return_date

  def id(self) -> str:
    return self.__id

  def movie_id(self) -> str:
    return self.__movie_id

  def client_id(self) -> str:
    return self.__client_id

  def rented_date(self) -> str:
    return self.__rented_date

  def due_date(self) -> str:
    return self.__due_date

  def return_date(self) -> str:
    return self.__return_date

  def returned(self) -> bool:
    return self.__return_date is not None

  def with_returned(self, return_date: str) -> Rental:
    return Rental(self.__id, self.__movie_id, self.__client_id, self.__rented_date, self.__due_date, return_date)

  def __eq__(self, rhs: object) -> bool:
    if not isinstance(rhs, Rental):
      return NotImplemented

    return self.__id == rhs.__id

def rand_movies() -> Dict[str, Movie]:
  pass

def rand_clients() -> Dict[str, Client]:
  pass

def rand_rentals(movies: Dict[str, Movie], clients: Dict[str, Client]) -> Dict[str, Rental]:
  pass

def populate_rand() -> Tuple[Dict[str, Movie], Dict[str, Client], Dict[str, Rental]]:
  movies = rand_movies()
  clients = rand_clients()
  return movies, clients, rand_rentals(movies, clients)