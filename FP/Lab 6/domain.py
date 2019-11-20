from datetime import datetime
  
class Movie:
  def __init__(self, id, title, desc, genre):
    self.__id = id
    self.__title = title
    self.__desc = desc
    self.__genre = genre

  def id(self):
    return self.__id

  def title(self):
    return self.__title

  def desc(self):
    return self.__desc

  def genre(self):
    return self.__genre

  def __eq__(self, rhs):
    if not isinstance(rhs, Movie):
      return NotImplemented

    return self.__id == rhs.__id

  def __str__(self):
    return f"[Movie {self.id()}] {self.title()} (Desc: {self.desc()}) (Genre: {self.genre()})"

class Client:
  def __init__(self, id, name):
    self.__id = id
    self.__name = name

  def id(self):
    return self.__id

  def name(self):
    return self.__name

  def __eq__(self, rhs):
    if not isinstance(rhs, Client):
      return NotImplemented

    return self.__id == rhs.__id

  def __str__(self):
    return f"[Client {self.id()}] {self.name()}"

class Rental:
  def __init__(self, id, movie_id, client_id, rented_date, due_date, return_date):
    self.__id = id
    self.__movie_id = movie_id
    self.__client_id = client_id

    self.__rented_date = rented_date
    self.__due_date = due_date
    self.__return_date = return_date

  def id(self):
    return self.__id

  def movie_id(self):
    return self.__movie_id

  def client_id(self):
    return self.__client_id

  def rented_date(self):
    return self.__rented_date

  def due_date(self):
    return self.__due_date

  def return_date(self):
    return self.__return_date

  def returned(self):
    return self.__return_date is not None

  def with_returned(self, return_date):
    return Rental(self.__id, self.__movie_id, self.__client_id, self.__rented_date, self.__due_date, return_date)

  def days_rented(self):
    return (self.due_date() - self.rented_date()).days

  def is_late(self):
    return datetime.today() > self.due_date()

  def days_late(self):
    return (datetime.today() - self.due_date()).days

  def __eq__(self, rhs):
    if not isinstance(rhs, Rental):
      return NotImplemented

    return self.__id == rhs.__id

  def __str__(self):
    return f"[Rental {self.id()}] Client {self.client_id()} rented Movie {self.movie_id()} from {self.rented_date()} to {self.due_date()} [Returned: {self.return_date()}]"