from datetime import datetime

# Class that holds information about a client
# Fields:
#   - id: The ID of the client
#   - name: The name of the client
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

# Class that holds information about a movie
# Fields:
#   - id: The ID of the movie
#   - title: The title of the movie
#   - desc: The description of the movie
#   - genre: The genre of the movie
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

# Class that holds information about a rental
# Fields:
#   - id: The ID of the rental
#   - client_id: The ID of the client renting the movie
#   - movie_id: The ID of the movie that is being rented
#   - rented_date: The date the movie was rented on
#   - due_date: The date by which the rental must be returned
#   - return_date: [Optional] The day the movie was returned
class Rental:
  def __init__(self, id, client_id, movie_id, rented_date, due_date, return_date):
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