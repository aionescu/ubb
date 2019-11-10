class Movie:
  def __init__(self, id, title, description, genre):
    self.__id = id
    self.__title = title
    self.__description = description
    self.__genre = genre

  def id(self):
    return self.__id

  def title(self):
    return self.__title

  def description(self):
    return self.__description

  def genre(self):
    return self.__genre

  def __eq__(self, rhs):
    return self.__id == rhs.__id

class Client:
  def __init__(self, id, name):
    self.__id = id
    self.__name = name

  def id(self):
    return self.__id

  def name(self):
    return self.__name

  def __eq__(self, rhs):
    return self.__id == rhs.__id

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

  def __eq__(self, rhs):
    return self.__id == rhs.__id

def rand_movies():
  pass

def rand_clients():
  pass

def rand_rentals(movies, clients):
  pass

def populate_rand():
  movies = rand_movies()
  clients = rand_clients()
  return movies, clients, rand_rentals(movies, clients)