from datetime import datetime, timedelta
from random import randint

# Root of the inheritance hierarchy of entities
# Properties:
#   - id: The ID of the entity
class Entity:
  def __init__(self, id):
    self.__id = id

  @property
  def id(self):
    return self.__id

# Class that holds information about a client
# Properties:
#   - name: The name of the client
class Client(Entity):
  def __init__(self, id, name):
    Entity.__init__(self, id)
    self.__name = name

  @staticmethod
  def static_method(s):
    print(f"S is {s}")

  @property
  def name(self):
    return self.__name
    
  def __eq__(self, rhs):
    if not isinstance(rhs, Client):
      return NotImplemented

    return self.id == rhs.id

  def __str__(self):
    return f"[Client {self.id}] {self.name}"

# Class that holds information about a movie
# Properties:
#   - title: The title of the movie
#   - desc: The description of the movie
#   - genre: The genre of the movie
class Movie(Entity):
  def __init__(self, id, title, desc, genre):
    Entity.__init__(self, id)
    self.__title = title
    self.__desc = desc
    self.__genre = genre

  @property
  def title(self):
    return self.__title

  @property
  def desc(self):
    return self.__desc

  @property
  def genre(self):
    return self.__genre

  def __eq__(self, rhs):
    if not isinstance(rhs, Movie):
      return NotImplemented

    return self.id == rhs.id

  def __str__(self):
    return f"[Movie {self.id}] {self.title} (Desc: {self.desc}) (Genre: {self.genre})"

# Class that holds information about a rental
# Properties:
#   - client_id: The ID of the client renting the movie
#   - movie_id: The ID of the movie that is being rented
#   - rented_date: The date the movie was rented on
#   - due_date: The date by which the rental must be returned
#   - return_date: [Optional] The day the movie was returned
class Rental(Entity):
  def __init__(self, id, client_id, movie_id, rented_date, due_date, return_date):
    Entity.__init__(self, id)
    self.__movie_id = movie_id
    self.__client_id = client_id

    self.__rented_date = rented_date
    self.__due_date = due_date
    self.__return_date = return_date

  @property
  def client_id(self):
    return self.__client_id

  @property
  def movie_id(self):
    return self.__movie_id

  @property
  def rented_date(self):
    return self.__rented_date

  @property
  def due_date(self):
    return self.__due_date

  @property
  def return_date(self):
    return self.__return_date

  @property
  def returned(self):
    return self.__return_date is not None

  def with_returned(self, return_date):
    return Rental(self.__id, self.__client_id, self.__movie_id, self.__rented_date, self.__due_date, return_date)

  @property
  def days_rented(self):
    return (self.due_date - self.rented_date).days
    
  @property
  def is_late(self):
    return datetime.today() > self.due_date

  @property
  def days_late(self):
    return (datetime.today() - self.due_date).days

  def __eq__(self, rhs):
    if not isinstance(rhs, Rental):
      return NotImplemented

    return self.id == rhs.id

  def __str__(self):
    ret_str = "" if not self.returned else f" [Returned on {self.return_date.strftime('%Y-%m-%d')}]"
    return f"[Rental {self.id}] Client {self.client_id} rented Movie {self.movie_id} from {self.rented_date.strftime('%Y-%m-%d')} to {self.due_date.strftime('%Y-%m-%d')}" + ret_str

def populate_rand():
  fst_names = ["Alex", "Dan", "Mihai", "Ioana", "Florina", "Aurica"]
  lst_names = ["Ionescu", "Rotaru", "Oltean", "Georgescu", "Ionita"]

  names = []

  while len(names) < 10:
    f = fst_names[randint(0, len(fst_names) - 1)]
    l = lst_names[randint(0, len(lst_names) - 1)]

    n = f + " " + l
    if n not in names:
      names.append(n)

  nouns1 = ["Wrath", "Secrets", "Mystery", "Game"]
  nouns2 = ["Mushroom", "Librarian", "Programmer", "Bird"]
  descs = ["A nice family-friendly movie for a weekend night", "Blood and gore. Not suitable for minors.", "Not suitable for children under 12."]
  genres = ["Action", "Thriller", "Comedy", "Sci-Fi", "Documentary"]
  fmt1 = "%s of the %s"
  fmt2 = "Of %ss and %ss"
  fmt3 = "The %s"

  movie_names = []
  movies = []

  while len(movies) < 10:
    fmt = randint(1, 3)
    movie = "<No title>"

    if fmt == 1:
      n1 = nouns1[randint(0, len(nouns1) - 1)]
      n2 = nouns2[randint(0, len(nouns2) - 1)]
      movie = fmt1 % (n1, n2)
    elif fmt == 2:
      n1 = nouns2[randint(0, len(nouns2) - 1)]
      n2 = nouns2[randint(0, len(nouns2) - 1)]

      while n1 == n2:
        n2 = nouns2[randint(0, len(nouns2) - 1)]

      movie = fmt2 % (n1, n2)
    else:
      n1 = nouns2[randint(0, len(nouns2) - 1)]
      movie = fmt3 % n1

    if movie not in movie_names:
      movie_names.append(movie)
      desc = descs[randint(0, len(descs) - 1)]
      genre = genres[randint(0, len(genres) - 1)]
      movies.append((movie, desc, genre))

  rent_clients = []
  rent_movies = []
  rents = []
  today = datetime.today()
  init_date = today - timedelta(days = 15)

  while len(rents) < 5:
    p1 = randint(1, 10)
    p2 = randint(1, 10)

    if p1 not in rent_clients and p2 not in rent_movies:
      rent_clients.append(p1)
      rent_movies.append(p2)

      d = randint(-5, 5)
      rd = randint(-3, 3)
      rent_date = init_date + timedelta(days = rd)
      due_date = today + timedelta(days = d)

      rents.append((p1, p2, rent_date, due_date))

  return (names, movies, rents)