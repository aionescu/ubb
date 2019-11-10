from domain import populate_rand

class Services:
  def __init__(self, populate = False):
    self.__movies, self.__clients, self.__rentals = populate_rand() if populate else {}, {}, {}
    self.__done_actions, self.__undone_actions = [], []