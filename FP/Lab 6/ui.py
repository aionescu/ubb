from datetime import datetime
from domain import *
from services import *

class Ui:
  def __init__(self):
    self.__srv = Services(True)

  def handle(self, args):
    try:
      getattr(self, args[0].replace('-', '_'))(args[1:])
    except AttributeError as ae:
      if str(ae).startswith("'Ui'"):
        print("Command not recognized.")
      else:
        raise ae
      
  def exit(self, args):
    exit()

  def add_c(self, args):
    self.__srv.add_client(args[0])

  def add_m(self, args):
    self.__srv.add_movie(args[0], args[1], args[2])

  def update_c(self, args):
    self.__srv.update_client(int(args[0]), args[1])

  def update_m(self, args):
    self.__srv.update_movie(int(args[0]), args[1], args[2], args[3])

  def remove_c(self, args):
    self.__srv.remove_client(int(args[0]))

  def remove_m(self, args):
    self.__srv.remove_movie(int(args[0]))

  def list_c(self, args):
    print(self.__srv.list_clients())

  def list_m(self, args):
    print(self.__srv.list_movies())

  def rent(self, args):
    def to_d(s):
      return datetime.strptime(s, "%Y-%m-%d")

    self.__srv.rent_movie(int(args[0]), int(args[1]), to_d(args[2]), to_d(args[3]))

  def return_m(self, args):
    self.__srv.return_movie(int(args[0]))

  def search_c(self, args):
    print(self.__srv.search_clients(args[0], args[1]))

  def search_m(self, args):
    print(self.__srv.search_movies(args[0], args[1]))

  def stats_m(self, args):
    print(self.__srv.stats_most_rented())

  def stats_c(self, args):
    print(self.__srv.stats_most_active())

  def stats_late(self, args):
    print(self.__srv.stats_late())