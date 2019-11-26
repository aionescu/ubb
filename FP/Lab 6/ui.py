from datetime import datetime
from domain import *
from exn import *
from services import *

class Ui:
  def __init__(self, populate = False):
    self.__srv = Services(populate)

  def handle(self, cmd, args):
    try:
      getattr(self, cmd.replace('-', '_'))(args)
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
    try:
      self.__srv.update_client(int(args[0]), args[1])
    except InexistentItemError:
      print("Client does not exist.")
    except ValueError:
      print("ID must be an integer.")

  def update_m(self, args):
    try:
      self.__srv.update_movie(int(args[0]), args[1], args[2], args[3])
    except InexistentItemError:
      print("Movie does not exist.")
    except ValueError:
      print("ID must be an integer.")

  def remove_c(self, args):
    try:
      self.__srv.remove_client(int(args[0]))
    except InexistentItemError:
      print("Client does not exist.")
    except ValueError:
      print("ID must be an integer.")

  def remove_m(self, args):
    try:
      self.__srv.remove_movie(int(args[0]))
    except InexistentItemError:
      print("Movie does not exist.")
    except ValueError:
      print("ID must be an integer.")

  def list_c(self, args):
    print(self.__srv.list_clients())

  def list_m(self, args):
    print(self.__srv.list_movies())

  def list_r(self, args):
    print(self.__srv.list_rentals())

  def rent(self, args):
    def to_d(s):
      return datetime.strptime(s, "%Y-%m-%d")

    try:
      self.__srv.rent_movie(int(args[0]), int(args[1]), to_d(args[2]), to_d(args[3]))
    except InexistentItemError:
      print("Both client and movie must exist.")
    except InvalidRentalException:
      print("Client has late rentals.")
    except MovieNotAvailableError:
      print("Movie is not available for renting.")
    except ValueError:
      print("All IDs must be integers.")

  def return_m(self, args):
    try:
      self.__srv.return_movie(int(args[0]))
    except InexistentItemError:
      print("Rental does not exist.")
    except ValueError:
      print("ID must be an integer.")

  def search_c(self, args):
    try:
      print(self.__srv.search_clients(args[0], args[1]))
    except AttributeError:
      print("Incorrect search category.")

  def search_m(self, args):
    try:
      print(self.__srv.search_movies(args[0], args[1]))
    except AttributeError:
      print("Incorrect search category.")
      
  def stats_most_rented(self, args):
    print(self.__srv.stats_most_rented())

  def stats_most_active(self, args):
    print(self.__srv.stats_most_active())

  def stats_late(self, args):
    print(self.__srv.stats_late())

  def undo(self, args):
    try:
      self.__srv.undo()
    except InvalidUndoError:
      print("Nothing to undo.")

  def redo(self, args):
    try:
      self.__srv.redo()
    except InvalidRedoError:
      print("Nothing to redo.")