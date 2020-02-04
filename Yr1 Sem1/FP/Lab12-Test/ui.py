from services import *

class Ui:
  def __init__(self, addresses, drivers):
    try:
      self.__services = Services(addresses, drivers)
    except DuplicateItemError:
      print("Duplicate address ID found in file")
      exit()

    self.__cmds = {
      "exit": self.exit,
      "show-addr": self.show_addresses,
      "show-drv": self.show_drivers,
      "show-sorted": self.show_sorted,
      "show-closest": self.show_closest
    }

  def handle(self, cmd, arg):
    if cmd not in self.__cmds:
      print("Command not recognized.")
    else:
      self.__cmds[cmd](arg)

  def exit(self, arg):
    exit()

  def show_addresses(self, arg):
    print(self.__services.show_addresses())

  def show_drivers(self, arg):
    print(self.__services.show_drivers())

  def show_sorted(self, arg):
    if arg == "":
      print("No ID specified.")
      return

    try:
      print(self.__services.show_sorted(int(arg)))
    except ValueError:
      print("ID must be a number.")
    except InexistentItemError:
      print("Address does not exist.")

  def show_closest(self, arg):
    print(self.__services.show_closest())