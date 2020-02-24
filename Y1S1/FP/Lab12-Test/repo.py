from domain import *
from exn import *

# Class that manages a repository of values
class Repo:
  def __init__(self):
    self.__data = []

  @property
  def data(self):
    return self.__data

  # Function that inserts a value into the repository
  # Parameters: val - The value to add
  def insert(self, val):
    self.__data.append(val)

# Class that manages a repository of addresses
class AddressRepo(Repo):
  def __init__(self, filename):
    Repo.__init__(self)

    with open(filename, "r") as file:
      while True:
        line = file.readline()

        if not line:
          break

        line = line.split(",")

        addr = self.create(int(line[0]), line[1], int(line[2]), int(line[3]))
        self.insert(addr)

  def unique_check(self, id):
    for addr in self.data:
      if addr.id == id:
        raise DuplicateItemError()

  # Function that returns the address with the specified ID
  # Parameters: id - The ID to lookup
  # Returns: The address with the specified ID
  # Raises: InexistentItemError if the ID is not present in the repository
  def get(self, id):
    for addr in self.data:
      if addr.id == id:
        return addr

    raise InexistentItemError()

  # Function that creates a new address
  # Parameters:
  #   - id: The id of the address
  #   - name: The street name of the address
  #   - x: The X coordinate of the address
  #   - y: The Y coordinate of the address
  def create(self, id, name, x, y):
    self.unique_check(id)
    return Address(id, name, x, y)

# Class that manages a repository of drivers
class DriverRepo(Repo):
  def __init__(self, filename):
    Repo.__init__(self)

    with open(filename, "r") as file:
      while True:
        line = file.readline()

        if not line:
          break

        line = line.split(",")

        driver = self.create(line[0], int(line[1]), int(line[2]))
        self.insert(driver)

  # Function that creates a new driver
  # Parameters:
  #   - name: The name of the driver
  #   - x: The X coordinate of the driver
  #   - y: The Y coordinate of the driver
  def create(self, name, x, y):
    return Driver(name, x, y)