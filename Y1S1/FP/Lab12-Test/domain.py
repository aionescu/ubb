# Class that holds information about an address
# Properties:
#   - id: The id of the address
#   - name: The street name of the address
#   - x: The X coordinate of the address
#   - y: The Y coordinate of the address
class Address:
  def __init__(self, id, name, x, y):
    self.__id = id
    self.__name = name
    self.__x = x
    self.__y = y

  @property
  def id(self):
    return self.__id

  @property
  def name(self):
    return self.__name

  @property
  def x(self):
    return self.__x

  @property
  def y(self):
    return self.__y

  def __str__(self):
    return f"{self.id},{self.name},{self.x},{self.y}"

# Class that holds information about a driver
# Properties:
#   - name: The name of the driver
#   - x: The X coordinate of the driver
#   - y: The Y coordinate of the driver
class Driver:
  def __init__(self, name, x, y):
    self.__name = name
    self.__x = x
    self.__y = y

  @property
  def name(self):
    return self.__name

  @property
  def x(self):
    return self.__x

  @property
  def y(self):
    return self.__y

  def __str__(self):
    return f"{self.name},{self.x},{self.y}"