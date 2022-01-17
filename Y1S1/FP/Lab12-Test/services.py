from repo import *

# Class that manages the application's services
class Services:
  def __init__(self, addresses, drivers):
    self.__addresses = AddressRepo(addresses)
    self.__drivers = DriverRepo(drivers)

  def show_addresses(self):
    return '\n'.join(map(str, self.__addresses.data))

  def show_drivers(self):
    return '\n'.join(map(str, self.__drivers.data))

  # Function that returns a formatted list of drivers, sorted by distance
  # to the specified address.
  # Parameters: address_id - The ID of the address.
  # Returns: A formatted list of drivers, sorted by distance to
  # the address.
  # Raises: InexistentItemError if the address does not exist.
  def show_sorted(self, address_id):
    addr = self.__addresses.get(address_id)

    def dist(driver):
      return abs(driver.x - addr.x) + abs(driver.y - addr.y)

    return '\n'.join(map(str, sorted(self.__drivers.data, key = dist)))
  
  def show_closest(self):
    def dist(d1, d2):
      return abs(d1.x - d2.x) + abs(d1.y - d2.y)

    lst = self.__drivers.data
    ln = len(lst)

    min_dst = 2147483647
    pair = ((), ())

    for i in range(0, ln - 1):
      for j in range(i + 1, ln):
        d1 = lst[i]
        d2 = lst[j]
        dst = dist(d1, d2)

        if dst < min_dst:
          min_dst = dst
          pair = (d1, d2)

    return str(pair[0]) + "\n" + str(pair[1])