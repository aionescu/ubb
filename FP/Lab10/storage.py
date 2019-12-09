from exn import InexistentItemError
import pickle

class Storage:
  def __init__(self):
    self.__data = {}
    self.load()

  @property
  def data(self):
    return self.__data
  
  @data.setter
  def data(self, data):
    self.__data = data

  def load(self):
    pass

  def save(self):
    pass

  def must_exist(self, id):
    if id not in self.__data:
      raise InexistentItemError()

  def get(self, id):
    self.must_exist(id)
    return self.__data[id]

  def add(self, val):
    self.__data[val.id] = val
    self.save()

  def update(self, val):
    self.must_exist(val.id)
    self.__data[val.id] = val
    self.save()

  def remove(self, val):
    self.must_exist(val.id)
    del self.__data[val.id]
    self.save()

  def keys(self):
    for key in sorted(self.__data.keys):
      yield key

  def values(self):
    def get_id(x):
      return x.id

    for val in sorted(self.__data.values(), key = get_id):
      yield val

class InMemoryStorage(Storage):
  def __init__(self):
    Storage.__init__(self)

  def load(self):
    pass

  def save(self):
    pass

class TextFileStorage(Storage):
  def __init__(self, ty, file):
    self.__ty = ty
    self.__file = file
    Storage.__init__(self)

  def load(self):
    with open(self.__file, "r") as file:
      while True:
        pos = file.tell()
        line = file.readline()

        if not line:
          break

        file.seek(pos)

        val = self.__ty.deserialize(file)
        self.data[val.id] = val

  def save(self):
    with open(self.__file, "w") as file:
      for val in self.values():
        val.serialize(file)

class BinaryFileStorage(Storage):
  def __init__(self, file):
    self.__file = file
    Storage.__init__(self)

  def load(self):
    with open(self.__file, "rb") as file:
      self.data = pickle.load(file)
      
  def save(self):
    with open(self.__file, "wb") as file:
      pickle.dump(self.data, file)