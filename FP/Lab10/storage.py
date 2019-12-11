from exn import InexistentItemError
import pickle
import jsonpickle

class Storage:
  def __init__(self):
    self.__data = {}
    self.__crr_id = 0
    self.load()

  @property
  def crr_id(self):
    return self.__crr_id

  @crr_id.setter
  def crr_id(self, crr_id):
    self.__crr_id = crr_id

  @property
  def next_id(self):
    self.__crr_id += 1
    return self.__crr_id

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

class MemStorage(Storage):
  def __init__(self, _file):
    Storage.__init__(self)

  def load(self):
    pass

  def save(self):
    pass

class JsonStorage(Storage):
  def __init__(self, file):
    self.__file = file
    Storage.__init__(self)

  def load(self):
    try:
      with open(self.__file, "r") as file:
        json = file.read()
        (self.crr_id, self.data) = jsonpickle.decode(json)
    except FileNotFoundError:
      pass

  def save(self):
    with open(self.__file, "w") as file:
      json = jsonpickle.encode((self.crr_id, self.data))
      file.write(json)

class PickleStorage(Storage):
  def __init__(self, file):
    self.__file = file
    Storage.__init__(self)

  def load(self):
    try:
      with open(self.__file, "rb") as file:
        (self.crr_id, self.data) = pickle.load(file)
    except FileNotFoundError:
      pass

  def save(self):
    with open(self.__file, "wb") as file:
      pickle.dump((self.crr_id, self.data), file)