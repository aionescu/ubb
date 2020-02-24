import pickle
import jsonpickle
import os

def is_empty_file(fpath):  
    return os.path.isfile(fpath) and os.path.getsize(fpath) == 0

class Loader:
  def load(self):
    pass

  def save(self, crr_id, data):
    pass

class MemLoader(Loader):
  def __init__(self, _file):
    Loader.__init__(self)

  def load(self):
    return (0, {})

  def save(self):
    pass

class JsonLoader(Loader):
  def __init__(self, file):
    self.__file = file
    Loader.__init__(self)

  def load(self):
    if is_empty_file(self.__file):
      return (0, {})

    try:
      with open(self.__file, "r") as file:
        json = file.read()
        return jsonpickle.decode(json)
    except FileNotFoundError:
      return (0, {})

  def save(self, crr_id, data):
    with open(self.__file, "w") as file:
      json = jsonpickle.encode((crr_id, data))
      file.write(json)

class PickleLoader(Loader):
  def __init__(self, file):
    self.__file = file
    Loader.__init__(self)

  def load(self):
    if is_empty_file(self.__file):
      return (0, {})

    try:
      with open(self.__file, "rb") as file:
        return pickle.load(file)
    except FileNotFoundError:
      return (0, {})

  def save(self, crr_id, data):
    with open(self.__file, "wb") as file:
      pickle.dump((crr_id, data), file)