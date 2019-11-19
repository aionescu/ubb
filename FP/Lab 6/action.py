class Action:
  def __init__(self, dict):
    self.__dict = dict

  def dict(self):
    return self.__dict
    
  def apply(self):
    pass
  
  def roll_back(self):
    pass
  
class AddAction(Action):
  def __init__(self, dict, id, val):
    Action.__init__(self, dict)
    self.__id = id
    self.__val = val

  def apply(self):
    self.dict()[self.__id] = self.__val

  def roll_back(self):
    del self.dict()[self.__id]

class RemoveAction(Action):
  def __init__(self, dict, id, val):
    Action.__init__(self, dict)
    self.__id = id
    self.__val = val

  def apply(self):
    del self.dict()[self.__id]

  def roll_back(self):
    self.dict()[self.__id] = self.__val

class UpdateAction(Action):
  def __init__(self, dict, id, old_val, new_val):
    Action.__init__(self, dict)
    self.__id = id
    self.__old_val = old_val
    self.__new_val = new_val

  def apply(self):
    self.dict()[self.__id] = self.__new_val

  def roll_back(self):
    self.dict()[self.__id] = self.__old_val