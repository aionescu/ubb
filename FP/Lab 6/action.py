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
  def __init__(self, dict, val):
    Action.__init__(self, dict)
    self.__val = val

  def apply(self):
    val = self.__val
    self.dict()[val.id()] = val

  def roll_back(self):
    del self.dict()[self.__val.id()]

class RemoveAction(Action):
  def __init__(self, dict, val):
    Action.__init__(self, dict)
    self.__val = val

  def apply(self):
    del self.dict()[self.__val.id()]

  def roll_back(self):
    val = self.__val
    self.dict()[val.id()] = val

class UpdateAction(Action):
  def __init__(self, dict, old_val, new_val):
    Action.__init__(self, dict)
    self.__old_val = old_val
    self.__new_val = new_val

  def apply(self):
    self.dict()[self.__old_val.id()] = self.__new_val

  def roll_back(self):
    self.dict()[self.__new_val.id()] = self.__old_val