from typing import Dict
from domain import HasId

class Action:
  def __init__(self, dict: Dict[str, HasId]) -> None:
    self.__dict = dict

  def dict(self) -> Dict[str, HasId]:
    return self.__dict
    
  def apply(self) -> None:
    pass
  
  def roll_back(self) -> None:
    pass
  
class AddAction(Action):
  def __init__(self, dict: Dict[str, HasId], val: HasId) -> None:
    Action.__init__(self, dict)
    self.__val = val

  def apply(self) -> None:
    val = self.__val
    self.dict()[val.id()] = val

  def roll_back(self) -> None:
    del self.dict()[self.__val.id()]

class RemoveAction(Action):
  def __init__(self, dict: Dict[str, HasId], val) -> None:
    Action.__init__(self, dict)
    self.__val = val

  def apply(self) -> None:
    del self.dict()[self.__val.id()]

  def roll_back(self) -> None:
    val = self.__val
    self.dict()[val.id()] = val

class UpdateAction(Action):
  def __init__(self, dict: Dict[str, HasId], old_val, new_val) -> None:
    Action.__init__(self, dict)
    self.__old_val = old_val
    self.__new_val = new_val

  def apply(self) -> None:
    self.dict()[self.__old_val.id()] = self.__new_val

  def roll_back(self) -> None:
    self.dict()[self.__new_val.id()] = self.__old_val