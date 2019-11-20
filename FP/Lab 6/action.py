class Action:
  def __init__(self, repo):
    self.__repo = repo

  def repo(self):
    return self.__repo
    
  def apply(self):
    pass
  
  def roll_back(self):
    pass
  
class AddAction(Action):
  def __init__(self, repo, val):
    Action.__init__(self, repo)
    self.__val = val

  def apply(self):
    self.repo().add(self.__val)

  def roll_back(self):
    self.repo().remove(self.__val)

class RemoveAction(Action):
  def __init__(self, repo, val):
    Action.__init__(self, repo)
    self.__val = val

  def apply(self):
    self.repo().remove(self.__val)

  def roll_back(self):
    self.repo().add(self.__val)

class UpdateAction(Action):
  def __init__(self, repo, old_val, new_val):
    Action.__init__(self, repo)
    self.__old_val = old_val
    self.__new_val = new_val

  def apply(self):
    self.repo().update(self.__new_val)

  def roll_back(self):
    self.repo().update(self.__old_val)