class Action:    
  def apply(self):
    pass
  
  def roll_back(self):
    pass
  
class LambdaAction(Action):
    def __init__(self, apply, roll_back):
        self.__apply = apply
        self.__roll_back = roll_back

    def apply(self):
        self.__apply()

    def roll_back(self):
        self.__roll_back()

class CreateAction(Action):
  def __init__(self, repo, val):
    self.__repo = repo
    self.__val = val

  def apply(self):
    self.__repo.create(self.__val)

  def roll_back(self):
    self.__repo.delete(self.__val.id_entity)

class DeleteAction(Action):
  def __init__(self, repo, val):
    self.__repo = repo
    self.__val = val

  def apply(self):
    self.__repo.delete(self.__val.id_entity)

  def roll_back(self):
    self.__repo.create(self.__val)

class UpdateAction(Action):
  def __init__(self, repo, old_val, new_val):
    self.__repo = repo
    self.__old_val = old_val
    self.__new_val = new_val

  def apply(self):
    self.__repo.update(self.__new_val)

  def roll_back(self):
    self.__repo.update(self.__old_val)

class MultiAction(Action):
  def __init__(self, actions):
    self.__actions = actions

  def apply(self):
    for a in self.__actions:
      a.apply()

  def roll_back(self):
    for a in reversed(self.__actions):
      a.roll_back()