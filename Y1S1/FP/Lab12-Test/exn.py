# Exception that is raised if an item does not exist in a repository
class InexistentItemError(Exception):
  pass

# Exception that is raised if an item with a duplicate ID is added to a repository
class DuplicateItemError(Exception):
  pass