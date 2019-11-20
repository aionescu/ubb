# Exception that is raised if a value does not exist inside a repository in certain circumstances.
class InexistentItemError(Exception):
  pass

# Exception that is raised if a movie cannot be rented due to late rentals.
class InvalidRentalException(Exception):
  pass

# Exception that is raised if an undo is attempted when there is nothing to undo.
class InvalidUndoError(Exception):
  pass

# Exception that is raised if a redo is attempted when there is nothing to redo.
class InvalidRedoError(Exception):
  pass