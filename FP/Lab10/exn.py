# Exception that is raised if a value does not exist inside a repository in certain circumstances.
class InexistentItemError(Exception):
  pass

# Exception that is raised if a movie cannot be rented due to late rentals.
class InvalidRentalException(Exception):
  pass

# Exception that is raised if an attempt is made to rent a movie that is already rented
class MovieNotAvailableError(Exception):
  pass

# Exception that is raised if an attempt is made to return a rental that is already returned
class RentalReturnedError(Exception):
  pass

# Exception that is raised if an undo is attempted when there is nothing to undo.
class InvalidUndoError(Exception):
  pass

# Exception that is raised if a redo is attempted when there is nothing to redo.
class InvalidRedoError(Exception):
  pass

class SerializationError(Exception):
  pass