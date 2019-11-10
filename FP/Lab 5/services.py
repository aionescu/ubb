from book import rand_books

class Services:
  def __init__(self, init_rand = True):
    self.books = rand_books() if init_rand else []
    self.undo = []
    self.redo = []

  def contains(self, book):
    def same_isbn(b):
      return b.isbn == book.isbn

    return any(map(same_isbn, self.books))

  # Adds the specified book to the collection if it does not already exist (i.e. if no books with the same ISBN exist)
  # Input: The book to add
  # Output: -
  # Raises: Exception if the book already exists
  # Postconditions: If the operation was successful, the collection should contain the new book
  def try_add_book(self, book):
    if self.contains(book):
      raise Exception("Book already exists.")

    self.undo.append(self.books.copy())
    self.redo.clear()

    self.books.append(book)

  # Returns a formatted list of all the books in the collection
  # Input: -
  # Output: A string that contains the string representation of each book
  # Postconditions: The book collection is not mutated
  def list_books(self):
    return '\n'.join(map(str, self.books))

  # Filters the collection such that all books that have a title that begins with the specified word are removed
  # Input: The word to filter by
  # Output: -
  # Postconditions: The books that match the specified condition are removed from the list
  def filter_books(self, word):
    def not_starts_with_word(s):
      return not s.title.startswith(word)

    self.undo.append(self.books)
    self.redo.clear()

    self.books = list(filter(not_starts_with_word, self.books))

  # Attempts to undo the last operation
  # Input: -
  # Output: -
  # Raises: Exception if there is nothing to undo
  # Postconditions: The book collection is restored to its state from before the last operation
  def try_undo(self):
    if not self.undo:
      raise Exception("Nothing to undo.")

    self.redo.append(self.books)
    self.books = self.undo.pop()

  # Attempts to redo the last undone operation
  # Input: -
  # Output: -
  # Raises: Exception if there is nothing to redo
  # Postconditions: The book collection is restored to its state from before the last undo
  def try_redo(self):
    if not self.redo:
      raise Exception("Nothing to redo.")

    self.undo.append(self.books)
    self.books = self.redo.pop()