from book import Book
from services import Services

def run_all_tests(print_success = False):
  vars = globals()

  for name, var in vars.items():
    if name.startswith("test_") and callable(var):
      var()

  if print_success:
    print("All tests passed.")

def test_add_book():
  srv = Services(False)
  book = Book("isbn1", "author1", "title1")

  assert srv.try_add_book(book)
  assert not srv.try_add_book(book)

def test_filter():
  srv = Services(False)
  srv.try_add_book(Book("isbn1", "author1", "title 1"))
  srv.try_add_book(Book("isbn2", "author2", "title 2"))
  srv.try_add_book(Book("isbn3", "author2", "not title 2"))

  srv.filter_books("title")
  assert srv.books == [Book("isbn3", "author2", "not title 2")]

def test_undo_redo():
  srv = Services(False)

  srv.try_add_book(Book("isbn1", "author1", "title1"))
  srv.try_add_book(Book("isbn2", "author1", "title1"))

  assert srv.try_undo()
  assert srv.books == [Book("isbn1", "author1", "title1")]

  assert srv.try_undo()
  assert srv.books == []

  assert not srv.try_undo()

  assert srv.try_redo()
  assert srv.books == [Book("isbn1", "author1", "title1")]

  assert srv.try_redo()
  assert len(srv.books) == 2

  assert not srv.try_redo()