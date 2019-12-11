from book import Book
from services import Services

class Ui:
  def __init__(self):
    self.srv = Services()
    self.cmds = {
      "exit": exit,
      "add": self.add_book,
      "list": self.list_books,
      "filter": self.filter_books,
      "undo": self.undo,
      "redo": self.redo
    }

  def add_book(self):
    isbn = input("ISBN: ")
    author = input("Author: ")
    title = input("Title: ")

    book = Book(isbn, author, title)

    if not self.srv.try_add_book(book):
      print("Book already exists.")

  def list_books(self):
    print(self.srv.list_books())

  def filter_books(self):
    word = input("Word to filter out: ")
    self.srv.filter_books(word)

  def undo(self):
    if not self.srv.try_undo():
      print("Nothing to undo.")

  def redo(self):
    if not self.srv.try_redo():
      print("Nothing to redo.")

  def handle_input(self):
    cmd = input("books> ")

    if cmd in self.cmds:
      self.cmds[cmd]()
    else:
      print("Command not recognized.")