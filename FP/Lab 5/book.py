from random import randint

class Book:
  def __init__(self, isbn, author, title):
    self.isbn = isbn
    self.author = author
    self.title = title

  def __eq__(self, other):
    return self.isbn == other.isbn and self.author == other.author and self.title == other.title
    
  def __str__(self):
    return f"{self.title} by {self.author} [ISBN: {self.isbn}]"

author_firsts = ["Richard", "Christie", "Chris"]
author_lasts = ["Knaak", "Golden", "Metzen"]

book_nouns1 = ["Wrath", "Secrets", "Mystery", "Game"]
book_nouns2 = ["Mushroom", "Librarian", "Programmer", "Bird"]
book_fmt1 = "%s of the %s"
book_fmt2 = "Of %ss and %ss"
book_fmt3 = "The %s"

def rand_author():
  first_idx = randint(0, len(author_firsts) - 1)
  last_idx = randint(0, len(author_lasts) - 1)

  return author_firsts[first_idx] + " " + author_lasts[last_idx]

def rand_book():
  fmt = randint(1, 3)

  if fmt == 1:
    idx1 = randint(0, 3)
    idx2 = randint(0, 3)

    return book_fmt1 % (book_nouns1[idx1], book_nouns2[idx2])

  elif fmt == 2:
    idx1 = randint(0, 3)
    idx2 = randint(0, 3)

    while idx1 == idx2:
      idx2 = randint(0, 3)

    return book_fmt2 % (book_nouns2[idx1], book_nouns2[idx2])
    
  else:
    idx1 = randint(0, 3)
    return book_fmt3 % book_nouns2[idx1]

def rand_books():
  books = []

  for i in range(1, 11):
    book = Book("isbn" + str(i), rand_author(), rand_book())
    books.append(book)

  return books