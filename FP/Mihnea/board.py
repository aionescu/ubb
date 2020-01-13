UP = 0
RIGHT = 1
DOWN = 2
LEFT = 3

def create_plane(head, orientation):
  x, y = head

  if orientation == UP:
    return            [head,
      (x + 1, y - 1), (x + 1, y), (x + 1, y + 1),
                      (x + 2, y),
      (x + 3, y - 1), (x + 3, y), (x + 3, y + 1)]

  if orientation == RIGHT:
    return [(x - 1, y - 3),            (x - 1, y - 1),
            (x, y - 3),   (x, y - 2),  (x, y - 1),     head,
            (x + 1, y - 3),            (x + 1, y - 1)]

  if orientation == LEFT:
    return [(x - 1, y + 1),            (x - 1, y + 3),
       head, (x, y + 1),   (x, y + 2), (x, y + 3),
            (x + 1, y + 1),            (x + 1, y + 3)]

  if orientation == DOWN:
    return [(x - 3, y - 1), (x - 3, y), (x - 3, y + 1),
                            (x - 2, y),
            (x - 1, y - 1), (x - 1, y), (x - 1, y + 1),
                            head]

  raise ValueError()

EMPTY = 0
PLANE_TAIL = 1
PLANE_HEAD = 2

class Board:
  def __init__(self, width, height):
    self.__width = width
    self.__height = height
    self.__board = [[EMPTY for y in range(self.__width)] for x in range(self.__height)]

  @property
  def width(self):
    return self.__width
  
  @property
  def height(self):
    return self.__height

  def is_in(self, p):
    x, y = p
    return (x >= 0 and x < self.__height
      and y >= 0 and y < self.__width)

  def get(self, p):
    x, y = p
    return self.__board[x][y]

  def set(self, p, val):
    x, y = p
    self.__board[x][y] = val

  def speculate(self):
    return Board(self.width, self.height)

  def try_place_plane(self, head, orientation):
    plane = create_plane(head, orientation)

    all_is_in = all(map(self.is_in, plane))

    if not all_is_in:
      return False

    def f(p):
      return self.get(p) == EMPTY

    all_is_empty = all(map(f, plane))

    if not all_is_empty:
      return False

    def g(p):
      self.set(p, PLANE_TAIL)

    map(g, plane)

    self.set(head, PLANE_HEAD)
    return True

  def try_hit(self, p):
    return self.get(p)

  def __str__(self):
    header = " ".join(map(str, range(1, self.__width + 1)))
    buf = "  " + header + "\n"
    letter = "A"

    def inc_char(c):
      return chr(ord(c) + 1)

    def to_string(cell):
      if cell == EMPTY:
        return "."

      if cell == PLANE_TAIL:
        return "#"

      return "*"

    for row in self.__board:
      buf += letter + " "
      letter = inc_char(letter)

      for cell in row:
        buf += to_string(cell) + " "

      buf += "\n"

    return buf