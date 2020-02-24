import unittest
from utils import nub, length, all_f
from board import Cell, Board
from game import Game

class DomainTestCase(unittest.TestCase):
  def test_nub(self) -> None:
    l = []
    for i in range(0, 10):
      l.append(i)
      l.append(i)

      assert list(nub(l)) == list(nub(nub(l)))

  def test_length(self) -> None:
    l = []
    for i in range(1, 10):
      l.append(i)

      assert length(l) == i

  def test_all_f(self) -> None:
    l = [i for i in range(0, 10)]

    def true(x: int) -> bool:
      return True

    def false(x: int) -> bool:
      return False

    assert all_f(true, l) == True
    assert all_f(false, l) == False

  def test_flip(self) -> None:
    cell = Cell.P1
    assert cell.flip == Cell.P2
    assert cell.flip.flip == cell

  def test_dims(self) -> None:
    board = Board()

    assert board.height == 6
    assert board.width == 7

  def test_is_in(self) -> None:
    board = Board()

    assert not board.is_in((-1, -1))
    assert board.is_in((0, 0))

    for i in range(1, 5):
      for j in range(1, 5):
        assert board.is_in((i, j))

  def test_get_set(self) -> None:
    board = Board()

    for i in range(0, 6):
      for j in range(0, 6):
        p = (i, j)
        cell = board.get(p)

        board.set(p, cell)
        assert board.get(p) == cell

  def test_make_move(self) -> None:
    game = Game()

    assert not game.make_move(-1)

  def test_ended(self) -> None:
    game = Game()

    assert not game.ended()

if __name__ == "__main__":
  unittest.main()