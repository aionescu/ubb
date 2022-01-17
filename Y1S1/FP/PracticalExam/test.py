import unittest
from exn import AlreadyHitError, HitEarthError, HitAsteroidError
from game import Game

class DomainTestCase(unittest.TestCase):
  def test_parse_coord(self):
    g = Game()

    self.assertEqual(g.parse_coord("A1"), (0, 0))
    self.assertEqual(g.parse_coord("A2"), (1, 0))
    self.assertEqual(g.parse_coord("B2"), (1, 1))

    self.assertEqual(g.parse_coord(""), None)
    self.assertEqual(g.parse_coord("AA"), None)
    self.assertEqual(g.parse_coord("A13"), None)

    self.assertEqual(g.parse_coord("Z1"), None)
    self.assertEqual(g.parse_coord("A9"), None)

  def test_fire(self):
    g = Game()

    for asteroid in g.all_asteroids():
      with self.assertRaises(HitAsteroidError):
        g.fire_exn(asteroid)

    with self.assertRaises(HitEarthError):
      g.fire_exn((3, 3))

    empty = list(g.all_empty())[0]

    self.assertFalse(g.fire_exn(empty))

    with self.assertRaises(AlreadyHitError):
      g.fire_exn(empty)

    for ship in g.all_ships():
      self.assertTrue(g.fire_exn(ship))

      with self.assertRaises(AlreadyHitError):
        g.fire_exn(ship)

if __name__ == "__main__":
  unittest.main()