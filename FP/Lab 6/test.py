import unittest
from domain import Movie, Client, Rental

class DomainTestCase(unittest.TestCase):
  def test_eq(self):
    client = Client(0, "Asdf")
    client2 = Client(1, "Asdff")
    assert client == client2
    self.assertEqual(client, client2)

unittest.main()