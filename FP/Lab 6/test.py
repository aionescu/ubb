import unittest
from datetime import datetime
from domain import *
from services import *

def len_l(l):
  return len(l.split('\n'))

class DomainTestCase(unittest.TestCase):
  def test_list_c(self):
    srv = Services()
    assert len_l(srv.list_clients()) == 10

  def test_list_m(self):
    srv = Services()
    assert len_l(srv.list_movies()) == 10

  def test_list_r(self):
    srv = Services()
    assert len_l(srv.list_rentals()) == 10

  def test_add_c(self):
    srv = Services()

    for i in range(1, 10):
      srv.add_client(f"Client {i}")
      assert len_l(srv.list_clients()) == 10 + i

  def test_add_m(self):
    srv = Services()

    for i in range(1, 10):
      srv.add_movie(f"Movie {i}", f"Desc {i}", f"Genre {i}")
      assert len_l(srv.list_movies()) == 10 + i

  def test_update_c(self):
    srv = Services()

    for i in range(1, 10):
      srv.update_client(i, f"Client {i} {i}")
      assert len_l(srv.list_clients()) == 10

    try:
      srv.update_client(-1, "")
      assert False
    except InexistentItemError:
      pass

  def test_update_m(self):
    srv = Services()

    for i in range(1, 10):
      srv.update_movie(i, f"Movie {i} {i}", f"Desc {i} {i}", f"Genre {i} {i}")
      assert len_l(srv.list_movies()) == 10

    try:
      srv.update_movie(-1, "", "", "")
      assert False
    except InexistentItemError:
      pass

  def test_remove_c(self):
    srv = Services()

    for i in range(1, 11):
      srv.remove_client(i)

    try:
      srv.remove_client(-1)
      assert False
    except InexistentItemError:
      pass

    assert len_l(srv.list_clients()) == 1

  def test_remove_m(self):
    srv = Services()

    for i in range(1, 11):
      srv.remove_movie(i)

    try:
      srv.remove_movie(-1)
      assert False
    except InexistentItemError:
      pass

    assert len_l(srv.list_movies()) == 1

  def test_rent(self):
    srv = Services()
    today = datetime.today()

    for i in range(1, 11):
      try:
        srv.rent_movie(i, i, today, today)
        assert False
      except InvalidRentalException:
        pass

if __name__ == "__main__":
  unittest.main()