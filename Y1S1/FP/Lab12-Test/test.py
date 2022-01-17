import unittest
from datetime import datetime
from domain import *
from services import *

def len_l(l):
  return len(l.split('\n'))

addrs = "addresses.txt"
drvs = "drivers.txt"

class DomainTestCase(unittest.TestCase):
  def test_show_addr(self):
    srv = Services(addrs, drvs)
    assert len_l(srv.show_addresses()) == 6

  def test_show_drv(self):
    srv = Services(addrs, drvs)
    assert len_l(srv.show_drivers()) == 6

  def test_show_sorted(self):
    srv = Services(addrs, drvs)

    lst = srv.show_sorted(1).split("\n")
    assert lst[0] == "ion,10,21"
    assert lst[1] == "dan,5,10"
    assert lst[2] == "popa,1,4"
    assert lst[3] == "marius,40,60"
    assert lst[4] == "vasile,50,102"
    assert lst[5] == "ionut,100,100"

    lst = srv.show_sorted(2).split("\n")
    assert lst[0] == "vasile,50,102"
    assert lst[1] == "ionut,100,100"
    assert lst[2] == "marius,40,60"
    assert lst[3] == "ion,10,21"
    assert lst[4] == "dan,5,10"
    assert lst[5] == "popa,1,4"

    lst = srv.show_sorted(6).split("\n")
    assert lst[0] == "ionut,100,100"
    assert lst[1] == "vasile,50,102"
    assert lst[2] == "marius,40,60"
    assert lst[3] == "ion,10,21"
    assert lst[4] == "dan,5,10"
    assert lst[5] == "popa,1,4"

    lst = srv.show_sorted(3).split("\n")
    assert lst[0] == "marius,40,60"
    assert lst[1] == "ion,10,21"
    assert lst[2] == "vasile,50,102"
    assert lst[3] == "dan,5,10"
    assert lst[4] == "popa,1,4"
    assert lst[5] == "ionut,100,100"

if __name__ == "__main__":
  unittest.main()