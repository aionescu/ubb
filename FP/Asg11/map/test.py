import unittest
from typing import List
from random import randint
from map import filter_by, gnome_sort, is_sorted, Map, shuffle

class DomainTestCase(unittest.TestCase):
  def test_gnome_sort(self):
    for i in range(1, 100):
      l = list(range(1, i + 1))

      shuffle(l)

      gnome_sort(l)
      assert is_sorted(l)

  def test_map_set(self):
    m = Map()

    for i in range(1, 100):
      k = randint(1, 100)
      v = randint(1, 100)

      m[k] = v
      assert m[k] == v

  def test_map_get(self):
    m = Map()

    for i in range(1, 100):
      m[i] = i + 1

    for i in range(1, 100):
      assert m[i] == i + 1

  def test_map_iter(self):
    m = Map()

    for i in range(1, 100):
      m[i] = i

    l = [i for i in range(1, 100)]
    ll = [(i, i) for i in range(1, 100)]

    assert list(m.keys) == l
    assert list(m.values) == l
    assert list(m.items) == ll

  def test_map_filter(self):
    m = Map()

    for i in range(1, 100):
      m[i] = i + 1

    def f(x):
      return x % 2 == 0

    def ff(p):
      x, y = p
      return f(x) and f(y)

    for i in m.filter_values(f):
      assert f(i)

    for i in m.filter_keys(f):
      assert f(i)

    for p in m.filter_items(ff):
      assert ff(p)

  def test_map_ctor(self):
    d = {}

    for i in range(1, 100):
      d[i] = i + 1

    m = Map(d)

    assert m.dict == d

    d[1000] = 1000

    assert m[1000] == 1000

  def test_filter_by(self):
    l = range(1, 1000)

    def even(x):
      return x % 2 == 0

    ll = filter_by(even, l)

    for e in ll:
      assert even(e)

if __name__ == "__main__":
  unittest.main()