from expense import *
from impl import *

def run_all_tests(print_success = False):
  vars = globals()

  for name, var in vars.items():
    if name.startswith("test_") and callable(var):
      var()

  if print_success:
    print("All tests passed.")

def test_rand_exps():
  assert len(rand_exps()) == 10

def test_insert():
  exps = []

  exps = insert(exps, today.day, 20, "food")
  assert exps == [expense(today.day, 20, "food")]

  exps = insert(exps, 5, 30, "internet")
  assert exps == [expense(today.day, 20, "food"), expense(5, 30, "internet")]

def test_remove_range():
  exps = [expense(today.day, 20, "food"), expense(5, 30, "internet")]
  assert remove_range(exps, 20, 30) == [exps.pop()]

def test_remove_category():
  exps = [expense(today.day, 20, "food"), expense(5, 30, "internet")]
  assert remove_category(exps, "food") == [exps.pop()]

def test_max_day():
  exps = [expense(today.day, 20, "food"), expense(5, 30, "internet")]
  assert max_day(exps) == 5

def test_sum_category():
  exps = [expense(today.day, 20, "food"), expense(5, 30, "internet"), expense(5, 30, "internet"), expense(5, 20, "internet"), expense(5, 15, "internet")]
  assert sum_category(exps, "internet") == 30 + 30 + 20 + 15

def test_filter_category():
  exps = [expense(today.day, 20, "food"), expense(5, 30, "internet")]
  assert filter_category(exps, "internet") == [exps.pop()]