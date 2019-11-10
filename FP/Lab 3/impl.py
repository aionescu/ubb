from itertools import groupby
from expense import *

def is_category(category):
  return lambda e: get_category(e) == category

def negate(f):
  return lambda e: not f(e)

# Inserts an expense with the specified info
# Input: current expense list, day of new expense, amount of new expense, category of new expense
# Output: A new expense list with the new expense added
# Postconditions: The original list is not mutated
def insert(exps, day, amount, category):
  res = exps.copy()
  
  res.append(expense(day, amount, category))
  return res

# Removes all expenses between the specified days
# Input: current expense list, start of range, end of range
# Output: A new expense list with the expenses removed
# Postconditions: The original list is not mutated
def remove_range(exps, start, end):
  def in_range(e):
    day = get_day(e)
    return day >= start and day <= end

  return list(filter(negate(in_range), exps))

# Removes all expenses of the specified category
# Input: current expense list, category
# Output: A new expense list with the expenses removed
# Postconditions: The original list is not mutated
def remove_category(exps, category):
  return list(filter(negate(is_category(category)), exps))

def list_to_string(exps):
  return '\n'.join(map(to_string, exps))

# Returns a formatted list of all expenses
# Input: current expense list
# Output: The string representation of the expense list
# Postconditions: The original list is not mutated
def list_all(exps):
  return list_to_string(exps)

# Returns a formatted list of all expenses of the specified category
# Input: current expense list, category
# Output: The string representation of the specified expenses
# Postconditions: The original list is not mutated
def list_category(exps, category):
  return list_to_string(filter(is_category(category), exps))

# Returns a formatted list of all expenses of the specified category that satisfy the predicate
# Input: current expense list, category, predicate operator, predicate value
# Output: The string representation of the specified expenses
# Postconditions: The original list is not mutated
def list_predicate(exps, category, op, value):
  def predicate(e):
    return get_category(e) == category and op(get_amount(e), value)
  
  return list_to_string(filter(predicate, exps))

# Returns the sum of the amounts of all expenses of the specified category
# Input: current expense list, category
# Output: The sum of the specified expenses as an integer
# Postconditions: The original list is not mutated
def sum_category(exps, category):
  return sum(map(get_amount, filter(is_category(category), exps)))

def sorted_by(f, exps):
  return sorted(exps, key = f)

# Returns the day with the highest amount spent
# Input: current expense list
# Output: The day with the highest amount spent as an integer
# Postconditions: The original list is not mutated
def max_day(exps):
  def get_sum(x):
    return (x[0], sum(map(get_amount, x[1])))

  def snd(x):
    return x[1]

  grouped = groupby(sorted_by(get_day, exps), get_day)
  return max(map(get_sum, grouped), key = snd)[0]

# Returns a formatted and ordered list of all expenses on the specified day
# Input: current expense list, day
# Output: The string representation of the specified expenses
# Postconditions: The original list is not mutated
def sort_day(exps, day):
  def is_day(e):
    return get_day(e) == day

  return list_to_string(sorted_by(get_amount, filter(is_day, exps)))

# Returns a formatted and ordered list of all expenses of the specified category
# Input: current expense list, category
# Output: The string representation of the specified expenses
# Postconditions: The original list is not mutated
def sort_category(exps, category):
  return list_to_string(sorted_by(get_amount, filter(is_category(category), exps)))

# Removes all expenses that are NOT of the specified category
# Input: current expense list, category
# Output: List of expenses of the specified category
# Postconditions: The original list is not mutated
def filter_category(exps, category):
  return list(filter(is_category(category), exps))

# Removes all expenses that do not satisfy the specified predicate
# Input: current expense list, category, predicate operator, predicate value
# Output: List of expenses that satisfy the specified predicate
# Postconditions: The original list is not mutated
def filter_predicate(exps, category, op, value):
  def predicate(e):
    return get_category(e) == category and op(get_amount(e), value)
  
  return list(filter(predicate, exps))