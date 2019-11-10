from datetime import datetime
from random import randint

today = datetime.today()    

def expense(day, amount, category):
  return [day, amount, category]
  
def get_day(e):
  return e[0]

def get_amount(e):
  return e[1]

def get_category(e):
  return e[2]

def set_day(e, day):
  e[0] = day

def set_amount(e, amount):
  e[1] = amount

def set_category(e, category):
  e[2] = category

def to_string(e):
  return f"${get_amount(e)} for {get_category(e)} on {today.year}-{today.month}-{get_day(e)}"

categories = [
  "housekeeping",
  "food",
  "transport",
  "clothing",
  "internet",
  "others"
]

def rand_exps():
  exps = []

  for _ in range(0, 10):
    day = randint(1, today.day - 1)
    amount = randint(3, 20) * 5
    idx = randint(0, len(categories) - 1)

    exps.append(expense(day, amount, categories[idx]))

  return exps