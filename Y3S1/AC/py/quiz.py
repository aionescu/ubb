from datetime import datetime
from subprocess import run
from sys import argv
from time import sleep
from random import choices, randrange, uniform
import pandas as pd

key_cols = ["key", "duration", "is_mouse"]
run_cols = ["timestamp"]
quiz_cols = ["q1", "q2", "q3", "q4"]

def num_keys(keys):
  return len(list(filter(lambda x: not x, keys["is_mouse"])))

def num_clicks(keys):
  return len(list(filter(lambda x: x, keys["is_mouse"])))

def num_backspace(keys):
  return len(list(filter(lambda x: x in [8, 127], keys["key"])))

def avg_duration(keys):
  return sum(keys["duration"]) / len(keys)

def questions(quiz):
  return [
    quiz.iloc[0, 0],
    quiz.iloc[0, 1],
    quiz.iloc[0, 2],
    quiz.iloc[0, 3]
  ]

def data_row(keys, runs, quiz):
  cols = [
    num_keys(keys), avg_duration(keys),
    num_clicks(keys), num_backspace(keys),
    len(runs), *questions(quiz)
  ]

  return ",".join(map(str, cols)) + "\n"

def do_quiz():
  keys = pd.read_csv("~/Desktop/Study/Keys.csv", names = key_cols)
  runs = pd.read_csv("~/Desktop/Study/Runs.csv", names = run_cols)

  run(["./quiz-gui"])

  quiz = pd.read_csv("~/Desktop/Study/Quiz.csv", names = quiz_cols)

  with open("/home/oldpug/Desktop/Study/Stop", "w") as _:
    with open("/home/oldpug/Desktop/Study/Runs.csv", "w") as _:
      with open("/home/oldpug/Desktop/Study/Quiz.csv", "w") as _:
        pass

  row = data_row(keys, runs, quiz)

  with open("/home/oldpug/Desktop/Study/Data.csv", "a+") as f:
    f.write(row)

def main(loop):
  if not loop:
    do_quiz()
  else:
    while True:
      now = datetime.now().strftime("%H:%M:%S")
      print(f"{now} > Waiting 15min...")

      sleep(60 * 15)

      now = datetime.now().strftime("%H:%M:%S")
      print(f"{now} > Recording")

      do_quiz()

if __name__ == "__main__":
  main(len(argv) > 1 and argv[1] == "--loop")
