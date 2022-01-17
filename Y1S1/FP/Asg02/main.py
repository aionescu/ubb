# Assignments 1 & 2

if __name__ != "__main__":
  exit()

import test
import cmd

def main():
  list = []

  while True:
    str = input("complex> ")
    cmd.run_cmd(str, list)

test.run_all_tests(True)
main()