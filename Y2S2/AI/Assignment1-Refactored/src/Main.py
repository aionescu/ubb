from sys import argv

from UI import *

def main():
  try:
    stepTime = int(argv[1])
  except:
    stepTime = 100

  ui = UI(stepTime)
  ui.run()

if __name__ == "__main__":
  main()
