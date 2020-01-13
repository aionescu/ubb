from ui import Ui

def main():
  ui = Ui(8, 8)
  ui.initialize()

  while True:
    ui.print()
    ui.make_move()
    ui.check_win()

if __name__ == "__main__":
  main()