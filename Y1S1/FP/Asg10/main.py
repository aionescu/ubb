from ui import Ui

def main() -> None:
  ui = Ui()
  
  while True:
    ui.print()
    ui.advance_turn()

if __name__ == "__main__":
  main()