from ui import Ui

# TODO: Fix diag1 and vertical win conditions not working
# TODO: Add tests & specs

def main() -> None:
  ui = Ui()
  
  while True:
    ui.print()
    ui.advance_turn()

if __name__ == "__main__":
  main()