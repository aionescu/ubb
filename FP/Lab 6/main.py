from ui import *

def main() -> None:
  ui = Ui()

  while True:
    cmd = input("rentals> ")
    ui.handle(cmd.split())

if __name__ == "__main__":
  main()

# TODO: Add tests and specs
# TODO: Add exn handling