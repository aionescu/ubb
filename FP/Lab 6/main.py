from ui import *

def main() -> None:
  ui = Ui(True)

  while True:
    raw = input("rentals> ")
    cmd = raw.split(' ', 1)

    args = cmd[1] if len(cmd) > 1 else ""
    arg = list(map(lambda s: s.strip(), args.split(',')))

    ui.handle(cmd[0], arg)

if __name__ == "__main__":
  main()

# TODO: Fix stats-m (?)
# TODO: Cascade delete/undo/redo: e.g. if a client is deleted, their rentals are also deleted