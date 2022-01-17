from ui import UI

def main() -> None:
  ui = UI()

  while True:
    ui.handleCommand()

if __name__ == "__main__":
  main()