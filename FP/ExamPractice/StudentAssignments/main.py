from ui import Ui

def main():
  ui = Ui()

  while True:
    cmd = input("students> ")
    ui.handle_cmd(cmd)

if __name__ == "__main__":
  main()