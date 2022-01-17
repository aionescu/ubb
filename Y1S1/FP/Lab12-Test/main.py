from ui import *

def main():
  ui = Ui("addresses.txt", "drivers.txt")

  while True:
    cmd = input("taxis> ")
    cmd = cmd.split(" ")
    if len(cmd) == 1:
      cmd = [cmd[0], ""]

    ui.handle(cmd[0], cmd[1])

if __name__ == "__main__":
  main()