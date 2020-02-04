from ui import Ui
from loader import *
from exn import *
from settings import *

def main():
  try:
    settings = Settings("settings.properties")
  except InvalidSettingsError:
    print("Settings file has incorrect format.")
    exit()

  ui = Ui(settings)

  while True:
    raw = input("rentals> ")
    cmd = raw.split(' ', 1)

    args = cmd[1] if len(cmd) > 1 else ""
    arg = list(map(lambda s: s.strip(), args.split(',')))

    ui.handle(cmd[0], arg)

if __name__ == "__main__":
  main()