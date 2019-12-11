from ui import Ui
from loader import *

def load_settings():
  with open("settings.properties", "r") as file:
    repo = file.readline().replace("repo=", "").replace("\n", "")
    clients = file.readline().replace("clients=", "").replace("\n", "")
    movies = file.readline().replace("movies=", "").replace("\n", "")
    rentals = file.readline().replace("rentals=", "").replace("\n", "")

    if repo == "mem":
      loader_type = MemLoader
      populate = True
    elif repo == "json":
      loader_type = JsonLoader
      populate = False
    elif repo == "pickle":
      loader_type = PickleLoader
      populate = False
    else:
      print("Invalid settings file.")
      exit()

    return (populate, loader_type, [clients, movies, rentals])

def main():
  (populate, loader_type, files) = load_settings()

  ui = Ui(loader_type, files, populate)

  while True:
    raw = input("rentals> ")
    cmd = raw.split(' ', 1)

    args = cmd[1] if len(cmd) > 1 else ""
    arg = list(map(lambda s: s.strip(), args.split(',')))

    ui.handle(cmd[0], arg)

if __name__ == "__main__":
  main()