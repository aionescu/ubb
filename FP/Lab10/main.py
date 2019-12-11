from ui import Ui
from storage import *

def load_settings():
  with open("settings.properties", "r") as file:
    repo = file.readline().replace("repo=", "").replace("\n", "")
    clients = file.readline().replace("clients=", "").replace("\n", "")
    movies = file.readline().replace("movies=", "").replace("\n", "")
    rentals = file.readline().replace("rentals=", "").replace("\n", "")

    if repo == "mem":
      storage_type = MemStorage
      populate = True
    elif repo == "json":
      storage_type = JsonStorage
      populate = False
    elif repo == "pickle":
      storage_type = PickleStorage
      populate = False
    else:
      print("Invalid settings file.")
      exit()

    return (populate, storage_type, [clients, movies, rentals])

def main():
  (populate, storage_type, files) = load_settings()

  ui = Ui(storage_type, files, populate)

  while True:
    raw = input("rentals> ")
    cmd = raw.split(' ', 1)

    args = cmd[1] if len(cmd) > 1 else ""
    arg = list(map(lambda s: s.strip(), args.split(',')))

    ui.handle(cmd[0], arg)

if __name__ == "__main__":
  main()