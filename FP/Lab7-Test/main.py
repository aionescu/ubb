import flight
import test
import ui

cmds = {
  "exit": ui.exit_,
  "add": ui.add,
  "delete": ui.delete,
  "show": ui.show_all_departure,
  "increase": ui.increase_time
}

def main():
  list = flight.init_flights()

  while True:
    cmd = input("flights> ")
    if cmd in cmds:
      cmds[cmd](list)
    else:
      print("Command not recognized.")

if __name__ == "__main__":
  test.run_all_tests()
  main()