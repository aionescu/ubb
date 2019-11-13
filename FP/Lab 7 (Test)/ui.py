import flight
import srv

def exit_(list):
  exit()

def add(list):
  code = input("Code: ")
  duration = input("Duration: ")
  departure = input("Departure city: ")
  destination = input("Destination city: ")

  fl = flight.new(code, duration, departure, destination)

  try:
    srv.add(list, fl)
  except Exception as ex:
    print(str(ex))

def delete(list):
  code = input("Code to delete: ")

  try:
    srv.delete(list, code)
  except Exception as ex:
    print(str(ex))

def show_all_departure(list):
  departure = input("Departure city to show: ")
  print(srv.show_all_departure(list, departure))

def increase_time(list):
  departure = input("Departure city: ")
  amount = input("Amount of time: ")

  try:
    srv.increase_time(list, departure, amount)
  except Exception as ex:
    print(str(ex))