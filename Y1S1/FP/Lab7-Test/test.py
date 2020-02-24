import flight
import srv

def test_flight_new():
  fl = flight.new("abc", 20, "dep", "dest")

  assert flight.code(fl) == "abc"
  assert flight.duration(fl) == 20
  assert flight.departure(fl) == "dep"
  assert flight.destination(fl) == "dest"

def test_flight_set():
  fl = flight.new("abc", 20, "dep", "dest")
  assert flight.duration(fl) == 20

  flight.set_duration(fl, 40)
  assert flight.duration(fl) == 40

def test_srv_add():
  fl = flight.new("abc", 20, "dep", "dest")
  list = []

  srv.add(list, fl)
  assert list == [fl]

  srv.add(list, fl)
  assert list == [fl, fl]

  fl = flight.new("a", 20, "dep", "aaa")

  try:
    srv.add(list, fl)
    assert False
  except:
    pass

def test_srv_delete():
  list = flight.init_flights()

  srv.delete(list, "0001")
  assert list == flight.init_flights()[1:]

  try:
    srv.delete(list, "0000")
    assert False
  except:
    pass

  srv.delete(list, "0005")
  assert list == flight.init_flights()[1:-1]

def test_srv_show_all_departure():
  list = flight.init_flights()
  assert srv.show_all_departure(list, "Cluj-Napoca") == srv.show_all_departure(list, "Cluj-Napoca")
  assert srv.show_all_departure(list, "Hawaii") == "Flight 0005: From Hawaii to Rome in 50min"

def test_srv_increase_time():
  list = flight.init_flights()

  try:
    srv.increase_time(list, "Cluj-Napoca", 5)
    assert False
  except:
    pass

  assert flight.duration(list[0]) == 20
  assert flight.duration(list[1]) == 40
  assert flight.duration(list[2]) == 55

  srv.increase_time(list, "Cluj-Napoca", 20)

  assert flight.duration(list[0]) == 40
  assert flight.duration(list[1]) == 60
  assert flight.duration(list[2]) == 55

def run_all_tests():
  test_flight_new()
  test_flight_set()

  test_srv_add()
  test_srv_delete()
  test_srv_show_all_departure()
  test_srv_increase_time()

  print("All tests passed.")