def new(code, duration, departure, destination):
  return [code, duration, departure, destination]

def code(flight):
  return flight[0]

def duration(flight):
  return flight[1]

def departure(flight):
  return flight[2]

def destination(flight):
  return flight[3]

def set_duration(flight, duration):
  flight[1] = duration

def to_string(flight):
  return f"Flight {code(flight)}: From {departure(flight)} to {destination(flight)} in {duration(flight)}min"

def init_flights():
  return [
    new("0001", 20, "Cluj-Napoca", "Bucharest"),
    new("0002", 40, "Cluj-Napoca", "Budapest"),
    new("0003", 55, "Bucharest", "Paris"),
    new("0004", 75, "Paris", "New York"),
    new("0005", 50, "Hawaii", "Rome")
  ]