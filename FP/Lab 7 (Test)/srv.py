import flight

# Function that adds a new flight to the list
# Input: list - The list of flights, fl - The flight to add
# Output: -
# Postconditions: Flight is added to the list if it is valid
# Raises: Exception if flight's duration is not a number or is less than 20, or
# if the flight'scode, departure or destination city has less than 3 characters.
def add(list, fl):
  if len(flight.code(fl)) < 3:
    raise Exception("Flight code must have at least 3 characters.")

  try:
    duration = int(flight.duration(fl))
  except:
    raise Exception("Duration must be a number.")

  flight.set_duration(fl, duration)

  if duration < 20:
    raise Exception("Duration must be at least 20 minutes.")

  if len(flight.departure(fl)) < 3:
    raise Exception("Departure city must have at least 3 characters.")

  if len(flight.destination(fl)) < 3:
    raise Exception("Destination city must have at least 3 characters.")

  list.append(fl)

# Function that deletes the flight with the specified code from the list
# Input: list - The list of flights, code - The code of the flight to remove
# Output: -
# Postconditions: The flight with the specified code is removed from the list
# Raises: Exception if the specified code is not in the list
def delete(list, code):
  for i in range(0, len(list)):
    if flight.code(list[i]) == code:
      del list[i]
      return

  raise Exception("Flight does not exist.")

# Function that returns a formatted string representation of all flights with the
# specified departure city.
# Input: list - The list of flights, departure_city - The departure city to filter by
# Output: A formatted string representation of all flights with the specified
# departure city
def show_all_departure(list, departure_city):
  def has_departure(fl):
    return flight.departure(fl) == departure_city

  return '\n'.join(map(flight.to_string, sorted(filter(has_departure, list), key = flight.destination)))

# Function that increases the duration of all flights with the specified departure city,
# by the specified amount
# Input: list - The list of flights, departure_city - The departure city of the flights
# to increase, inc_amount - The amount of time to increase by
# Output: -
# Postconditions: The time of all flights with the specified departure city is increased
# Raises: Exception if amount of time is not a number or if it is not in the range [10, 60]
def increase_time(list, departure_city, inc_amount):
  try:
    inc_amount = int(inc_amount)
  except:
    raise Exception("Amount of time must be a number.")

  if inc_amount < 10 or inc_amount > 60:
    raise Exception("Amount of time must be between 10 and 60 minutes.")

  for fl in list:
    if flight.departure(fl) == departure_city:
      flight.set_duration(fl, flight.duration(fl) + inc_amount)