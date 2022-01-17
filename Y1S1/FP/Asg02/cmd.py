import complex
import impl

def to_kebab(s):
  return s.replace("_", "-")

def to_snake(s):
  return s.replace("-", "_")

def add_prefix(s):
  return "cmd_" + s

def remove_prefix(s):
  return s[4:]

def to_ui_name(s):
  return to_kebab(remove_prefix(s))

def to_var_name(s):
  return add_prefix(to_snake(s))

def get_fn(s):
  vars = globals()
  cmd = to_var_name(s)

  if cmd in vars:
    fn = vars[cmd]
    if callable(fn):
      return fn

  return None

def run_cmd(s, l):
  fn = get_fn(s)
  if fn is not None:
    fn(l)
  else:
    print("Command not recognized. To see a list of available commands, use the `help` command.")

def cmd_exit(_):
  """Exits the application."""
  exit()

def cmd_help(_):
  """Displays a list of available commands."""

  vars = globals()

  print("Available commands:")

  for name, var in vars.items():
    if name.startswith("cmd_") and callable(var):
      print("  " + to_ui_name(name) + ": " + var.__doc__)

def list_to_string(l):
  # return str(list(map(complex.to_string, l)))

  return "[" + ", ".join(map(complex.to_string, l)) + "]"

def cmd_print_list(l):
  """Prints the list to the console."""
  print(list_to_string(l))

def cmd_clear_list(l):
  """Clears the list."""
  impl.clear(l)

def read_list(l):
  idx = impl.length(l)

  while True:
    s = input(f"[{idx}]: ")
    if not s:
      break

    c = complex.parse(s)

    if c is None:
      print("Invalid complex number.")
    else:
      l.append(c)
      idx += 1

def cmd_preload_list(l):
  """Loads a predefined list of complex numbers."""
  impl.clear(l)
  impl.preload(l)

def cmd_read_list(l):
  """Reads new values and overrides the existing list with them."""
  impl.clear(l)
  read_list(l)
  pass

def cmd_append_list(l):
  """Reads new values and appends them to the list."""
  read_list(l)
  pass

def cmd_pop_list(l):
  """Removes the last element added to the list."""
  impl.pop(l)

def cmd_subseq1(l):
  """Displays the largest sequence consisting of numbers with a strictly increasing real part."""
  seq = impl.subseq1(l)
  print(list_to_string(seq))

def cmd_subseq2(l):
  """Displays the largest sequence containing at most 3 distinct values."""
  seq = impl.subseq2(l)
  print(list_to_string(seq))