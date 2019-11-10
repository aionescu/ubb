if __name__ != "__main__":
  exit()
  
def get_string():
  return input()
  
def get_digits(n):
  return set(n)

def have_p(a, b):
  return get_digits(a) == get_digits(b)

# main

a = get_string()
b = get_string()

print(have_p(a, b))