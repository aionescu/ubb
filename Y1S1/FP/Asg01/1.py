if __name__ != "__main__":
  exit()
  
def get_int():
  return int(input())

def is_prime(n):
  for i in range(2, n - 1):
    if n % i == 0:
      return False

  return True

def next_prime(n):
  p = n + 1
  while not is_prime(p):
    p = p + 1

  return p

# main

print(next_prime(get_int()))