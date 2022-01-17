if __name__ != "__main__":
  exit()

def get_int():
  return int(input())

def is_prime(n):
  for crr in range(2, n - 1):
    if n % crr == 0:
      return False

  return True

# main

n = get_int()

if n == 1:
  print(1)
  exit()

crr = 1
count = 1

while count < n:
  crr = crr + 1
  for i in range(2, n + 1): 
    if is_prime(i) and crr % i == 0:
      count = count + 1

      if n == count:
        print(i)
        break