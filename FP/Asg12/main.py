from typing import List, Iterable

def sieve(n: int) -> List[bool]:
  p = [True for i in range(n + 1)]

  i = 2

  while i * i <= n:
    if p[i]:
      for j in range(i * 2, n + 1, i):
        p[j] = False

    i += 1

  return p

def primes_from_to(start: int, end: int, sieve: List[bool]) -> Iterable[int]:
  for i in range(start, end + 1):
    if sieve[i]:
      yield i

def pretty_print(l: List[int]) -> None:
  s = " + ".join(map(str, l))
  print(s)

def backtrack_recursive(n: int, sieve: List[bool], crr: List[int]) -> None:
  if sum(crr) == n:
    pretty_print(crr)
    return

  fst = 0 if len(crr) == 0 else crr[-1]

  for j in primes_from_to(fst + 1, n, sieve):
    crr.append(j)
    backtrack_recursive(n, sieve, crr)
    crr.pop()

def main():
  print("Please enter the value of N.")
  n = int(input())

  backtrack_recursive(n, sieve(n), [])

if __name__ == "__main__":
  main()