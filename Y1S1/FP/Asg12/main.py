import sys
from typing import Any, List, Iterable
from itertools import combinations

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

def next_prime(crr: int, sieve: List[bool]) -> int:
  i = crr + 1

  while not sieve[i]:
    i += 1

  return i

def pretty_print(l: List[int]) -> None:
  s = " + ".join(map(str, l))
  print(s)

def backtrack_recursive(n: int, sieve: List[bool], crr: List[int]) -> None:
  if sum(crr) == n:
    pretty_print(crr)
    return

  last = 0 if len(crr) == 0 else crr[-1]

  for j in primes_from_to(last + 1, n, sieve):
    crr.append(j)
    backtrack_recursive(n, sieve, crr)
    crr.pop()

def to_list(l: Any) -> List[int]:
  return list(l)

def subsets(s: List[int]) -> Iterable[List[int]]:
  for cardinality in range(len(s) + 1):
    yield from map(to_list, combinations(s, cardinality))

def backtrack_iterative(n: int, sieve: List[bool]) -> None:
  primes = list(primes_from_to(1, n, sieve))

  for subset in subsets(primes):
    if sum(subset) == n:
      pretty_print(subset)

def main():
  rec = sys.argv[1] == "--rec"
  n = int(sys.argv[2] if rec else sys.argv[1])
  siev = sieve(n)

  if rec:
    backtrack_recursive(n, siev, [])
  else:
    backtrack_iterative(n, siev)

if __name__ == "__main__":
  main()