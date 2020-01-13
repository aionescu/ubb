from typing import List

def primes(n: int) -> List[bool]:
  p = [True for i in range(n + 1)]

  i = 2

  while i * i <= n:
    if p[i]:
      for j in range(i * 2, n + 1, i):
        p[j] = False

    i += 1

  return p

def backtrack_iterative(n: int) -> List[List[int]]:
  def go(n: int, primes: List[bool]) -> List[List[int]]:
    for i in range(1, n + 1):
      
    pass

  p = primes(n)
  return go(n, p)