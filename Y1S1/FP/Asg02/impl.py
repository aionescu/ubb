import complex

def preload(l):
  nums = [
    "1 + 2i",
    "2 - 3i",
    "4",
    "5 - 5i",
    "4 + 3i",
    "i",
    "i",
    "5 - 5i",
    "-i",
    "100"
  ]

  l.extend(list(map(complex.parse, nums)))

def length(l):
  return len(l)

def clear(l):
  l.clear()

def append(l, c):
  l.append(c)

def extend(l, l2):
  l.extend(l2)

def pop(l):
  del l[-1]

# Function that computes the longest subsequence of the list where numbers have a strictly increasing real part
# Input: l - the list of numbers. Must not be modified
# Output: a list representing the longest subsequence with the specified property
def subseq1(l):
  ll = len(l)
  max_seq = []
  max_len = 0

  for i in range(0, ll):
    c = l[i]
    seq = [c]
    ln = 1

    for j in range(i + 1, ll):
      c2 = l[j]
      if complex.get_re(c2) <= complex.get_re(c):
        break

      seq.append(c2)
      ln += 1
      c = c2

    if ln > max_len:
      max_seq = seq
      max_len = ln

  return max_seq

# Function that computes the longest subsequence of the list that contains at most 3 distinct complex numbers
# Input: l - the list. Must not be modified
# Output: a list representing the longest subsequence with the specified property
def subseq2(l):
  ll = len(l)
  max_seq = []
  max_len = 0

  for i in range(0, ll):
    c = l[i]
    seq = [c]
    ln = 1
    uniques = [c]

    for j in range(i + 1, ll):
      c2 = l[j]

      if c2 not in uniques:
        if len(uniques) == 3:
          break

        uniques.append(c2)

      seq.append(c2)
      ln += 1

    if ln > max_len:
      max_seq = seq
      max_len = ln

  return max_seq