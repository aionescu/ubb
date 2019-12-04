import complex
import impl

def run_all_tests(print_success = False):
  vars = globals()

  for name, var in vars.items():
    if name.startswith("test_") and callable(var):
      var()

  if print_success:
    print("All tests passed.")

def test_complex_ctors():
  assert complex.new(1, 1) != None # Sanity check
  assert complex.new(1, 1) == complex.new(1, 1) # Structural equality
  assert complex.of_re(2) == complex.new(2, 0)
  assert complex.of_im(3) == complex.new(0, 3)

def test_complex_parse():
  assert complex.parse("1 + 2i") == complex.new(1, 2)
  assert complex.parse("1 + 2j") == complex.new(1, 2)

  assert complex.parse("1") == complex.of_re(1)

  assert complex.parse("2i") == complex.of_im(2)
  assert complex.parse("2j") == complex.of_im(2)

  assert complex.parse("-2j") == complex.of_im(-2)

  assert complex.parse("i") == complex.of_im(1)
  assert complex.parse("j") == complex.of_im(1)

  assert complex.parse("1 + -2i") == complex.new(1, -2)
  assert complex.parse("1 - 2i") == complex.new(1, -2)

  assert complex.parse("  1  +  -  3  j  ") == complex.new(1, -3)

  assert complex.parse("0 + 0j") == complex.new(0, 0)

def test_complex_to_string():
  assert complex.to_string(complex.new(1, 2)) == "1 + 2i"
  assert complex.to_string(complex.new(1, 2), True) == "1 + 2j"

  assert complex.to_string(complex.new(1, -2)) == "1 - 2i"

  assert complex.to_string(complex.of_re(4)) == "4"
  assert complex.to_string(complex.of_im(2)) == "2i"

  assert complex.to_string(complex.of_im(1)) == "i"
  assert complex.to_string(complex.of_im(-1), True) == "-j"

  assert complex.to_string(complex.of_im(-1)) == "-i"

def of_ints(ints):
  return list(map(complex.of_re, ints))

def test_subseq1():
  assert impl.subseq1(of_ints([1, 2, 3, 1, 2, 3, 4, 1, 2])) == of_ints([1, 2, 3, 4])

  l = [
    complex.new(0, -1),
    complex.new(1, 0),
    complex.new(-5, 10),
    complex.new(-4, 9),
    complex.new(-3, 8),
    complex.new(-2, 7),
    complex.new(-1, 6),
    complex.new(-2, 7)
  ]

  assert impl.subseq1(l) == l[2:-1]

  l = []
  for n in range(1, 6):
    assert impl.subseq1(l) == l
    l.append(complex.of_re(n))
    
def test_subseq2():
  l = of_ints([1, 2, 3, 3, 3, 3, 3, 3, 4, 5, 5, 5, 5, 4, 3, 2, 1, 1, 1, 0])
  assert impl.subseq2(l) == l[2:15]

  l = of_ints([1, 2])
  assert impl.subseq2(l) == l

  l = []
  for _ in range(1, 6):
    assert impl.subseq2(l) == l
    l.append(complex.of_re(1))