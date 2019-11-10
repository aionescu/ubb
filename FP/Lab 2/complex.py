# class Complex:
#   def __init__(self, re, im):
#     self.re = re
#     self.im = im

#   # Python uses reference equality by default, we need structural equality
#   def __eq__(self, rhs):
#     if rhs is None:
#       return False

#     return self.re == rhs.re and self.im == rhs.im

def get_re(c):
  # return c.real # Built-in class
  # return c.re # Class
  # return c["re"] # Dict
  return c[0] # Tuple & list

def get_im(c):
  # return c.imag
  # return c.im
  # return c["im"]
  return c[1]

def set_re(c, re):
  # c.real = re
  # c.re = re
  # c["re"] = re
  c[0] = re

def set_im(c, im):
  # c.imag = im
  # c.im = im
  # c["im"] = im
  c[1] = im

def new(re, im):
  # return complex(re, im)
  # return Complex(re, im)
  # return { "re": re, "im": im }
  return [re, im]
  # return (re, im)

def of_re(re):
  return new(re, 0)

def of_im(im):
  return new(0, im)

def to_string(c, eng_notation = False):
  re = get_re(c)
  im = get_im(c)
  letter = "j" if eng_notation else "i"

  if re == 0:
    if im == 0:
      return "0"
    elif im == 1:
      return letter
    elif im == -1:
      return "-" + letter
    else:
      return str(im) + letter

  if im < 0:
    return str(re) + " - " + str(-im) + letter

  if im == 0:
    return str(re)

  return str(get_re(c)) + " + " + str(im) + letter

def get_int_substr(s):
  ss = ""
  l = 0

  for c in s:
    if not c in "0123456789":
      return ss, s[l:]

    ss += c
    l += 1

  return ss, s[l:]

def parse_int(s):
  os = s
  s = s.lstrip()
  sign = 1

  if s[0] in "-+":
    sign = -1 if s[0] == '-' else 1
    s = s[1:]

  s, rest = get_int_substr(s.lstrip())

  if not s:
    return None, os
  else:
    return sign * int(s), rest

def parse(s):
  s = s.rstrip()

  re, s = parse_int(s.lstrip())
  s = s.lstrip()

  if re is None:
    if s.startswith("-"):
      if s[1:].lstrip() in ["i", "j"]:
        return of_im(-1)

    return of_im(1) if s in ["i", "j"] else None
  if not s:
    return of_re(re)

  sign = 1

  if s[0] in "-+":
    sign = -1 if s[0] == '-' else 1
    s = s[1:].lstrip()
  elif s[0] in "ij":
    return of_im(re)
  else:
    return None

  im, s = parse_int(s)

  if im is None:
    return None
  else:
    im *= sign

  s = s.lstrip()

  if s in ["i", "j"]:
    return new(re, im)
  else:
    return None