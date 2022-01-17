#!/usr/bin/env xi

let rec fac (n: Num): Num =
  if n == 0
  then 1
  else n * fac (n - 1)
in

print fac 5;
print fac (fac 5)
