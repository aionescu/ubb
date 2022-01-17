#!/usr/bin/env xi

let rec fibSlow (n: Num): Num =
  if n < 2
  then n
  else fibSlow (n - 1) + fibSlow (n - 2)
in

let fibFast (n: Num) =
  let rec go (a: Num) (b: Num) (c: Num): Num =
    if c == 0
    then a
    else go b (a + b) (c - 1)
  in
  go 0 1 n
in

let rec for (start: Num) (end: Num) (f: Num -> ()): () =
  if start > end
  then ()
  else (f start; for (start + 1) end f)
in

print "Slow";
for 0 30 (n: Num) -> print fibSlow n;

print "Fast";
for 0 30 (n: Num) -> print fibFast n
