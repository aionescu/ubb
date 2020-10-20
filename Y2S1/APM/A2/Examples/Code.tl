a : Bool;
a <- True;

if a and True or False {
  print a
} else {
  print 2
};

b : Int;
b{-Nested {- multiline -}


  comment -}<- 2;
print b;

-- Single line comment
c : Int;
c <- -1;
print c;

d : Int;
d <- 1 + 2 * 3; -- Comment
print d;

-- If with no else branch
if True {
  print {- Multiline comment -} False
};

-- DeclAssign shorthand syntax
e : Int <- 2;
if e = 2 {
  print e
};
e <- e + 1;
print e
