---
marp: true
paginate: true
theme: uncover
---

<!-- class: invert -->

![bg left:50% 80%](Assets/Haskell.svg)

# **Haskell**

A quick introduction

<br/>

*Presented by Alex Ionescu*
[github.com/aionescu](https://github.com/aionescu)

---

# **What is Haskell?**

As per [haskell.org](https://haskell.org), Haskell is an *advanced, purely functional programming language*.

**But what does that actually mean?**

---

# **Haskell's Core Concepts**

- Expressive type system
- Purity
- Laziness

---

# **First, The Basics**

```haskell
f :: Int -> Int
f x = x + 1
```

```haskell
factorial :: Int -> Int
factorial n = product [1 .. n]
```

```haskell
min :: Int -> Int -> Int
min x y =
  if x < y
  then x
  else y
```

---

# **Haskell's Type System**

In Haskell, types only model data, while functions only model behavior.

Haskell supports *algebraic data types*. That is, types are composed from other types by using **AND**s and **OR**s.

---

# **Records (≈ Structs)**

```haskell
data Person = Person { name :: String, age :: Int }

-- Field accessors
name :: Person -> String
age :: Person -> Int

-- Construction
p = Person { name = "Alex", age = 20 }
p2 = Person "Alex" 20

n = name p2 -- "Alex"

-- Record update syntax
p3 = p { age = 21 }
p4 = Person { name = name p, age = 21 }
```

---

# **Unions (≈ Enums)**

```haskell
data Color = Red | Green | Blue

data Shape
  = Circle Double
  | Square Double
  | Triangle Double Double Double
  | Rectangle Double Double

s1 = Circle 2
s2 = Square 3.5

data List a
  = Nil
  | Cons a (List a)

l1 = Cons 2 (Cons 3 (Cons 4 Nil))
```

---

# **Pattern Matching**

```haskell
data Shape
  = Circle Double
  | Square Double
  | Triangle Double Double Double
  | Rectangle Double Double

perimeter :: Shape -> Double
perimeter (Circle r) = 2 * pi * r
perimeter (Square l) = 4 * l
perimeter (Triangle a b c) = a + b + c
perimeter (Rectangle w h) = 2 * (w + h)
```

```haskell
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

---

# **Type Classes (≈ Interfaces)**

```haskell
-- Same definition as above
data Person = Person { name :: String, age :: Int }

-- Defined in the standard library
class Show a where
  show :: a -> String

instance Show Person where
  show p = name p

greet :: Show a => a -> String
greet a = "Hello, " ++ show a ++ "!"
```

```java
// Java equivalent
<A extends Show> String greet(A a);
```

---

# **Automatic Deriving**

```haskell
data Person = Person { name :: String, age :: Int }
  deriving (Show, Eq, Ord)

p = Person { name = "Alex", age = 20 }

show p -- Person {name = "Alex", age = 20}
```

---

# **Safety Through Types**

Haskell has no `null`s or exceptions, everything is expressed through types.

```haskell
data Maybe a
  = Nothing
  | Just a

data Either a b
  = Left a
  | Right b

head :: [Int] -> Maybe Int
saveDiv :: Int -> Int -> Either DivByZero Int
```

---

# **Purity & Side Effects**

Haskell differentiates between code that performs *pure computations* and code that performs *side effects*.

---

# **The IO Type**

```cpp
int f(int x) {
  return x + 1;
}

```

```haskell
f :: Int -> Int
f x = x + 1


```

---

# **The IO Type**

```cpp
int f(int x) {
  std::cout << x;
  return x + 1;
}
```

```haskell
f :: Int -> IO Int
f x = do
  print x
  return (x + 1)
```

---

# **Why bother?**

- Allows you to reason about your code easier
- Allows the compiler to optimize aggressively

---

# **Custom Effects: Monads**

```haskell
class Monad m where
  return :: a -> m a
  ...
```

You can think of `Monad m` as `IEffect<M>`

---

# **Example: DB Effect**

```haskell
data DB a = ...

instance Monad DB where
  return a = DB ...

getAllStudents :: DB [Student]
getAllStudents = ...

inGroup :: Int -> Student -> Bool
inGroup group student = getGroup student == group

getByGroup :: Int -> DB [Student]
getByGroup group = do
  students <- getAllStudents
  let result = filter (inGroup group) students
  return result
```

---

# **Laziness**

Haskell employs *lazy evaluation*: It only evaluates values when they are actually needed.

```cpp
int f(int x) {
  int n = factorial(100);
  return x + 1;
}
```

```haskell
f :: Int -> Int
f x =
  let n = factorial 100 in
  x + 1
```

---

# **Laziness & Functions**

```haskell
f :: Int -> Int -> Int
f 0 y = y
f x _ = x
```

```haskell
f 1 2 -- 1
f 0 2 -- 2

f 1 (1 / 0) -- 1 (But div by zero in C++)
f 0 (1 / 0) -- Division by zero
```

---

# **Data Streaming**

```haskell
processData :: [Int] -> Int
processData = sum . filter even . take 20

processData [1 ..] -- 110
```

---

# **Example: Tic-Tac-Toe AI**

```haskell

data Board = ...
data BoardTree = BoardTree { current :: Board, nextMoves :: [BoardTree] }

possibleMoves :: Board -> [Board]
possibleMoves = ...

makeTree :: Board -> BoardTree
makeTree board =
  let nextMoves = map makeTree (possibleMoves board) in
  BoardTree board nextMoves
```

---

# **Traversing the BoardTree**

```haskell
type Move = (Board, Board)

pickBest :: [BoardTree] -> BoardTree
pickBest b = ...

hasWon :: BoardTree -> Bool
hasWon b = ...

play :: BoardTree -> [Move]
play b | hasWon b = []
play (BoardTree _ []) = []
play (BoardTree crr next) =
  let best = pickBest next in
  (crr, current best) : play best
```

---

# **Why Use Haskell?**

- High assurance of correctness
- Security
- Performance

Haskell shines in scenarios where correctness and security are critical, as well as applications with complex domains, such as compilers and interpreters.
