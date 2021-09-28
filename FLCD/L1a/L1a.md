# L1a

## p1.mini

```rust
// Compute the maximum of 3 numbers

let a: Int = readInt();
let b: Int = readInt();
let c: Int = readInt();

if a < b {
  a = b;
}

if a < c {
  a = c;
}

print(a);
```

## p1err.mini

```rust
// Compute the maximum of 3 numbers

let 1a: Int = readInt(); // Invalid identifier
let b: Int = a $ 3; // Invalid operator
let c: Int = readInt();

if a < b {
  a = b;
}

if a < c {
  a = c;
}

print(a);
```

## p2.mini

```rust
// Compute the GCD of 2 numbers

let a: Int = readInt();
let b: Int = readInt();

while b != 0 {
  let c = b;
  b = a % b;
  a = c;
}

print(a);
```

## p3.mini

```rust
// Compute the average of n numbers

let arr: Array[Int] = readArray();
let sum: Double = 0.0;

for i in 0 .. arr.length {
  sum += arr[i] as Double;
}

let avg: Double = sum / (arr.length as Double);
print(avg);
```
