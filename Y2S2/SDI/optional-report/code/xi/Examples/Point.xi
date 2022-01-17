#!/usr/bin/env xi

type Point = { x: Num, y: Num } in
type Point3D = Point & { z: Num } in

type HasX = { x: Num } in

let add (a: Point) (b: Point) =
  { x = a.x + b.x, y = a.y + b.y }
in

let a = { x = 2, y = 3 } in
let b: Point3D = { x = -2, y = 4, z = 11 } in
print add a b;
print add a b as HasX;

let c: Point = b in
print b as Point;
print c
