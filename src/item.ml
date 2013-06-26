type value = Val of int
type weight = Wei of int
type item = Item of value * weight * float

let make v w = Item (Val v, Wei w, float_of_int v /. float_of_int w)

let compare i1 i2 =
  let Item (_, _, d1) = i1 in
  let Item (_, _, d2) = i2 in
  if d1 > d2
  then 1
  else if d1 < d2
  then -1
  else 0

let string i =
  let Item (Val v, Wei w, _) = i in
  Printf.sprintf "%d, %d" v w

let weight = function
  | Item (_, Wei w, _) -> w

let value = function
  | Item (Val v, _, _) -> v

let density = function
  | Item (_, _, d) -> d

