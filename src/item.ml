type value = Val of int
type weight = Wei of int
type item = Item of value * weight

let make a b = Item (Val a, Wei b)

let compare i1 i2 =
  let Item (Val v1, Wei w1) = i1 in
  let Item (Val v2, Wei w2) = i2 in
  if v1 > v2
  then 1
  else if v1 < v2
  then -1
  else if w1 > w2
  then 1
  else if w1 < w2
  then -1
  else 0

let string i =
  let Item (Val v, Wei w) = i in
  Printf.sprintf "%d, %d" v w
