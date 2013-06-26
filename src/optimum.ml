open Item

let add_whole acc acc_int capacity item =
  let w = Item.weight item in
  let v = Item.value item in
  let new_acc = acc +. float_of_int v in
  let new_acc_int = acc_int + v in
  let new_capacity = capacity - w in
  new_acc, new_acc_int, new_capacity

let add_part acc acc_int capacity item =
  let w = Item.weight item in
  let v = Item.value item in
  let frac = (float_of_int capacity) /. (float_of_int w) in
  let v_frac = (float_of_int v) *. frac in
  let new_acc = acc +. v_frac in
  let new_capacity = 0 in
  new_acc, acc_int, new_capacity

let rec adding acc acc_int capacity = function
  | _ when capacity = 0 -> acc, acc_int
  | [] -> acc, acc_int
  | h :: t ->
    let w = Item.weight h in
    let new_acc, new_acc_int, new_capacity =
      if w > capacity
      then add_part acc acc_int capacity h
      else add_whole acc acc_int capacity h
    in
    adding new_acc new_acc_int new_capacity t

let calc (n_items, capacity, items) =
  let sorted = List.rev (List.sort Item.compare items) in
  adding 0.0 0 capacity sorted

