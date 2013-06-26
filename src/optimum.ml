open Item

let add_whole acc acc_int capacity item =
  let w = Item.weight item in
  let v = Item.value item in
  let new_acc = acc +. float_of_int v in
  let new_acc_int = acc_int + v in
  let new_capacity = capacity - w in
  new_acc, new_acc_int, new_capacity

let add_part = add_whole

let rec adding acc acc_int capacity = function
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

