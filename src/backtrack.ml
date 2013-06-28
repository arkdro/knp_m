
let do_not_use_item cur_c item_idx acc =
  let new_item_idx = item_idx - 1 in
  let _ = Point.set_item acc item_idx 0 in
  (new_item_idx, cur_c, acc)

let use_item cur_c c item_idx items acc =
  let item = Point.get_item item_idx items in
  let wei = Item.weight item in
  let new_item_idx = item_idx - 1 in
  let new_cur_c = cur_c - wei in
  let _ = Point.set_item acc item_idx 1 in
  (new_item_idx, new_cur_c, acc)

let get_new_item cur_c c item_idx items acc table =
  let cur_val = Point.get_point item_idx cur_c c table in
  let prev2_val = Point.get_point (item_idx - 1) cur_c c table in
  if cur_val = prev2_val
  then do_not_use_item cur_c item_idx acc
  else use_item cur_c c item_idx items acc

let rec backtrack_aux cur_c c item_idx items acc table =
  if item_idx < 0
  then acc
  else let (new_x, new_y, new_acc) = get_new_item
         cur_c c item_idx items acc table in
       backtrack_aux new_y c new_x items new_acc table

let backtrack capacity items table =
  let x = (Array.length items) - 1 in
  let y = capacity - 1 in
  let acc = Array.make (Array.length items) 0 in
  backtrack_aux y capacity x items acc table

let print items =
  let rec print_aux x items =
    if x >= Array.length items
    then Printf.printf "\n"
    else let _ = Printf.printf "%d " items.(x) in
         print_aux (x + 1) items
  in
  print_aux 0 items

