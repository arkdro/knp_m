open String

let get_prev_total_vals cur_c c item_idx wei table =
  if item_idx = 0
  then (0, 0)
  else if cur_c < 0
  then (0, 0)
  else let prev_x = item_idx - 1 in
       let prev_y1 = cur_c - wei in
       let prev_y2 = cur_c in
       let p1 = Point.get_point prev_x prev_y1 c table in
       let p2 = Point.get_point prev_x prev_y2 c table in
       (p1, p2)

let set_new_val cur_c c item_idx value table =
  let _ = Point.set_point item_idx cur_c c value table in
  table                                 (* table is modifyed in-place *)

let copy_prev_val cur_y cur_x c table =
  let prev_x = cur_x - 1 in
  let value = Point.get_point prev_x cur_y c table in
  let _ = Point.set_point cur_x cur_y c value table in
  table                                 (* table is modifyed in-place *)

let choose_and_set_items cur_c c item_idx items table =
  let value = Item.value items.(item_idx) in
  let wei = Item.weight items.(item_idx) in
  let (prev1, prev2) = get_prev_total_vals cur_c c item_idx wei table in
  let new_sum_val = value + prev1 in
  if prev2 < new_sum_val
  then set_new_val cur_c c item_idx new_sum_val table
  else copy_prev_val cur_c item_idx c table

let use_item item_idx c items =
  let wei = Item.weight items.(item_idx) in
  not (wei > (c + 1))

let update_table cur_c c item_idx items table =
  if use_item item_idx cur_c items
  then choose_and_set_items cur_c c item_idx items table
  else copy_prev_val cur_c item_idx c table

let rec iter_one_item_aux cur_c c item_idx items table =
  if cur_c >= c
  then table
  else let new_table = update_table cur_c c item_idx items table in
       iter_one_item_aux (cur_c + 1) c item_idx items new_table

let iter_one_item c item_idx items table =
  let cur_c = 0 in
  iter_one_item_aux cur_c c item_idx items table

let rec iter_items_aux c item_idx items table =
  Printf.printf "iter_items_aux %d\n" item_idx;
  if item_idx >= Array.length items
  then table
  else let new_table = iter_one_item c item_idx items table in
       iter_items_aux c (item_idx + 1) items new_table

let iter_items c items =
  let item_idx = 0 in
  let width = List.length (Array.to_list items) in
  let table = Bigarray.Array2.create
    Bigarray.int Bigarray.c_layout width c in
  Bigarray.Array2.fill table 0;
  Printf.printf "iter-items table filled\n";
  iter_items_aux c item_idx items table

let calc (n_items, capacity, items) =
  let opt, opt_int = Optimum.calc (n_items, capacity, items) in
  Printf.printf "opt: %f, opt_int: %d\n" opt opt_int;
  let res = iter_items capacity items in
  let max_val = Point.get_point (n_items - 1) (capacity - 1) 0 res in
  Printf.printf "max val: %d\n" max_val;
  let used_items = Backtrack.backtrack capacity items res in
  Backtrack.print used_items;
  (max_val, res, used_items)

