open String

let get_prev_total_vals cur_c c item_idx wei (prev_col, _) =
  if item_idx = 0
  then (0, 0)
  else if cur_c < 0
  then (0, 0)
  else let prev_x = item_idx - 1 in
       let prev_y1 = cur_c - wei in
       let prev_y2 = cur_c in
       let p1 = Point.get_point1 prev_y1 prev_col in
       let p2 = Point.get_point1 prev_y2 prev_col in
       (p1, p2)

let set_new_val cur_c c item_idx value (_, cur_col) =
  Point.set_point1 cur_c value cur_col

let copy_prev_val cur_y cur_x c (prev_col, cur_col) =
  let prev_x = cur_x - 1 in
  let value = Point.get_point1 cur_y prev_col in
  Point.set_point1 cur_y value cur_col

let choose_and_set_items cur_c c item_idx items acc =
  let value = Item.value items.(item_idx) in
  let wei = Item.weight items.(item_idx) in
  let (prev1, prev2) = get_prev_total_vals cur_c c item_idx wei acc in
  let new_sum_val = value + prev1 in
  if prev2 < new_sum_val
  then set_new_val cur_c c item_idx new_sum_val acc
  else copy_prev_val cur_c item_idx c acc

let use_item item_idx c items =
  let wei = Item.weight items.(item_idx) in
  not (wei > (c + 1))

let update_table cur_c c item_idx items acc =
  if use_item item_idx cur_c items
  then choose_and_set_items cur_c c item_idx items acc
  else copy_prev_val cur_c item_idx c acc

let rec iter_one_item_aux cur_c c item_idx items acc =
  if cur_c >= c
  then acc
  else let _ = update_table cur_c c item_idx items acc in
       iter_one_item_aux (cur_c + 1) c item_idx items acc

let iter_one_item c item_idx items acc =
  let cur_c = 0 in
  iter_one_item_aux cur_c c item_idx items acc

(* item_idx - just filled current column number *)
let rotate_columns_and_table c item_idx ((_, _, cur) as acc) =
  let new_tab = match acc with
    | (None, _, _) ->
      None
    | (Some tab, _, _) ->
      for i = 0 to (c-1) do
        tab.{item_idx, i} <- cur.{i}
      done;
      Some tab
  in
  let new_column = Bigarray.Array1.create
    Bigarray.int Bigarray.c_layout c in
  Bigarray.Array1.fill new_column 0;
  (new_tab, cur, new_column)

let rec iter_items_aux c item_idx items ((table, prev_col, cur_col) as acc) =
  Printf.eprintf "iter_items_aux %d\n" item_idx;
  if item_idx >= Array.length items
  then
    acc
  else
    let _ = iter_one_item c item_idx items (prev_col, cur_col) in
    let new_in_acc = (table, prev_col, cur_col) in
    let new_out_acc = rotate_columns_and_table c item_idx new_in_acc in
    iter_items_aux c (item_idx + 1) items new_out_acc

let iter_items c items max =
  let item_idx = 0 in
  let width = List.length (Array.to_list items) in
  let table =
    if (Int64.mul (Int64.of_int width) (Int64.of_int c)) <= Int64.of_int max
    then
      let t0 = Bigarray.Array2.create Bigarray.int Bigarray.c_layout width c in
      Bigarray.Array2.fill t0 0;
      Printf.eprintf "iter-items table filled\n";
      Some t0
    else (Printf.eprintf "iter-items too many items, table is not created\n";
          None)
  in
  let prev = Bigarray.Array1.create Bigarray.int Bigarray.c_layout c in
  Bigarray.Array1.fill prev 0;
  let cur = Bigarray.Array1.create Bigarray.int Bigarray.c_layout c in
  Bigarray.Array1.fill cur 0;
  iter_items_aux c item_idx items (table, prev, cur)

let calc (n_items, capacity, items) max =
  let opt, opt_int = Optimum.calc (n_items, capacity, items) in
  Printf.eprintf "opt: %f, opt_int: %d\n" opt opt_int;
  let (res_tab, res_column, _) = iter_items capacity items max in
  let max_val = Point.get_point1 (capacity - 1) res_column in
  Printf.printf "%d 1\n" max_val;
  let used_items = Backtrack.backtrack capacity items res_tab in
  Backtrack.print used_items;
  (max_val, res_tab, res_column, used_items)

