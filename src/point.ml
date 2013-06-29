exception Wrong_coord of string * int

let get_point x y height table =
  if x < 0
  then 0
  else if y < 0
  then 0
  else
    (* table.{x, y} *)
    Bigarray.Array2.get table x y

let set_point x y height value table =
  if x < 0 then raise (Wrong_coord ("x < 0 on set_point", x));
  if y < 0 then raise (Wrong_coord ("y < 0 on set_point", y));
  (* table.{x, y} <- value *)
  Bigarray.Array2.set table x y value

let get_item idx items =
  if idx < 0 then raise (Wrong_coord ("idx < 0 on get_item", idx));
  items.(idx)

let set_item items idx value =
  if idx < 0 then raise (Wrong_coord ("idx < 0 on set_item", idx));
  items.(idx) <- value

let dump_table tab =
  let w = Bigarray.Array2.dim1 tab in
  let h = Bigarray.Array2.dim2 tab in
  for x = 0 to w - 1 do
    for y = 0 to h - 1 do
      let value = get_point x y h tab in
      Printf.eprintf "%d " value
    done;
    Printf.eprintf "\n"
  done;
  Printf.eprintf "\n"

