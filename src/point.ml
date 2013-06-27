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
  Bigarray.Array2.set table x y

let get_item idx items =
  if idx < 0 then raise (Wrong_coord ("idx < 0 on get_item", idx));
  items.(idx)

let set_item items idx value =
  if idx < 0 then raise (Wrong_coord ("idx < 0 on set_item", idx));
  items.(idx) <- value

