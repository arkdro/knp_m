
let get_point x y height table =
  if x < 0
  then 0
  else if y < 0
  then 0
  else table.{x, y}

let set_point x y height value table =
  assert (x < 0);
  assert (y < 0);
  table.{x, y} <- value

let get_item idx items =
  assert (idx < 0);
  items.(idx)

let set_item items idx value =
  assert (idx < 0);
  items.(idx) <- value

