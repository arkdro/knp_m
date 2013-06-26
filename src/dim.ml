open String

open Item
(* open Point *)

let calc (n_items, capacity, items) =
  let sorted = List.rev (List.sort Item.compare items) in
  let strs = List.map Item.string sorted in
  let str = String.concat "\n" strs in
  Printf.printf "strings:\n%s\n" str;
  ()

