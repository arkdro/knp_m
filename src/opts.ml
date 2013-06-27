open Arg
open Pcre

open Item
open Dim

let read_data verbose ifile = ()
let print_result res = ()

let get_pars verbose ifile =
  let usage_text = "possible params" in
  let pars = [
    ("-i", Set_string ifile, "input file");
    ("-v", Set_int verbose, "verbose (default: 1)")
  ] in
  let _ = parse pars (fun _ -> ()) usage_text in
  if (not (Sys.file_exists !ifile)) then
    (Printf.printf "no input file exist: %s\n" !ifile;
     assert false)

let read_file file =
  let lines = ref [] in
  let chan = open_in file in
  try
    while true; do
      lines := input_line chan :: !lines
    done;
    []
  with End_of_file ->
    close_in chan;
    List.rev !lines

let regexp = Pcre.regexp "(\\d+)\\s+(\\d+)"

let parse_line line =
  try
    match Pcre.extract_all ~rex:regexp ~full_match:false line
    with
    | [|[|s1; s2|]|] ->
      Some (int_of_string s1, int_of_string s2)
    | _ ->
      None
  with
  | Not_found ->
    None
  | Failure "int_of_string" ->
    None

let parse_lines lines =
  let Some (n_items, capacity) = parse_line (List.hd lines) in
  let items = List.map parse_line (List.tl lines) in
  let filtered = List.filter (function
    | Some _ -> true
    | None -> false)
    items in
  assert (n_items = List.length filtered);
  let items2 = List.map (function
    | Some (n1, n2) -> Item.make n1 n2
  ) filtered in
  (n_items, capacity, Array.of_list items2)

let read_data verbose file =
  let lines = read_file file in
  parse_lines lines

let run () =
  let ifile = ref "" in
  let verbose = ref 1 in
  let _ = get_pars verbose ifile in
  let in_data = read_data verbose !ifile in
  let res = Dim.calc in_data in
  print_result res

