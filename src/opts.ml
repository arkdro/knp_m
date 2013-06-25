open Arg
open Dim

let read_data verbose ifile = ()
let print_result res = ()

let get_pars verbose ifile =
  let usage_text = "possible params" in
  let pars = [
    ("-i", Set_string ifile, "input file");
    ("-v", Set_int verbose, "verbose (default: 1)")
  ] in
  let _ = parse pars (fun x -> ()) usage_text in
  if (not (Sys.file_exists !ifile)) then
    (Printf.printf "no input file exist: %s\n" !ifile;
     assert false)

let run () =
  let ifile = ref "" in
  let verbose = ref 1 in
  let _ = get_pars verbose ifile in
  let in_data = read_data verbose ifile in
  let res = Dim.calc in_data in
  print_result res

