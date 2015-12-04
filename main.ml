(*
 * Aquila Assembler
 *)

let usage_msg =
  "Aquila Assmbler developed by cpuex2015-group-4\n" ^
  Printf.sprintf "usage: %s [asm-file]\n" Sys.argv.(0)

(* Check if the given assembly file name is valid *)
let validate_filename name =
  if name = "" then
    (false, "specify assembly file")
  else
    let p = String.rindex name '.' + 1 in
    let ext = String.sub name p ((String.length name) - p) in
    if ext <> "s" then
      (false, "assembly file name should have extension `.s`")
    else
      (true, String.sub name 0 (p-1))

(* Entry Point *)
let () = 
  (* Arguments Parsing *)
  let file = ref "" in
  Arg.parse [] (fun s -> file := s) usage_msg;
  let valid, s = validate_filename !file in
  if not valid then
    failwith s
  else
    file := s;

  (* Opening File *)
  let ic = open_in  (!file ^ ".s")   in
  let oc = open_out (!file ^ ".run") in

  (* Read Input Assembly *)
  let buf = Std.input_list ic in
  Assembler.emit oc buf;

  (* Finalize *)
  close_in  ic;
  close_out oc;
