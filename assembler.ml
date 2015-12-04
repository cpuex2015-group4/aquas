(* add prefix to label *)
let label l = "_leml_" ^ l

(* Convert integer to big-endian formatted bytecode *)
let bytes_of_int i =
  let c0 = Char.chr ((i lsr 24) land 0xff) in
  let c1 = Char.chr ((i lsr 16) land 0xff) in
  let c2 = Char.chr ((i lsr 8) land 0xff) in
  let c3 = Char.chr (i land 0xff) in
  List.fold_left (fun acc c -> acc ^ (String.make 1 c)) "" [c0;c1;c2;c3]

(* Emit header *)
let emit_header oc text_size data_size entry_point =
  Printf.fprintf oc "AQIL";
  Printf.fprintf oc "%s" (bytes_of_int text_size);
  Printf.fprintf oc "%s" (bytes_of_int data_size);
  Printf.fprintf oc "%s" (bytes_of_int entry_point)

(* .text section emitter *)
let rec emit_text oc lines addr_map =
  match lines with
  | [] -> ()
  | l::lines ->
      (* dummy *)
      (Printf.fprintf oc "line";
       emit_text oc lines addr_map)

(* Executable code emitter *)
let emit oc lines =
  (* Split lines to .text/.data section *)
  (* TODO: implement
  let text_lines, data_lines = split_input lines in
  *)

  (* Build mapping between label name and address *)
  (* TODO: implement
  let addr_map = build_addr_map data_lines in
  *)

  (* Emit Aquila header *)
  let text_size = 0 (* List.length text_lines *) in
  let data_size = 0 (* List.length data_lines *) in
  let entry_point = 0 (* AddrMap.find (label "entry") addr_map *) in
  emit_header oc text_size data_size entry_point;

  (* Emit .text section *)
  let text_lines = lines in (* TODO: delete after implementation *)
  emit_text oc text_lines [] (* addr_map *);

  (* Emit .data section *)
  (* TODO: impelement
  emit_data oc data_lines;
  *)
