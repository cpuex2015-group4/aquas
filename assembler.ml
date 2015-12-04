(* add prefix to label *)
let label l = "_leml_" ^ l

(* Convert integer to big-endian formatted bytecode *)
let bytes_of_int i =
  let c0 = Char.chr ((i lsr 24) land 0xff) in
  let c1 = Char.chr ((i lsr 16) land 0xff) in
  let c2 = Char.chr ((i lsr 8) land 0xff) in
  let c3 = Char.chr (i land 0xff) in
  List.fold_left (fun acc c -> acc ^ (String.make 1 c)) "" [c0;c1;c2;c3]

(* Eliminate comments *)
let eliminate_comments lines =
  List.map (fun l -> 
    if String.contains l '#' then
      let p = String.index l '#' - 1 in
      String.sub l 0 p
    else
      l)
  lines

(* Split input assembly lines to .text/.data section *)
type section = Text | Data
let split_input lines =
  let rec split_input' sec textls datals = function
    (* End of buffer *)
    | [] -> (List.rev textls, List.rev datals)

    (* Check current section then add line to either section *)
    | l::ls ->
        (let l = ExtString.String.strip l in
         match l with
         | "" -> split_input' sec textls datals ls
         | ".text" -> split_input' Text textls datals ls
         | ".data" -> split_input' Data textls datals ls
         | _ when sec = Text -> split_input' sec (l::textls) datals ls
         | _ when sec = Data -> split_input' sec textls (l::datals) ls
         | _ -> assert false) in

  split_input' Text [] [] lines

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
  (* Ignore .extern pseudo-instruction *)
  let lines =
    List.filter (fun l -> not (ExtString.String.exists l ".extern")) lines in

  (* Ignore comments *)
  let lines = eliminate_comments lines in

  (* Split lines to .text/.data section *)
  let text_lines, data_lines = split_input lines in

  (* Build mapping between label name and address *)
  (* TODO: implement
  let addr_map = build_addr_map text_lines data_lines in
  *)

  (* Emit Aquila header *)
  let text_size = List.length text_lines in
  let data_size = List.length data_lines in
  let entry_point = 0 (* AddrMap.find (label "entry") addr_map *) in
  emit_header oc text_size data_size entry_point;

  (* Emit .text section *)
  emit_text oc text_lines [] (* addr_map *);

  (* Emit .data section *)
  (* TODO: impelement
  emit_data oc data_lines;
  *)
