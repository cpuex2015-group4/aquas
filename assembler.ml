module AddrMap = Map.Make(String)

(* constants *)
let text_offset = 0x00400
let data_offset = 0x10000
let p_extern = ".extern"
let p_text   = ".text"
let p_data   = ".data"
let p_word   = ".word"

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
         | l when l = p_text -> split_input' Text textls datals ls
         | l when l = p_data -> split_input' Data textls datals ls
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

(* Build (Label -> Address) map *)
let build_addr_map text_lines data_lines =
  let pattern_label = Str.regexp "[A-za-z_][A-za-z0-9_\\.]*:" in

  (* general mapping-building function *)
  let rec build map offset n striped_lines = function
    | [] -> (map, List.rev striped_lines)
    | l::lines ->
        let l = ExtString.String.strip l in
        if Str.string_match pattern_label l 0 then
          (* if input line is label *)
          build
            (* add new relation between label and address *)
            (AddrMap.add (String.sub l 0 (String.length l -1)) (n + offset) map)
            offset (n + 1)
            (* strip label line *)
            (* l :: *) striped_lines
            lines
        else
          build map offset (n + 1) (l :: striped_lines) lines in

  (* build map for .text/.data respectively *)
  let text_map, text_lines = build AddrMap.empty text_offset 0 [] text_lines in
  let data_map, data_lines = build AddrMap.empty data_offset 0 [] data_lines in

  (* merge .text/.data address map *)
  let merge_spec = fun l text_addr data_addr ->
    match text_addr, data_addr with
    | Some addr, None -> Some addr
    | None, Some addr -> Some addr
    | Some addr1, Some addr2 -> failwith "cannot define same label"
    | None, None -> assert false in
  let map = AddrMap.merge merge_spec text_map data_map in
  (map, text_lines, data_lines)

(* .text section emitter *)
let rec emit_text oc lines addr_map =
  match lines with
  | [] -> ()
  | l::lines ->
      (* dummy *)
      (Printf.fprintf oc "line";
       emit_text oc lines addr_map)

(* .data section emitter *)
let rec emit_data oc = function
  | [] -> ()
  | l::lines ->
      (let l = ExtString.String.strip l in
       let len = String.length p_word in 

       (* .data lines should start with `.word` *)
       assert ((String.sub l 0 len) = p_word);

       (* fetch word data *)
       let s = ExtString.String.strip (String.sub l len (String.length l - len - 1)) in
       let i = int_of_string (ExtString.String.strip s) in

       (* emit data *)
       Printf.fprintf oc "%s" (bytes_of_int i);
       emit_data oc lines)

(* Executable code emitter *)
let emit oc lines =
  (* Ignore .extern pseudo-instruction *)
  let lines =
    List.filter (fun l -> not (ExtString.String.exists l p_extern)) lines in

  (* Ignore comments *)
  let lines = eliminate_comments lines in

  (* Split lines to .text/.data section *)
  let text_lines, data_lines = split_input lines in
  (* here empty lines are striped *)

  (* Build mapping between label name and address *)
  let addr_map, text_lines, data_lines =
    build_addr_map text_lines data_lines in
  (* here text_lines and data_lines are striped labels off *)

  (* Emit Aquila header *)
  let text_size = List.length text_lines in
  let data_size = List.length data_lines in
  let entry_point = AddrMap.find (label "entry") addr_map in
  emit_header oc text_size data_size entry_point;

  (* Emit .text section *)
  emit_text oc text_lines addr_map;

  (* Emit .data section *)
  emit_data oc data_lines;
