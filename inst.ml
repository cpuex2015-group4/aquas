let bytecode line addr_map =
  let line = ExtString.String.strip line in
  match Str.split (Str.regexp "[ \t,]") line with
  | [] -> assert false
  | op :: args -> begin
    match op with
    | "ld" -> "0001"
    | "st" -> "0002"
    | _ -> failwith (Printf.sprintf "invalid mnemonic `%s`" op)
  end
