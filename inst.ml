(* X-Format *)
let hlt () = "\x00\x00\x00\x00"
let nop () = "\x00\x00\x00\x02"

(* Assembly -> Bytecode *)
let bytecode line addr_map =
  let line = ExtString.String.strip line in
  match Str.split (Str.regexp "[ \t,]") line with
  | [] -> assert false
  | op :: args -> begin
    match op with
    | "hlt" -> hlt ()
    | "nop" -> nop ()
    | _ -> failwith (Printf.sprintf "invalid mnemonic `%s`" op)
  end
