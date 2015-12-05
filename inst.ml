(* operand -> number *)
let gpr x =
  let prefix = String.sub x 0 2 in
  assert (prefix = "%r");
  int_of_string (String.sub x 2 (String.length x - 2))

let fpr x =
  let prefix = String.sub x 0 2 in
  assert (prefix = "%f");
  int_of_string (String.sub x 2 (String.length x - 2))

(* X-Format *)
let x_format ?(d=0) ?(s=0) ?(t=0) funct =
  Utils.bytes_of_int (
    (d lsl 21) +
    (s lsl 16) +
    (t lsl 11) +
    (funct lsl 1))

let hlt () = x_format 0
let nop () = x_format 1
let in_ rd = x_format ~d:(gpr rd) 2
let out rs = x_format ~s:(gpr rs) 3
let itof fd rs = x_format ~d:(fpr fd) ~s:(gpr rs) 4
let ftoi rd fs = x_format ~d:(gpr rd) ~s:(fpr fs) 5

(* Assembly -> Bytecode *)
let bytecode line addr_map =
  let line = ExtString.String.strip line in
  match Str.split (Str.regexp "[ \t,]+") line with
  | [] -> assert false
  | op :: args -> begin
    let args = Array.of_list args in
    match op with
    (* X-Format *)
    | "hlt"  -> hlt ()
    | "nop"  -> nop ()
    | "in"   -> in_ args.(0)
    | "out"  -> out args.(0)
    | "itof" -> itof args.(0) args.(1)
    | "ftoi" -> ftoi args.(0) args.(1)
    | _ -> failwith (Printf.sprintf "invalid mnemonic `%s`" op)
  end
