type regtype = GPR | FPR
type compop = None | Neg | Abs

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
  assert (0 <= d && d <= 0b11111);
  assert (0 <= s && s <= 0b11111);
  assert (0 <= t && t <= 0b11111);
  assert (0 <= funct && funct <= 0b111111);
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

(* B-Format *)
let b_format op d s imm =
  assert (0 <= op && op <= 0b11111);
  assert (0 <= d && d <= 0b11111);
  assert (0 <= s && s <= 0b11111);
  assert (0 <= imm && imm <= 0b1111111111111111);
  Utils.bytes_of_int (
    (op lsl 26) +
    (d lsl 21) +
    (s lsl 16) +
    imm)

let beq rf d s imm =
  match rf with
  | GPR -> b_format 0b01100 (gpr d) (gpr s) imm
  | FPR -> b_format 0b11100 (fpr d) (fpr s) imm
let blt rf d s imm =
  match rf with
  | GPR -> b_format 0b01101 (gpr d) (gpr s) imm
  | FPR -> b_format 0b11101 (fpr d) (fpr s) imm
let ble rf d s imm =
  match rf with
  | GPR -> b_format 0b01110 (gpr d) (gpr s) imm
  | FPR -> b_format 0b11110 (fpr d) (fpr s) imm

(* R-Format *)
let r_format d s ?(t=0) op =
  assert (0 <= op && op <= 0b11111);
  assert (0 <= d && d <= 0b11111);
  assert (0 <= s && s <= 0b11111);
  assert (0 <= t && t <= 0b11111);
  Utils.bytes_of_int (
    (op lsl 26) +
    (d lsl 21) +
    (s lsl 16) +
    (t lsl 11))

let add rf opt d s t =
  let bitim = 0 in
  let optbit = match opt with None -> 0 | Neg -> 0b0100 | Abs -> 0b1000 in
  match rf with
  | GPR -> r_format (gpr d) (gpr s) ~t:(gpr t) (optbit + bitim)
  | FPR -> r_format (fpr d) (fpr s) ~t:(fpr t) (0b10000 + optbit + bitim)

let sub rf opt d s t =
  let bitim = 1 in
  let optbit = match opt with None -> 0 | Neg -> 0b0100 | Abs -> 0b1000 in
  match rf with
  | GPR -> r_format (gpr d) (gpr s) ~t:(gpr t) (optbit + bitim)
  | FPR -> r_format (fpr d) (fpr s) ~t:(fpr t) (0b10000 + optbit + bitim)

let mul opt d s t =
  let bitim = 2 in
  let optbit = match opt with None -> 0 | Neg -> 0b0100 | Abs -> 0b1000 in
  r_format (fpr d) (fpr s) ~t:(fpr t) (0b10000 + optbit + bitim)

let div opt d s t =
  let bitim = 3 in
  let optbit = match opt with None -> 0 | Neg -> 0b0100 | Abs -> 0b1000 in
  r_format (fpr d) (fpr s) ~t:(fpr t) (0b10000 + optbit + bitim)

(* Assembly -> Bytecode *)
let bytecode line addrmap =
  let line = String.trim line in
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
    (* B-Format *)
    | "beq"   -> beq GPR args.(0) args.(1) (AddrMap.find args.(2) addrmap)
    | "beq.s" -> beq FPR args.(0) args.(1) (AddrMap.find args.(2) addrmap)
    | "blt"   -> blt GPR args.(0) args.(1) (AddrMap.find args.(2) addrmap)
    | "blt.s" -> blt FPR args.(0) args.(1) (AddrMap.find args.(2) addrmap)
    | "ble"   -> ble GPR args.(0) args.(1) (AddrMap.find args.(2) addrmap)
    | "ble.s" -> ble FPR args.(0) args.(1) (AddrMap.find args.(2) addrmap)
    (* R-Format *)
    | "add"    -> add GPR None args.(0) args.(1) args.(2)
    | "add.s"  -> add FPR None args.(0) args.(1) args.(2)
    | "addn"   -> add GPR Neg args.(0) args.(1) args.(2)
    | "addn.s" -> add FPR Neg args.(0) args.(1) args.(2)
    | "adda"   -> add GPR Abs args.(0) args.(1) args.(2)
    | "adda.s" -> add FPR Abs args.(0) args.(1) args.(2)
    | "sub"    -> sub GPR None args.(0) args.(1) args.(2)
    | "sub.s"  -> sub FPR None args.(0) args.(1) args.(2)
    | "subn"   -> sub GPR Neg args.(0) args.(1) args.(2)
    | "subn.s" -> sub FPR Neg args.(0) args.(1) args.(2)
    | "suba"   -> sub GPR Abs args.(0) args.(1) args.(2)
    | "suba.s" -> sub FPR Abs args.(0) args.(1) args.(2)
    | "mul.s"  -> mul None args.(0) args.(1) args.(2)
    | "muln.s" -> mul Neg args.(0) args.(1) args.(2)
    | "mula.s" -> mul Abs args.(0) args.(1) args.(2)
    | "div.s"  -> div None args.(0) args.(1) args.(2)
    | "divn.s" -> div Neg args.(0) args.(1) args.(2)
    | "diva.s" -> div Abs args.(0) args.(1) args.(2)
    | _ -> failwith (Printf.sprintf "invalid mnemonic `%s`" op)
  end
