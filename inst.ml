type regtype    = GPR | FPR
type processing = None | Neg | Abs
type jumpspec   = None | Link

let pc = ref (Config.text_offset - 1)

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
  assert (-0b1000000000000000 <= imm && imm <= 0b0111111111111111);
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
let r_format d ?(s=0) ?(t=0) op =
  assert (0 <= op && op <= 0b11111);
  assert (0 <= d && d <= 0b11111);
  assert (0 <= s && s <= 0b11111);
  assert (0 <= t && t <= 0b11111);
  Utils.bytes_of_int (
    (op lsl 26) +
    (d lsl 21) +
    (s lsl 16) +
    (t lsl 11) +
    1)

let add rf (opt:processing) d s t =
  let bitim = 0 in
  let optbit = match opt with None -> 0 | Neg -> 0b0100 | Abs -> 0b1000 in
  match rf with
  | GPR -> r_format (gpr d) ~s:(gpr s) ~t:(gpr t) (optbit + bitim)
  | FPR -> r_format (fpr d) ~s:(fpr s) ~t:(fpr t) (0b10000 + optbit + bitim)

let sub rf (opt:processing) d s t =
  let bitim = 1 in
  let optbit = match opt with None -> 0 | Neg -> 0b0100 | Abs -> 0b1000 in
  match rf with
  | GPR -> r_format (gpr d) ~s:(gpr s) ~t:(gpr t) (optbit + bitim)
  | FPR -> r_format (fpr d) ~s:(fpr s) ~t:(fpr t) (0b10000 + optbit + bitim)

let mul (opt:processing) d s t =
  let bitim = 2 in
  let optbit = match opt with None -> 0 | Neg -> 0b0100 | Abs -> 0b1000 in
  r_format (fpr d) ~s:(fpr s) ~t:(fpr t) (0b10000 + optbit + bitim)

let div (opt:processing) d s t =
  let bitim = 3 in
  let optbit = match opt with None -> 0 | Neg -> 0b0100 | Abs -> 0b1000 in
  r_format (fpr d) ~s:(fpr s) ~t:(fpr t) (0b10000 + optbit + bitim)

(* I-Format *)
let i_format ?(d=0) ?(s=0) ?(imm=0) op =
  assert (0 <= op && op <= 0b11111);
  assert (0 <= d && d <= 0b11111);
  assert (0 <= s && s <= 0b11111);
  assert (-0b1000000000000000 <= imm && imm <= 0b0111111111111111);
  Utils.bytes_of_int (
    (1 lsl 31) +
    (op lsl 26) +
    (d lsl 21) +
    (s lsl 16) +
    imm)

let addi rf (opt:processing) d s imm =
  let bitim = 0 in
  let optbit = match opt with None -> 0 | Neg -> 0b0100 | Abs -> 0b1000 in
  match rf with
  | GPR -> i_format ~d:(gpr d) ~s:(gpr s) ~imm:imm (optbit + bitim)
  | FPR -> i_format ~d:(fpr d) ~s:(fpr s) ~imm:imm (0b10000 + optbit + bitim)

let subi rf (opt:processing) d s imm =
  let bitim = 1 in
  let optbit = match opt with None -> 0 | Neg -> 0b0100 | Abs -> 0b1000 in
  match rf with
  | GPR -> i_format ~d:(gpr d) ~s:(gpr s) ~imm:imm (optbit + bitim)
  | FPR -> i_format ~d:(fpr d) ~s:(fpr s) ~imm:imm (0b10000 + optbit + bitim)

let muli (opt:processing) d s imm =
  let bitim = 2 in
  let optbit = match opt with None -> 0 | Neg -> 0b0100 | Abs -> 0b1000 in
  i_format ~d:(fpr d) ~s:(fpr s) ~imm:imm (0b10000 + optbit + bitim)

let divi (opt:processing) d s imm =
  let bitim = 3 in
  let optbit = match opt with None -> 0 | Neg -> 0b0100 | Abs -> 0b1000 in
  i_format ~d:(fpr d) ~s:(fpr s) ~imm:imm (0b10000 + optbit + bitim)

let jspec = function None -> 0 | Link -> 1
let j (jopt:jumpspec) imm = i_format ~imm:imm (((jspec jopt) lsl 2) + 2)
let jr (jopt:jumpspec) d = r_format ~d:(gpr d) (((jspec jopt) lsl 2) + 2)

let ld rf d s imm =
  match rf with
  | GPR -> i_format ~d:(gpr d) ~s:(gpr s) ~imm:imm 0b01100
  | FPR -> i_format ~d:(fpr d) ~s:(gpr s) ~imm:imm 0b11100

let st rf d s imm =
  match rf with
  | GPR -> i_format ~d:(gpr d) ~s:(gpr s) ~imm:imm 0b01101
  | FPR -> i_format ~d:(fpr d) ~s:(gpr s) ~imm:imm 0b11101

let sll d s imm =
  if not ((imm land 0b11111) <> 0) then failwith "shamt must be under 31";
  i_format ~d:(gpr d) ~s:(gpr s) ~imm:imm 0b01110
let srl d s imm =
  if not ((imm land 0b11111) <> 0) then failwith "shamt must be under 31";
  i_format ~d:(gpr d) ~s:(gpr s) ~imm:imm 0b01111

let inv d s = i_format ~d:(fpr d) ~s:(fpr s) 0b11110
let sqrt d s = i_format ~d:(fpr d) ~s:(fpr s) 0b11111

(* pseudo instruction *)
let li d imm = addi GPR None d "%r0" imm
let mr rf d s =
  match rf with
  | GPR -> add rf None d s "%r0" 
  | FPR -> add rf None d s "%f0" 

(* Assembly -> Bytecode *)
let bytecode line addrmap =
  pc := !pc + 1;
  (* convert immediate (/$-?\d+/ or label) *)
  let imm x =
    let prefix = String.sub x 0 1 in
    if prefix = "$" then
      (* fetch integer immediate *)
      int_of_string (String.sub x 1 (String.length x - 1))
    else
      (* fetch label address *)
      AddrMap.find x addrmap in

  (* convert address to relative address *)
  let rel x =
    let abs_addr = AddrMap.find x addrmap in
    let rel_addr = abs_addr - !pc in
    rel_addr in

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
    | "beq"   -> beq GPR args.(0) args.(1) (rel args.(2))
    | "beq.s" -> beq FPR args.(0) args.(1) (rel args.(2))
    | "blt"   -> blt GPR args.(0) args.(1) (rel args.(2))
    | "blt.s" -> blt FPR args.(0) args.(1) (rel args.(2))
    | "ble"   -> ble GPR args.(0) args.(1) (rel args.(2))
    | "ble.s" -> ble FPR args.(0) args.(1) (rel args.(2))
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
    | "jr"     -> jr None args.(0)
    | "jral"   -> jr Link args.(0)
    (* I-Format *)
    | "addi"    -> addi GPR None args.(0) args.(1) (imm args.(2))
    | "addin"   -> addi GPR Neg args.(0) args.(1)  (imm args.(2))
    | "addia"   -> addi GPR Abs args.(0) args.(1)  (imm args.(2))
    | "subi"    -> subi GPR None args.(0) args.(1) (imm args.(2))
    | "subin"   -> subi GPR Neg args.(0) args.(1)  (imm args.(2))
    | "subia"   -> subi GPR Abs args.(0) args.(1)  (imm args.(2))
    | "j"       -> j None (imm args.(0))
    | "jal"     -> j Link (imm args.(0))
    | "ld"      -> ld GPR args.(0) args.(1) (imm args.(2))
    | "ld.s"    -> ld FPR args.(0) args.(1) (imm args.(2))
    | "st"      -> st GPR args.(0) args.(1) (imm args.(2))
    | "st.s"    -> st FPR args.(0) args.(1) (imm args.(2))
    | "sll"     -> sll args.(0) args.(1) (imm args.(2))
    | "srl"     -> srl args.(0) args.(1) (imm args.(2))
    | "inv.s"   -> inv args.(0) args.(1)
    | "sqrt.s"  -> inv args.(0) args.(1)
    (* pseudo instruction *)
    | "li"      -> li args.(0) (imm args.(1))
    | "mr"      -> mr GPR args.(0) args.(1)
    | "mr.s"    -> mr FPR args.(0) args.(1)
    | _ -> failwith (Printf.sprintf "invalid mnemonic `%s`" op)
  end
