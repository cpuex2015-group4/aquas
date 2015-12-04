let rec print_lines = function
  | [] -> ()
  | l::ls -> print_string (l ^ "\n"); print_lines ls

let _ = 
  let ic = open_in "main.s" in
  let oc = open_out "main.o" in
  let buf = Std.input_list ic in
  print_lines buf;
  close_in  ic;
  close_out oc;
