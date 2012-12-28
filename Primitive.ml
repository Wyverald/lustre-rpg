open Signal
open Type

type primitive = {
  prim_inputs : ltype list;
  prim_outputs : ltype list;
  prim_print : signal list -> string;
}

let paren s = "(" ^ s.signal_name ^ ")"

let prim_num_inputs prim = List.length prim.prim_inputs
let prim_num_outputs prim = List.length prim.prim_outputs

let make_prim inputs outputs print = {
  prim_inputs = inputs;
  prim_outputs = outputs;
  prim_print = print;
}

let make_prim_binop inputs outputs name = {
  prim_inputs = inputs;
  prim_outputs = outputs;
  prim_print = fun signals -> String.concat (" " ^ name ^ " ")
                              (List.map paren signals)
}

let make_prim_unop inputs outputs name = {
  prim_inputs = inputs;
  prim_outputs = outputs;
  prim_print = fun signals ->
    name ^ "(" ^ (String.concat ", " (List.map 
      (fun s -> s.signal_name) signals)) ^ ")"
}

let print_if_then_else signals =
  let sigarr = Array.of_list signals in
  "if " ^ sigarr.(0).signal_name ^
  " then " ^ sigarr.(1).signal_name ^
  " else " ^ sigarr.(2).signal_name

let print_pre_and_init signals =
  let sigarr = Array.of_list signals in
  sigarr.(0).signal_name ^ " -> pre(" ^
  sigarr.(1).signal_name ^ ")"

let init_primset = List.flatten [
  [make_prim_unop [t_bool] [t_bool] "not"];
  List.map (make_prim_unop [t_int] [t_int]) ["+"; "-"];
  List.map (make_prim_unop [t_real] [t_real]) ["+"; "-"];
  [make_prim_unop [t_int] [t_real] "real"];
  [make_prim_unop [t_real] [t_int] "int"];
  List.map (make_prim_binop [t_bool; t_bool] [t_bool])
    ["and"; "or"; "xor"; "="; "<>"; "->"];
  [make_prim [t_bool; t_bool] [t_bool] print_pre_and_init];
  List.map (make_prim_binop [t_int; t_int] [t_int])
    ["+"; "-"; "*"; "/"; "div"; "mod"; "->"];
  [make_prim [t_int; t_int] [t_int] print_pre_and_init];
  List.map (make_prim_binop [t_real; t_real] [t_real])
    ["+"; "-"; "*"; "/"; "div"; "mod"; "->"];
  [make_prim [t_real; t_real] [t_real] print_pre_and_init];
  List.map (make_prim_binop [t_int; t_int] [t_bool])
    ["<"; ">"; "<>"; "="; "<="; ">="];
  List.map (make_prim_binop [t_real; t_real] [t_bool])
    ["<"; ">"; "<>"; "="; "<="; ">="];
  [make_prim [t_bool; t_bool; t_bool] [t_bool] print_if_then_else];
  [make_prim [t_bool; t_int; t_int] [t_int] print_if_then_else];
  [make_prim [t_bool; t_real; t_real] [t_real] print_if_then_else];
]

let int_range n = Array.to_list (Array.init n (fun x -> x))

let rec find_matching_type_indices_ have want = match have, want with
  | _, [] -> []
  | [], _ -> raise Not_found
  | (ht, hi) :: hs, (wt, wi) :: ws -> match compare ht wt with
    | 0 -> (wi, hi) :: find_matching_type_indices_ hs ws
    | 1 -> raise Not_found
    | -1 -> find_matching_type_indices_ hs ((wt, wi) :: ws)

let find_matching_type_indices have want =
  let comp a b = compare (fst a) (fst b) in
  let zip_with_index l = List.combine l (int_range (List.length l)) in
  let have_ = List.sort comp (zip_with_index have) in
  let want_ = List.sort comp (zip_with_index want) in
  let result = find_matching_type_indices_ have_ want_ in
  snd (List.split (List.sort comp result))

let rec find_matching_types_ have wants i = match wants with
  | [] -> []
  | x :: xs -> let rest = find_matching_types_ have xs (i + 1) in
      try
        let indices = find_matching_type_indices have x in
        (indices, i) :: rest
      with Not_found -> rest

let find_matching_types have wants = find_matching_types_ have wants 0

