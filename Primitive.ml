open Signal
open Type

type primitive = {
  prim_inputs : ltype list;
  prim_outputs : ltype list;
  prim_weight : float;
  prim_print : signal list -> string;
}

let prim_num_inputs prim = List.length prim.prim_inputs
let prim_num_outputs prim = List.length prim.prim_outputs

let make_prim inputs outputs weight print = {
  prim_inputs = inputs;
  prim_outputs = outputs;
  prim_weight = weight;
  prim_print = print;
}

let print_binop name signals = 
  String.concat (" " ^ name ^ " ") (List.map (fun s -> s.signal_name) signals)

let print_unop name signals = name ^ "(" ^ (String.concat ", " (List.map 
  (fun s -> s.signal_name) signals)) ^ ")"

let print_if_then_else signals =
  let sigarr = Array.of_list signals in
  "if " ^ sigarr.(0).signal_name ^
  " then " ^ sigarr.(1).signal_name ^
  " else " ^ sigarr.(2).signal_name

let print_pre_and_init signals =
  let sigarr = Array.of_list signals in
  sigarr.(0).signal_name ^ " -> pre(" ^
  sigarr.(1).signal_name ^ ")"

let make_prim_unop i o w p = make_prim i o w (print_unop p)
let make_prim_binop i o w p = make_prim i o w (print_binop p)

let init_primset = List.flatten [
  (* Unary operators *)
  [make_prim_unop [t_bool] [t_bool] 0.5 "not";
   make_prim_unop [t_int] [t_int] 0.05 "+";
   make_prim_unop [t_int] [t_int] 0.25 "-";
   make_prim_unop [t_real] [t_real] 0.05 "+";
   make_prim_unop [t_real] [t_real] 0.25 "-";
   make_prim_unop [t_int] [t_real] 0.5 "real";
   make_prim_unop [t_real] [t_int] 0.5 "int"];

  (* Binary operators *)
  List.map (make_prim_binop [t_bool; t_bool] [t_bool] 0.2)
    ["and"; "or"; "xor"; "="; "<>"];
  [make_prim_binop [t_bool; t_bool] [t_bool] 0.5 "->"];
  [make_prim [t_bool; t_bool] [t_bool] 1.0 print_pre_and_init];

  List.map (make_prim_binop [t_int; t_int] [t_int] 0.2)
    ["+"; "-"; "*"; "/"; "div"; "mod"];
  [make_prim_binop [t_int; t_int] [t_int] 0.5 "->"];
  [make_prim [t_int; t_int] [t_int] 1.0 print_pre_and_init];

  List.map (make_prim_binop [t_real; t_real] [t_real] 0.2)
    ["+"; "-"; "*"; "/"; "div"; "mod"];
  [make_prim_binop [t_real; t_real] [t_real] 0.5 "->"];
  [make_prim [t_real; t_real] [t_real] 1.0 print_pre_and_init];

  List.map (make_prim_binop [t_int; t_int] [t_bool] 0.1)
    ["<"; ">"; "<>"; "="; "<="; ">="];
  List.map (make_prim_binop [t_real; t_real] [t_bool] 0.1)
    ["<"; ">"; "<>"; "="; "<="; ">="];

  (* Ternary operators *)
  [make_prim [t_bool; t_bool; t_bool] [t_bool] 1.0 print_if_then_else];
  [make_prim [t_bool; t_int; t_int] [t_int] 1.0 print_if_then_else];
  [make_prim [t_bool; t_real; t_real] [t_real] 1.0 print_if_then_else];
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

let weighted_choose from weights =
  (* Assert that |from| is sorted by its snd part. *)
  let rec zip i from weights = match from with
    | [] -> []
    | (xl, xr) :: xs -> if xr = i
      then ((xl, xr), List.hd weights) :: zip (i + 1) xs (List.tl weights)
      else zip (i + 1) from (List.tl weights)
  in let zipped = zip 0 from weights in
  let sum_of_weights = List.fold_left (fun a b -> a +. snd b) 0.0 zipped in
  let r = Random.float sum_of_weights in
  let rec choose r lst = match lst with
    | (xl, xr) :: [] -> xl
    | (xl, xr) :: xs -> if r < xr then xl else choose (r -. xr) xs in
  choose r zipped

