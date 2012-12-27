open Node
open Type
open Primitive
open Signal

let print_if_then_else signals =
  let sigarr = Array.of_list signals in
  "if " ^ sigarr.(0).signal_name ^
  " then " ^ sigarr.(1).signal_name ^
  " else " ^ sigarr.(2).signal_name

let init_primset () =
  [ List.map (make_prim_unop [t_bool] [t_bool]) ["not"]
  ; List.map (make_prim_unop [t_int] [t_int]) ["+"; "-"]
  ; List.map (make_prim_binop [t_bool; t_bool] [t_bool])
             ["and"; "or"; "xor"]
  ; List.map (make_prim_binop [t_int; t_int] [t_int])
             ["+"; "-"; "*"; "/"; "div"; "mod"]
  ; [make_prim [t_bool; t_bool; t_bool] [t_bool] print_if_then_else]
  ; [make_prim [t_bool; t_int; t_int] [t_int] print_if_then_else]
  ]

let main () =
  let _ = Random.self_init () in
  (*let primset = init_primset () in*)
  let n = {
    node_inputs = [t_bool; t_bool; t_int];
    node_outputs = [t_int; t_int];
    node_locals = [make_signal "lel" t_bool];
    node_print = fun ins outs ->
      "lel:=" ^ (List.hd ins).signal_name ^ ";" ^
      (String.concat "" (List.map2 (fun si so ->
        so.signal_name ^ ":=" ^ si.signal_name ^ ";")
        (List.tl ins) outs))
  } in
  let res = print_as_node_decl
    (expand_with_additional_input
    (expand_with_additional_input
    (expand_with_additional_input
    (expand_with_shuffle
    (expand_with_constant_inputs
    (expand_with_omitted_outputs n)))))) "test" in
  print_string res;;

main ();;

