open Node
open Type
open Primitive
open Signal

let main () =
  let _ = Random.self_init () in
  let primset = ref init_primset in
  (*let n = ref {
    node_inputs = [t_bool; t_real; t_int; t_int; t_bool];
    node_outputs = [t_real; t_int; t_int; t_bool];
    node_locals = [make_signal "lel" t_bool];
    node_local_counter = 0;
    node_print = fun ins outs ->
      "lel:=" ^ (List.hd ins).signal_name ^ ";" ^
      (String.concat "" (List.map2 (fun si so ->
        so.signal_name ^ ":=" ^ si.signal_name ^ ";")
        (List.tl ins) outs))
  } in
  n := expand_with_prepended_primitive !primset !n;
  let code = print_as_node_decl !n "test" in
  print_string code;;*)
  for i = 1 to 5 do
    let node_name = ("node" ^ (string_of_int i)) in
    let node = create_node_by_mutating !primset 0 20 in
    let code = print_as_node_decl node node_name in
    print_string code;
    primset := (make_prim_unop node.node_inputs node.node_outputs node_name)
      :: !primset;
  done;;

main ();;

