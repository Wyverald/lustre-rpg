open Type
open Signal

type node = {
  node_inputs : ltype list;
  node_outputs : ltype list;
  node_locals : signal list;
(*  node_local_prefix : string;*)
  node_print : signal list -> signal list -> string;
}

val make_node : ltype list -> ltype list -> signal list ->
  (signal list -> signal list -> string) -> node

val node_num_inputs : node -> int
val node_num_outputs : node -> int
val node_num_locals : node -> int

val print_as_node_decl : node -> string -> string

val expand_with_shuffle : node -> node
val expand_with_additional_input : node -> node
val expand_with_omitted_outputs : node -> node
val expand_with_constant_inputs : node -> node

val shuffle_array : 'a array -> 'a array
val make_shuffle_array : int -> int array
val shuffle_list_with_array : int array -> 'a list -> 'a list

