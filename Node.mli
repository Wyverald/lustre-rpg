open Type
open Signal
open Primitive

type node = {
  node_inputs : ltype list;
  node_outputs : ltype list;
  node_locals : signal list;
  node_local_counter : int;
  node_print : signal list -> signal list -> string;
}

val make_node : ltype list -> ltype list -> signal list -> int ->
  (signal list -> signal list -> string) -> node

val node_num_inputs : node -> int
val node_num_outputs : node -> int
val node_num_locals : node -> int

val print_as_node_decl : node -> string -> string

val expand_with_shuffle : node -> node
val expand_with_additional_input : node -> node
val expand_with_omitted_outputs : node -> node
val expand_with_constant_inputs : node -> node
val expand_with_output_from_copy : node -> node
val expand_with_appended_primitive : primitive list -> node -> node
val expand_with_prepended_primitive : primitive list -> node -> node
val expand_with_pre_and_init : node -> node

val mutate_node : primitive list -> node -> node
val create_node_by_mutating : primitive list -> int -> int -> node

val shuffle_array : 'a array -> 'a array
val make_shuffle_array : int -> int array
val array_rev : 'a array -> 'a array
val shuffle_list_with_array : int array -> 'a list -> 'a list

val list_remove_indices : int list -> 'a list -> 'a list
val list_section : int -> 'a list -> 'a list * 'a list
val list_mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

