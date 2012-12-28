open Signal
open Type

type primitive = {
  prim_inputs : ltype list;
  prim_outputs : ltype list;
  prim_print : signal list -> string;
}

val prim_num_inputs : primitive -> int
val prim_num_outputs : primitive -> int

val make_prim : ltype list -> ltype list -> (signal list -> string) -> primitive

val make_prim_binop : ltype list -> ltype list -> string -> primitive
val make_prim_unop : ltype list -> ltype list -> string -> primitive

val print_if_then_else : signal list -> string
val print_pre_and_init : signal list -> string

val init_primset : primitive list

(* Given ltypes we have, and multiple sets of ltypes we want,
 * return pairs of results that tell us which ltypes we have were matched
 * with which set of wanted ltypes. Example:
 * find_matching_types [t_bool; t_bool; t_int; t_bool; t_int]
 *                     [[t_bool; t_int; t_int]; [t_real]; [t_int; t_bool]]
 * = [ ([0; 2; 4], 0); ([2; 0], 2) ] *)
val find_matching_types : ltype list -> ltype list list -> (int list * int) list
val find_matching_types_ : ltype list -> ltype list list -> int ->
  (int list * int) list
val find_matching_type_indices : ltype list -> ltype list -> int list
val find_matching_type_indices_ : (ltype * int) list -> (ltype * int) list ->
  (int * int) list

