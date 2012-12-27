open Signal
open Type

type primitive = {
  prim_inputs : ltype list;
  prim_outputs : ltype list;
  prim_print : signal list -> string;
}

val make_prim : ltype list -> ltype list -> (signal list -> string) -> primitive

val make_prim_binop : ltype list -> ltype list -> string -> primitive
val make_prim_unop : ltype list -> ltype list -> string -> primitive

