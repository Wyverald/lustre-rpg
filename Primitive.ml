open Signal
open Type

type primitive = {
  prim_inputs : ltype list;
  prim_outputs : ltype list;
  prim_print : signal list -> string;
}

let paren s = "(" ^ s.signal_name ^ ")"

let make_prim inputs outputs print = {
  prim_inputs = inputs;
  prim_outputs = outputs;
  prim_print = print;
}

let make_prim_binop inputs outputs name = {
  prim_inputs = inputs;
  prim_outputs = outputs;
  prim_print = fun signals -> String.concat name (List.map paren signals)
}

let make_prim_unop inputs outputs name = {
  prim_inputs = inputs;
  prim_outputs = outputs;
  prim_print = fun signals -> name ^ (paren (List.hd signals))
}

