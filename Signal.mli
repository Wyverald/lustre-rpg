open Type

type signal = {
  signal_name : string;
  signal_type : ltype;
}

val make_signal : string -> ltype -> signal
val print_signal : signal -> string
val print_assignment : signal list -> string -> string

val random_value : ltype -> string
val random_value_signal : ltype -> signal

