open Type

type signal = {
  signal_name : string;
  signal_type : ltype;
}

let make_signal n t = {
  signal_name = n;
  signal_type = t;
}

let print_signal s = s.signal_name ^ ": " ^ s.signal_type

let random_value t = match t with
  | t_bool -> string_of_bool (Random.bool ())
  | t_int -> string_of_int (Random.int 10)
  | t_real -> string_of_float (Random.float 10.0)

let random_value_signal t = make_signal (random_value t) t

