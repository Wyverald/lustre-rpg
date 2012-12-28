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

let print_assignment ls r =
  String.concat ", " (List.map (fun s -> s.signal_name) ls) ^
  " := " ^ r ^ ";\n"

let random_value t =
  if t = t_bool then string_of_bool (Random.bool ()) else
  if t = t_int then string_of_int (Random.int 10) else 
  if t = t_real then string_of_float (Random.float 10.0) else ""

let random_value_signal t = make_signal (random_value t) t

