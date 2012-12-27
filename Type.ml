type ltype = string

let t_int = "int"
let t_bool = "bool"
let t_real = "real"

let random_type () = match Random.int 3 with
  | 0 -> t_int
  | 1 -> t_bool
  | 2 -> t_real

