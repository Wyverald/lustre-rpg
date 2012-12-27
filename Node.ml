open Type
open Signal

type node = {
  node_inputs : ltype list;
  node_outputs : ltype list;
  node_locals : signal list;
(*  node_local_prefix : string;*)
  node_print : signal list -> signal list -> string;
}

let make_node inputs outputs locals (*prefix*) print = {
  node_inputs = inputs;
  node_outputs = outputs;
  node_locals = locals;
  (*node_local_prefix = prefix;*)
  node_print = print;
}

let node_num_inputs node = List.length node.node_inputs
let node_num_outputs node = List.length node.node_outputs
let node_num_locals node = List.length node.node_locals

let rec list_mapi_ f i l = match l with
  | [] -> []
  | x :: xs -> (f i x) :: (list_mapi_ f (i + 1) xs)

let list_mapi f l = list_mapi_ f 0 l

let print_as_node_decl node name =
  let in_signals = list_mapi
    (fun i t -> make_signal ("i" ^ (string_of_int i)) t) node.node_inputs in
  let out_signals = list_mapi
    (fun i t -> make_signal ("o" ^ (string_of_int i)) t) node.node_outputs in
  "node " ^ name ^ "(" ^
  (String.concat "; " (List.map print_signal in_signals)) ^
  ") returns (" ^
  (String.concat "; " (List.map print_signal out_signals)) ^
  ");\n" ^ 
  (if node_num_locals node = 0 then "" else "var " ^ 
    (String.concat "; " (List.map print_signal node.node_locals)) ^
    ";\n")
  ^ "let\n" ^
  (node.node_print in_signals out_signals) ^ "\ntel\n"

let shuffle_array arr = (* Knuth shuffle algorithm *)
  for n = Array.length arr - 1 downto 1 do
    let k = Random.int (n + 1) in
    let temp = arr.(n) in
    arr.(n) <- arr.(k);
    arr.(k) <- temp
  done;
  arr

let make_shuffle_array n = shuffle_array (Array.init n (fun x -> x))

let shuffle_list_with_array sarr lst =
  (* ASSERT: len(lst) == len(sarr) *)
  if Array.length sarr < 2 then lst else
  let arr = Array.of_list lst in
  let temp = arr.(sarr.(0)) in
  for i = 0 to Array.length sarr - 2 do
    arr.(sarr.(i)) <- arr.(sarr.(i + 1))
  done;
  arr.(sarr.(Array.length sarr - 1)) <- temp;
  Array.to_list arr

(* Shuffle all the inputs and outputs *)
let expand_with_shuffle node =
  let num_inputs = node_num_inputs node in
  let num_outputs = node_num_outputs node in
  let in_shuffle = make_shuffle_array num_inputs in
  let out_shuffle = make_shuffle_array num_outputs in
  make_node (shuffle_list_with_array in_shuffle node.node_inputs)
            (shuffle_list_with_array out_shuffle node.node_outputs)
            node.node_locals
            (fun ins outs -> node.node_print
              (shuffle_list_with_array in_shuffle ins)
              (shuffle_list_with_array out_shuffle outs))

(* Add one extra input and output, with the output being equal to the input *)
let expand_with_additional_input node =
  let add_type = random_type () in
  make_node (add_type :: node.node_inputs)
            (add_type :: node.node_outputs)
            node.node_locals
            (fun ins outs ->
              (List.hd outs).signal_name ^ ":=" ^
              (List.hd ins).signal_name ^ ";" ^
              node.node_print (List.tl ins) (List.tl outs))

(* Section a list into two parts by the index of the section point.
 * The first part will be in reverse order. That is, this function is 
 * basically the reverse of List.rev_append. *)
let rec list_section n lst = if n = 0 then ([], lst) else
  let fst, snd = list_section (n - 1) (List.tl lst) in
  (List.hd lst :: fst, snd)

(* Omit the first several outputs. *)
let expand_with_omitted_outputs node =
  (* ASSERT: node_num_outputs node > 1 *)
  let num_outputs = node_num_outputs node in
  let num_omitted = (Random.int num_outputs) / 2 + 1 in
  let omitted, remainder = list_section num_omitted node.node_outputs in
  let num_locals = node_num_locals node in
  let omitted_signals = list_mapi (fun i t ->
      make_signal ("l" ^ (string_of_int (num_locals + i))) t) omitted in
  make_node node.node_inputs remainder
            (List.rev_append omitted_signals node.node_locals)
            (fun ins outs -> node.node_print ins
                             (List.rev_append omitted_signals outs))

(* Supply constants to the first several inputs. *)
let expand_with_constant_inputs node =
  let num_inputs = node_num_inputs node in
  let num_supplied = (Random.int num_inputs) / 2 + 1 in
  let supplied, remainder = list_section num_supplied node.node_inputs in
  make_node remainder node.node_outputs node.node_locals
            (fun ins outs -> node.node_print
              (List.rev_append (List.map random_value_signal supplied) ins)
              outs)


