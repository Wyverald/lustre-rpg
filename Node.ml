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

let make_node inputs outputs locals lc print = {
  node_inputs = inputs;
  node_outputs = outputs;
  node_locals = locals;
  node_local_counter = lc;
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
  (node.node_print in_signals out_signals) ^ "tel\n"

let shuffle_array arr = (* Knuth shuffle algorithm *)
  for n = Array.length arr - 1 downto 1 do
    let k = Random.int (n + 1) in
    let temp = arr.(n) in
    arr.(n) <- arr.(k);
    arr.(k) <- temp
  done;
  arr

let make_shuffle_array n = shuffle_array (Array.init n (fun x -> x))

let array_rev arr = Array.of_list (List.rev (Array.to_list arr))

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
  if node_num_outputs node = 0 || node_num_inputs node = 0 then node else
  let num_inputs = node_num_inputs node in
  let num_outputs = node_num_outputs node in
  let in_shuffle = make_shuffle_array num_inputs in
  let out_shuffle = make_shuffle_array num_outputs in
  make_node (shuffle_list_with_array in_shuffle node.node_inputs)
            (shuffle_list_with_array out_shuffle node.node_outputs)
            node.node_locals
            node.node_local_counter
            (fun ins outs -> node.node_print
              (shuffle_list_with_array (array_rev in_shuffle) ins)
              (shuffle_list_with_array (array_rev out_shuffle) outs))

(* Add one extra input and output, with the output being equal to the input *)
let expand_with_additional_input node =
  let add_type = random_type () in
  make_node (add_type :: node.node_inputs)
            (add_type :: node.node_outputs)
            node.node_locals
            node.node_local_counter
            (fun ins outs ->
              (print_assignment [List.hd outs] (List.hd ins).signal_name) ^
              (node.node_print (List.tl ins) (List.tl outs)))

(* Section a list into two parts by the index of the section point. *)
let rec list_section n lst = if n = 0 then ([], lst) else
  let fst, snd = list_section (n - 1) (List.tl lst) in
  (List.hd lst :: fst, snd)

(* Omit the first several outputs. *)
let expand_with_omitted_outputs node =
  (* ASSERT: node_num_outputs node > 1 *)
  if node_num_outputs node < 2 then node else
  let num_outputs = node_num_outputs node in
  let num_omitted = (Random.int num_outputs) / 2 + 1 in
  let omitted, remainder = list_section num_omitted node.node_outputs in
  let omitted_signals = list_mapi (fun i t -> make_signal
    ("l" ^ (string_of_int (node.node_local_counter + i))) t) omitted in
  make_node node.node_inputs remainder
            (List.append omitted_signals node.node_locals)
            (num_omitted + node.node_local_counter)
            (fun ins outs -> node.node_print ins
                             (List.append omitted_signals outs))

(* Supply constants to the first several inputs. *)
let expand_with_constant_inputs node =
  if node_num_inputs node = 0 then node else
  let num_inputs = node_num_inputs node in
  let num_supplied = (Random.int num_inputs) / 2 + 1 in
  let supplied, remainder = list_section num_supplied node.node_inputs in
  make_node remainder node.node_outputs node.node_locals
            node.node_local_counter
            (fun ins outs -> node.node_print
              (List.append (List.map random_value_signal supplied) ins)
              outs)

(* Create a new output that's just a copy of an existing input. *)
let expand_with_output_from_copy node =
  if node_num_inputs node = 0 then node else
  let chosen_input = Random.int (node_num_inputs node) in
  make_node node.node_inputs
            (List.nth node.node_inputs chosen_input :: node.node_outputs)
            node.node_locals
            node.node_local_counter
            (fun ins outs ->
              (print_assignment [List.hd outs]
                (List.nth ins chosen_input).signal_name) ^
              (node.node_print ins (List.tl outs)))

(* Take some of the outputs and attach a primitive. *)
(* We keep the used outputs, but mark them as omittable. *)
let expand_with_appended_primitive primset node =
  let res = find_matching_types node.node_outputs
      (List.map (fun prim -> prim.prim_inputs) primset) in
  if List.length res = 0 then node else
  let out_indices, prim_index = List.nth res (Random.int (List.length res)) in
  let prim = List.nth primset prim_index in
  make_node node.node_inputs
            (List.append prim.prim_outputs node.node_outputs)
            node.node_locals
            node.node_local_counter
            (fun ins outs ->
              let l_outs, r_outs = list_section (prim_num_outputs prim) outs in
              print_assignment l_outs (prim.prim_print 
                (List.map (fun i -> List.nth r_outs i) out_indices)) ^
              node.node_print ins r_outs)

let rec list_remove_indices_ i ind lst = match ind, lst with
  | _, [] -> []
  | [], _ -> lst
  | x :: xs, _ -> if i = x
    then list_remove_indices_ (i + 1) xs (List.tl lst)
    else (List.hd lst) :: list_remove_indices_ (i + 1) ind (List.tl lst)

let list_remove_indices ind lst = list_remove_indices_ 0 ind lst

let list_merge_with_indices ind ll rl =
  let len = List.length ll + List.length rl in
  let range = Array.to_list (Array.init len (fun x -> x)) in
  let rl_ind = list_remove_indices ind range in
  let zipped = List.append (List.combine ind ll) (List.combine rl_ind rl) in
  let sorted = List.sort (fun a b -> compare (fst a) (fst b)) zipped in
  snd (List.split sorted)

(* Place a primitive before some of the inputs. *)
(* We discard these old inputs. *)
let expand_with_prepended_primitive primset node =
  let res = find_matching_types node.node_inputs
      (List.map (fun prim -> prim.prim_outputs) primset) in
  if List.length res = 0 then node else
  let in_indices, prim_index = List.nth res (Random.int (List.length res)) in
  let prim = List.nth primset prim_index in
  let new_locals = list_mapi (fun i t -> make_signal ("l" ^
      (string_of_int (node.node_local_counter + i))) t) prim.prim_outputs in
  make_node (List.append prim.prim_inputs
              (list_remove_indices in_indices node.node_inputs))
            node.node_outputs
            (List.append node.node_locals new_locals)
            (node.node_local_counter + prim_num_outputs prim)
            (fun ins outs ->
              let l_ins, r_ins = list_section (prim_num_inputs prim) ins in
              let r_ins_with_locals = list_merge_with_indices
                  in_indices new_locals r_ins in
              print_assignment new_locals (prim.prim_print l_ins) ^
              node.node_print r_ins_with_locals outs)

let __fc f g x = f (g x)

let rec find_index_ i ele lst = match lst with
  | [] -> None
  | x :: xs -> if x = ele then Some i else find_index_ (i + 1) ele xs

let find_index ele lst = find_index_ 0 ele lst

(* Introduce temporal logic with pre and ->.
 * We do this by taking an output, pushing it through a pre and then
 * immediately a ->, and finally routing it back to an input with the
 * same type.
 * Because we can prepend ->'s, it's possible to generate any program
 * this way. *)
let expand_with_pre_and_init node =
  if node_num_inputs node = 0 then node else
  let num_outputs = node_num_outputs node in
  let chosen_output = Random.int num_outputs in
  let find_input = find_index (List.nth node.node_outputs chosen_output)
                              node.node_inputs in
  if find_input = None then node else
  let Some chosen_input = find_input in
  let new_local = make_signal ("l" ^ string_of_int node.node_local_counter)
                              (List.nth node.node_outputs chosen_output) in
  make_node node.node_inputs node.node_outputs
            (new_local :: node.node_locals) (node.node_local_counter + 1)
            (fun ins outs ->
              let before, _ :: after = list_section chosen_input ins in
              print_assignment [new_local] (print_pre_and_init
                [List.nth ins chosen_input; List.nth outs chosen_output]) ^
              node.node_print (List.append before (new_local :: after)) outs)

(* Take a node and mutate it with one of the functions above. *)
let mutate_node primset = match Random.float 7.0 with
  | x when x < 1.0 -> expand_with_shuffle
  | x when x < 3.0 -> expand_with_additional_input
  | x when x < 3.5 -> __fc expand_with_omitted_outputs expand_with_shuffle
  | x when x < 4.0 -> __fc expand_with_constant_inputs expand_with_shuffle
  | x when x < 4.5 -> expand_with_output_from_copy
  | x when x < 7.0 -> expand_with_appended_primitive primset
  | x when x < 9.0 -> expand_with_prepended_primitive primset
  | x when x < 10.0 -> expand_with_pre_and_init

(* Mutate a basic node a number of times to produce
 * a sufficiently complex one. *)
let rec create_node_by_mutating primset init_lc depth = match depth with
  | 0 -> 
    let basic_node = make_node [] [] [] init_lc (fun _ _ -> "") in
    expand_with_additional_input basic_node
  | _ -> mutate_node primset
    (create_node_by_mutating primset init_lc (depth - 1))

