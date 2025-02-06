let print_from_file path =
  let ic = open_in path in
  try
    while true do
      let line = input_line ic in
      print_endline line
    done
  with End_of_file -> close_in ic

let clear_screen () = Printf.printf "\027[2J\027[H\027[3J"

let rec help () =
  clear_screen ();
  print_from_file "data/command_help.txt";
  let input = read_line () in
  if input = "H" then () else help ()

(** [congrats_message x t] is the congratulatory print statement depending on
    the type [t] of record [x]. Requires that [t] is ["turns"] or ["time"]*)
let congrats_message x t =
  let intro = "You're a TwistOcamlmaster! You solved the cube " in
  if t = "turns" then
    print_endline (intro ^ "with " ^ string_of_int x ^ " turns! A new record!")
  else
    print_endline (intro ^ "in " ^ string_of_int x ^ " seconds! A new record!")

(** [insulting_message x t] is the passive aggressive print statement depending
    on the type [t] of record [x]. Requires that [t] is ["turns"] or ["time"]*)
let insulting_message x t =
  let intro = "You need more practice! You solved the cube " in
  if t = "turns" then
    print_endline (intro ^ "with " ^ string_of_int x ^ " turns. It's a record.")
  else
    print_endline (intro ^ "in " ^ string_of_int x ^ " seconds. It's a record.")

(** [single_write_to x f t] is the updated file [f] where its highest or lowest
    value is replaced if [x] is found to be lower than the lowest value or
    higher than the highest value. A message is printed depending on the type
    [t] of record attained. Requires: [f] is a valid csv filepath and [t] is
    ["time"] or ["turns"]. *)
let single_write_to x f t =
  let lst = Csv.load f in
  let curr_highest = int_of_string (List.nth (List.nth lst 0) 1) in
  let curr_lowest = int_of_string (List.nth (List.nth lst 1) 1) in
  let new_lowest =
    if x < curr_lowest then (
      congrats_message x t;
      x)
    else curr_lowest
  in
  let new_highest =
    if x > curr_highest then (
      insulting_message x t;
      x)
    else curr_highest
  in
  let new_lst =
    [
      [ "high"; string_of_int new_highest ];
      [ "lowest"; string_of_int new_lowest ];
    ]
  in
  Csv.save f new_lst

let write_to turns time time_file turn_file =
  single_write_to turns turn_file "turns";
  single_write_to time time_file "times"
