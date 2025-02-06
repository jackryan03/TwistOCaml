open Rubiks_cube
open Rubiks_cube.Util
open Batteries
open Rubiks_cube.Shop
open Threecube
open Twocube

(** [Theme] represents the current user selected sticker theme *)
let theme =
  ref (int_of_string (List.hd (List.hd (Csv.load "data/equipped.csv"))))

let turn_tally = ref 0
let start_time = Unix.time ()

(** [print_faces f] prints out each face in csv filepath [f] separated by an
    empty line. *)
let print_faces f =
  let str_csv = Csv.load f in
  for i = 0 to (Csv.lines str_csv / 3) - 1 do
    let row_1 = List.nth str_csv (i * 3) in
    let row_2 = List.nth str_csv ((i * 3) + 1) in
    let row_3 = List.nth str_csv ((i * 3) + 2) in
    Threecube.print_face_from_string_list [ row_1; row_2; row_3 ];
    print_string "\n"
  done

(** [load_rand_face f] loads a random face from a csv at path [f] *)
let load_rand_face f =
  let str_csv = Csv.load f in
  let num_cubes = (Csv.lines str_csv / 3) - 1 (*zero-based index*) in
  let n = Random.int num_cubes in
  let row_1 = List.nth str_csv (n * 3) in
  let row_2 = List.nth str_csv ((n * 3) + 1) in
  let row_3 = List.nth str_csv ((n * 3) + 2) in
  Threecube.face_from_list [ row_1; row_2; row_3 ]

(** [save_cube f face] saves the Csv.t represention of [face] to [f] *)
let save_cube f face =
  let str_csv = Csv.load f in
  let new_str = str_csv @ Threecube.string_list_list_of_face face in
  Csv.save f new_str

(**[save_with_preview f cube] shows a preview of the front face of [cube] before
   it is saved to [f] *)
let rec save_with_preview f cube =
  print_endline "Save this face to gallery? (y/n)";
  let f_face = Threecube.get_f_face cube in
  Threecube.flat_print_face f_face;
  let reply = read_line () in
  match reply with
  | "y" ->
      print_endline "Saved!";
      save_cube f f_face
  | "n" -> ()
  | _ ->
      print_endline "Unknown input";
      save_with_preview f cube

(** [cmd] is the return type of many of our functions. Used to return a value or
    an indicator to terminate a loop so user is passed back to the menu loop. *)
type cmd =
  | Continue of string Threecube.cube
  | PatternCheck of string Threecube.cube
  | ExitToMenu

(** [cmd_two] is the return type for cube functions of the two by two cube. Used
    to return a value or an indicator to terminate a loop so user is passed back
    to the menu loop. *)
type cmd_two =
  | Continue_two of string Twocube.cube
  | ExitToMenu

(** [match_command_three_by command cube] takes a single command, and performs
    the command on the given three by three cube. *)
let match_command_three_by (command : string) cube timed_ind =
  if
    command = "R" || command = "R'" || command = "U" || command = "U'"
    || command = "L" || command = "L'" || command = "D" || command = "D'"
    || command = "F" || command = "F'" || command = "B" || command = "B'"
  then Stdlib.incr turn_tally;
  match command with
  | "U" -> Continue (Threecube.turn_u cube)
  | "U'" -> Continue (Threecube.turn_u_prime cube)
  | "D" -> Continue (Threecube.turn_d cube)
  | "D'" -> Continue (Threecube.turn_d_prime cube)
  | "L" -> Continue (Threecube.turn_l cube)
  | "L'" -> Continue (Threecube.turn_l_prime cube)
  | "F" -> Continue (Threecube.turn_f cube)
  | "F'" -> Continue (Threecube.turn_f_prime cube)
  | "R" -> Continue (Threecube.turn_r cube)
  | "R'" -> Continue (Threecube.turn_r_prime cube)
  | "B" -> Continue (Threecube.turn_b cube)
  | "B'" -> Continue (Threecube.turn_b_prime cube)
  | "x" -> Continue (Threecube.rot_x cube)
  | "x'" -> Continue (Threecube.rot_x_prime cube)
  | "y" -> Continue (Threecube.rot_y cube)
  | "y'" -> Continue (Threecube.rot_y_prime cube)
  | "z" -> Continue (Threecube.rot_z cube)
  | "z'" -> Continue (Threecube.rot_z_prime cube)
  | "solve" ->
      Continue (Threecube.init_solved_cube (Threecube.set_stickers !theme))
  | "cp" -> PatternCheck cube
  | "H" ->
      help ();
      Continue cube
  | "M" ->
      let tot_time = int_of_float (Unix.time () -. start_time) in
      if timed_ind && Threecube.is_solved cube then (
        write_to !turn_tally tot_time "data/times.csv" "data/turns.csv";
        reward_coins "data/purse.csv")
      else ();
      ExitToMenu
  | "S" ->
      save_with_preview "data/gallery.csv" cube;
      Continue cube
  | _ -> Continue cube

(** [match_command_two_by command cube] takes a single command, and performs the
    command on the given two by two cube. *)
let match_command_two_by (command : string) cube =
  if
    command = "R" || command = "R'" || command = "U" || command = "U'"
    || command = "L" || command = "L'" || command = "D" || command = "D'"
    || command = "F" || command = "F'" || command = "B" || command = "B'"
  then Stdlib.incr turn_tally;
  match command with
  | "U" -> Continue_two (Twocube.turn_u cube)
  | "U'" -> Continue_two (Twocube.turn_u_prime cube)
  | "D" -> Continue_two (Twocube.turn_d cube)
  | "D'" -> Continue_two (Twocube.turn_d_prime cube)
  | "L" -> Continue_two (Twocube.turn_l cube)
  | "L'" -> Continue_two (Twocube.turn_l_prime cube)
  | "F" -> Continue_two (Twocube.turn_f cube)
  | "F'" -> Continue_two (Twocube.turn_f_prime cube)
  | "R" -> Continue_two (Twocube.turn_r cube)
  | "R'" -> Continue_two (Twocube.turn_r_prime cube)
  | "B" -> Continue_two (Twocube.turn_b cube)
  | "B'" -> Continue_two (Twocube.turn_b_prime cube)
  | "x" -> Continue_two (Twocube.rot_x cube)
  | "x'" -> Continue_two (Twocube.rot_x_prime cube)
  | "y" -> Continue_two (Twocube.rot_y cube)
  | "y'" -> Continue_two (Twocube.rot_y_prime cube)
  | "z" -> Continue_two (Twocube.rot_z cube)
  | "z'" -> Continue_two (Twocube.rot_z_prime cube)
  | "solve" ->
      Continue_two (Twocube.init_solved_cube (Twocube.set_stickers !theme))
  | "H" ->
      help ();
      Continue_two cube
  | "M" -> ExitToMenu
  | _ -> Continue_two cube

(** [match_command_list_three_by command_list cube] takes in a string list of
    commands and applies each command in the given order using
    [match_command_three_by command cube] until all commands have been applied. *)
let rec match_command_list_three_by (command_list : string list) cube timed_ind
    =
  match command_list with
  | [] -> Continue cube
  | command :: remaining -> (
      match match_command_three_by command cube timed_ind with
      | Continue new_cube ->
          match_command_list_three_by remaining new_cube timed_ind
      | PatternCheck pcube -> PatternCheck pcube
      | ExitToMenu -> ExitToMenu)

(** [match_command_list_two_by command_list cube] takes in a string list of
    commands and applies each command in the given order using
    [match_command_two_by command cube] until all commands have been applied. *)
let rec match_command_list_two_by (command_list : string list) cube =
  match command_list with
  | [] -> Continue_two cube
  | command :: remaining -> (
      match match_command_two_by command cube with
      | Continue_two new_cube -> match_command_list_two_by remaining new_cube
      | ExitToMenu -> ExitToMenu)

(** [pattern_print] prints a random pattern from the pattern file. *)
let pattern_print () =
  let f = load_rand_face "data/patterns.csv" in
  Threecube.flat_print_face f;
  f

(** [check_pattern pface cube] checks if the front face of [cube] is equal to
    [pface]. User wins if true, loses if false.*)
let check_pattern pface cube =
  let won =
    Threecube.face_equal pface (Threecube.get_f_face cube) String.equal
  in
  let () =
    match won with
    | true -> print_endline "Correct! You won!"
    | false -> print_endline "Your cube is incorrect--you lost :("
  in
  let () = print_endline "returning to menu!" in
  won

(** [scrambled_help num] takes in an integer n, and generates n random turns
    that are returned as a string list. *)
let rec scrambled_help (num : int) =
  let random_number = Random.int 11 in
  if num = 0 then []
  else
    match random_number with
    | 0 -> "U" :: scrambled_help (num - 1)
    | 1 -> "U'" :: scrambled_help (num - 1)
    | 2 -> "L" :: scrambled_help (num - 1)
    | 3 -> "L'" :: scrambled_help (num - 1)
    | 4 -> "F" :: scrambled_help (num - 1)
    | 5 -> "F'" :: scrambled_help (num - 1)
    | 6 -> "R" :: scrambled_help (num - 1)
    | 7 -> "R'" :: scrambled_help (num - 1)
    | 8 -> "D" :: scrambled_help (num - 1)
    | 9 -> "D'" :: scrambled_help (num - 1)
    | 10 -> "B" :: scrambled_help (num - 1)
    | _ -> "B'" :: scrambled_help (num - 1)

(** [scrambed_cube cube] uses [scrambed_help] to generate 25 random turns and
    performs those turns onto the given cube. *)
let scrambled_cube cube =
  let scramble_list = scrambled_help 25 in
  let c = match_command_list_three_by scramble_list cube in
  c

let memory_tally = ref 0

(** [memory_print command] takes in one command, applies it to a solved cube
    using [match_command_three_by], prints out the resulting cube, and returns
    the command it was given. *)
let memory_print (command : string) =
  let cube = Threecube.init_solved_cube (Threecube.set_stickers !theme) in
  (match match_command_three_by command cube false with
  | Continue updated_cube -> Threecube.twod_print_cube updated_cube
  | _ -> ());
  command

(** [turn_loop cube debug] prints out a visualization of a cube, takes a string
    of user input, and performs the commands on a rubik's cube using
    [match_command_list], then repeats or ends. *)
let rec turn_loop ?pface ?mem_list ?mem_help cube debug_ind timed_ind
    (instructions : string) =
  clear_screen ();
  let () =
    match pface with
    | Some pf -> Threecube.flat_print_face pf
    | None -> ()
  in
  let new_command =
    match mem_list with
    | Some mem_l -> (
        match mem_help with
        | Some mh ->
            if not mh then memory_print (List.hd (scrambled_help 1))
            else memory_print (List.hd mem_l)
        | None -> "")
    | None ->
        Printf.printf
          "Please enter a single, or multiple space-separated commands. H for \
           help.\n\
           $->: ";
        ""
  in
  let () =
    Threecube.twod_print_cube cube;
    Printf.printf "%s" instructions
  in
  let () = print_endline "" in

  let input = read_line () in
  let input_list = String.split_on_char ' ' input in
  let rec process_commands commands current_cube =
    match commands with
    | [] -> Continue current_cube
    | command :: rest -> (
        if (not timed_ind) && command = "scramble" then
          scrambled_cube current_cube timed_ind
        else
          match match_command_three_by command current_cube timed_ind with
          | Continue new_cube -> process_commands rest new_cube
          | PatternCheck pcube -> (
              match pface with
              | Some pf -> PatternCheck pcube
              | None -> process_commands rest pcube)
          | ExitToMenu ->
              Printf.printf "Exit to menu";
              ExitToMenu)
  in
  let mtuple =
    match mem_list with
    | Some mlist ->
        if input_list = new_command :: mlist then (true, true) else (true, false)
    | None -> (false, false)
  in
  let ismem = fst mtuple in
  let matches = snd mtuple in
  let ishelp = List.hd input_list = "H" in
  let mem_lost = ismem && (not ishelp) && not matches in
  if mem_lost then (Threecube.null_face, true)
  else
    let () = Stdlib.incr memory_tally in
    let nc = process_commands input_list cube in
    match nc with
    | ExitToMenu -> (Threecube.null_face, false)
    | PatternCheck pcube -> (
        match pface with
        | Some pf -> (pf, check_pattern pf pcube)
        | None ->
            failwith
              "process_commands returned PatternMatch when pface was None")
    | Continue ncube -> (
        match pface with
        | Some pf -> turn_loop ncube debug_ind timed_ind instructions ~pface:pf
        | None -> (
            match mem_list with
            | Some ml -> (
                match ishelp with
                | true ->
                    turn_loop
                      (Threecube.init_solved_cube
                         (Threecube.set_stickers !theme))
                      debug_ind timed_ind instructions
                      ~mem_list:(new_command :: ml) ~mem_help:true
                | false ->
                    turn_loop
                      (Threecube.init_solved_cube
                         (Threecube.set_stickers !theme))
                      debug_ind timed_ind instructions
                      ~mem_list:(new_command :: ml) ~mem_help:false)
            | None -> turn_loop ncube debug_ind timed_ind instructions))

(** [turn_loop_two_by cube debug] prints out a 2x2 visualization of a cube,
    takes a string of user input, and performs the commands on a rubik's cube
    using [match_command_two_by], then repeats or ends. *)
let rec turn_loop_two_by (cube : string Twocube.cube) debug_ind timed_ind =
  clear_screen ();
  Twocube.twod_print_cube cube;
  Printf.printf
    "Please enter a single, or multiple space-separated commands. H for help.\n\
     $->: ";
  let input = read_line () in
  let input_list = String.split_on_char ' ' input in
  let rec process_commands commands current_cube =
    match commands with
    | [] -> Continue_two current_cube
    | command :: rest -> (
        match match_command_two_by command current_cube with
        | Continue_two new_cube -> process_commands rest new_cube
        | ExitToMenu ->
            Printf.printf "Exit to menu";
            ExitToMenu)
  in
  match process_commands input_list cube with
  | Continue_two new_cube -> turn_loop_two_by new_cube debug_ind timed_ind
  | ExitToMenu -> ()

(** [timed_loop] handles the zen game mode, calling turn_loop with the correct
    indicators *)
let rec timed_loop cube debug_ind =
  let instr =
    "Solve the cube. You’re being timed. Break your records – high or low!"
  in
  turn_loop cube debug_ind true instr

(** [zen_loop] handles the zen game mode, calling turn_loop with the correct
    indicators *)
let rec zen_loop cube debug_ind =
  let instructions =
    "You are in Zen mode! You now have the additional option to \n\
    \    type 'scramble' to scramble the cube."
  in
  turn_loop cube debug_ind false instructions

(** [two_by_loop] handles the 2x2 representation of the cube, which has its own
    loop *)
let rec two_by_loop cube debug_ind = turn_loop_two_by cube debug_ind false

(** [pretty_pattern_loop] handles the pattern game mode, calling turn_loop with
    the correct indicators *)
let rec pretty_pattern_loop cube =
  let instr =
    "Match the pattern above on your own cube. When done, enter cp.\n"
  in
  turn_loop cube false false instr ~pface:(pattern_print ())

(** [memory_loop] handles the memory game mode, calling turn_loop with the
    correct indicators *)
let rec memory_loop cube =
  let instr =
    "Copy the move above on your cube, then enter all the previous commands in \
     reverse chronological order, separated by spaces. If this is your first \
     move, just enter that move. IMPORTANT: if you toggled help, enter the \
     most  \n\
    \     recent commant before it TWICE as a penalty"
  in

  turn_loop cube false false instr ~mem_list:[] ~mem_help:false

(** [menu] is the lowest level loop of the program. It is was users start in and
    can quit from. Other loops are called and terminate here. *)
let rec menu (pface, won) =
  if pface <> Threecube.null_face then (
    clear_screen ();
    let () =
      match won with
      | true ->
          print_endline "you won";
          reward_coins "data/purse.csv"
      | false -> print_endline "you lost"
    in
    Threecube.print_face_from_string_list
      (Threecube.string_list_list_of_face pface);
    print_endline "press 1 to play again, 2 to exit to menu, and 3 to quit:";
    let input = read_line () in
    match input with
    | "1" ->
        menu
          (pretty_pattern_loop
             (Threecube.init_solved_cube (Threecube.set_stickers 1)))
    | "3" -> exit 0
    | _ -> menu (Threecube.null_face, false))
  else if won then (
    clear_screen ();
    let () =
      print_endline
        ("great job on memory mode. you scored "
        ^ string_of_int !memory_tally
        ^ " turns");
      memory_tally := 0
    in
    print_endline "press 1 to play again, 2 to exit to menu, and 3 to quit:";
    let input = read_line () in
    match input with
    | "1" ->
        menu
          (memory_loop
             (Threecube.init_solved_cube (Threecube.set_stickers !theme)))
    | "3" -> exit 0
    | _ -> menu (Threecube.null_face, false))
  else memory_tally := 0;
  let rec mode_select () =
    clear_screen ();
    print_from_file "data/logo.txt";
    print_endline "";
    print_from_file "data/mode_select.txt";
    print_endline "";
    let input = read_line () in
    match input with
    | "1" -> (
        let debug_ind = false in
        if debug_ind then
          let debub_cube = Threecube.init_debug_cube () in
          menu (timed_loop debub_cube debug_ind)
        else
          let c =
            scrambled_cube
              (Threecube.init_solved_cube (Threecube.set_stickers !theme))
              true
          in
          match c with
          | Continue scram_cube -> menu (timed_loop scram_cube false)
          | ExitToMenu -> menu (Threecube.null_face, false)
          | PatternCheck _ -> menu (Threecube.null_face, false))
    | "2" ->
        menu
          (zen_loop
             (Threecube.init_solved_cube (Threecube.set_stickers !theme))
             false)
    | "3" ->
        menu
          (pretty_pattern_loop
             (Threecube.init_solved_cube (Threecube.set_stickers 1)))
    | "4" ->
        menu
          (memory_loop
             (Threecube.init_solved_cube (Threecube.set_stickers !theme)))
    | "5" ->
        let _ =
          two_by_loop
            (Twocube.init_solved_cube (Twocube.set_stickers !theme))
            false
        in
        menu (Threecube.null_face, false)
    | "B" -> menu (Threecube.null_face, false)
    | _ -> mode_select ()
  in

  clear_screen ();
  print_from_file "data/logo.txt";
  print_endline "";
  print_from_file "data/menu_options.txt";
  print_endline "";
  let input = read_line () in
  match input with
  | "1" -> mode_select ()
  | "2" -> exit 0
  | "3" ->
      let shop =
        open_shop "data/shop_options.csv" "data/inventory.csv" "data/purse.csv"
      in
      if shop = 0 then menu (Threecube.null_face, false)
      else menu (Threecube.null_face, false)
  | "4" ->
      let inventory =
        open_inventory "data/shop_options.csv" "data/inventory.csv"
          "data/equipped.csv"
      in
      if inventory = 0 then (
        theme :=
          int_of_string (List.hd (List.hd (Csv.load "data/equipped.csv")));
        menu (Threecube.null_face, false))
      else menu (Threecube.null_face, false)
  | "5" -> (
      print_faces "data/gallery.csv";
      print_endline "Press any key to return to menu.";
      match read_line () with
      | _ -> menu (Threecube.null_face, false))
  | _ -> menu (Threecube.null_face, false)

let _ = menu (Threecube.null_face, false)

(* let post_pattern_menu pface won = let () = match won with | true ->
   print_endline "" | false -> print_endline "" in
   Threecube.print_face_from_string_list (Threecube.string_list_list_of_face
   pface); print_endline "press 1 to play again, 2 to exit to menu, and 3 to
   quit:"; let input = read_line () in match input with | "1" ->
   pretty_pattern_loop (Threecube.init_solved_cube (Threecube.set_stickers 0)) |
   "3" -> exit 0 | _ -> let _ = ExitToMenu in () *)
