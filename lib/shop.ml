(**[change_purse f amount] adds [amount] to the value in the purse csv [f].*)
let change_purse f amount =
  let purse = Csv.load f in
  let purse_value = int_of_string (List.hd (List.hd purse)) in
  let new_value = purse_value + amount in
  Csv.save f [ [ string_of_int new_value ] ]

let reward_coins f =
  print_endline "5 coins have been added to your purse.";
  change_purse f 5

(**[in_inventory s_index i_list] is true if the shop item index [s_index] is in
   the flattened loaded inventory csv [i_list]. *)
let rec in_inventory s_index i_list =
  match i_list with
  | [] -> false
  | h :: t -> if s_index = h then true else in_inventory s_index t

(**[buyable items s_list i_list] displays shop items that are not yet in the
   inventory. Takes the tail of the loaded shop csv [s_list] and (flattened)
   loaded inventory csv [i_list]. *)
let buyable_items s_list i_list =
  for i = 0 to List.length s_list - 1 do
    let shop_item = List.nth s_list i in
    let shop_index = List.hd shop_item in
    if in_inventory shop_index i_list then ()
    else print_endline (shop_index ^ ". " ^ List.nth shop_item 1)
  done

(**[buy_loop s_list i i_list p input] edits the inventory csv [i] to include new
   purchases by finding the matching index [input] in the tail of the loaded
   shop csv [s_list]. It removes [15 coins] from the user's purse [p]. If the
   item already exists in the flattened loaded inventory csv [i_list], the input
   is unusable, or funds are insufficient an appropriate message is printed.
   Will quit with [0] *)
let rec buy_loop s_list i i_list p input =
  match input with
  | "Q" -> 0
  | _ -> (
      (* Convert input to int *)
      let int_input = try Some (int_of_string input) with _ -> None in
      match int_input with
      | Some num when num > 0 && num <= List.length s_list ->
          (* Ensure the item is not already in the inventory *)
          if List.mem input i_list then
            let () = print_endline "Already owned." in
            let new_input = read_line () in
            buy_loop s_list i i_list p new_input
          else
            (* Check if user has enough funds *)
            let curr_funds =
              int_of_string (List.hd (List.flatten (Csv.load p)))
            in
            if curr_funds >= 15 then (
              change_purse p (-15);
              Csv.save i [ i_list @ [ input ] ];
              print_endline "Purchase added to your inventory.";
              0)
            else (
              print_endline
                "Insufficient TwistoCoins. Come back after you've solved some \
                 cubes.";
              0)
      | _ ->
          let () =
            print_endline
              "Invalid number or choice. Please enter a valid option."
          in
          let new_input = read_line () in
          buy_loop s_list i i_list p new_input)

let open_shop s i p =
  let whole_shop = Csv.load s in
  let s_list = List.tl whole_shop in
  let i_list = List.flatten (Csv.load i) in
  let coins = List.hd (List.hd (Csv.load p)) in
  let () = print_endline (List.hd (List.hd whole_shop) ^ "\n") in
  let () = buyable_items s_list i_list in
  let () =
    print_endline ("You have " ^ coins ^ " TwistoCoins at the moment.")
  in
  let () =
    print_endline
      "Purchase by typing in the number of the desired theme. Type Q to quit."
  in
  let input = read_line () in
  buy_loop s_list i i_list p input

(** [inventory_loop i_list e input] edits the equipped theme in [e] to reflect
    the user's [input]. If [input] is not a number or not a number in the
    flattened loaded inventory csv [i_list], an appropriate message is printed.
    Quits with [0]*)
let rec inventory_loop i_list e input =
  match input with
  | "Q" -> 0
  | _ -> (
      (* Convert input to int *)
      let int_input = try Some (int_of_string input) with _ -> None in
      match int_input with
      | Some int_input ->
          if in_inventory input i_list then (
            Csv.save e [ [ input ] ];
            0)
          else
            let () =
              print_endline
                "Invalid number or choice. Please enter the number of an owned \
                 theme."
            in
            let new_input = read_line () in
            inventory_loop i_list e new_input
      | _ ->
          let () =
            print_endline
              "Invalid number or choice. Please enter the number of an owned \
               theme."
          in
          let new_input = read_line () in
          inventory_loop i_list e new_input)

let open_inventory s i e =
  let s_list = List.tl (Csv.load s) in
  let i_list = List.flatten (Csv.load i) in
  let () = print_endline "Currently in inventory :" in
  for j = 0 to List.length s_list - 1 do
    if in_inventory (string_of_int (j + 1)) i_list then
      let shop_item = List.nth s_list j in
      let shop_index = List.hd shop_item in
      print_endline (shop_index ^ ". " ^ List.nth shop_item 1)
    else ()
  done;
  let () =
    print_endline
      "\n\
       Type in Q to quit. Otherwise, pick a theme to equip. (Your selection \
       will not appear for Pretty-Pattern mode)."
  in
  let input = read_line () in
  inventory_loop i_list e input
