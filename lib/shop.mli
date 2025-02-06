val reward_coins : string -> unit
(**[reward_coins f] adds 5 coins to the user's inventory, filepath [f], as a
   reward for solving the cube *)

val open_shop : string -> string -> string -> int
(** [open_shop s i p] opens the shop, allowing the user to buy themes for their
    cube. Only themes not yet in the user's inventory csv [i] will be available
    in store [s]. Checks purse [p] to ascertain the user has enough money.
    Themes appear in [i] after purchase. Returning [0] indicates a shop quit *)

val open_inventory : string -> string -> string -> int
(** [open_inventory s i e] opens the inventory, allowing the user to change the
    equipped cube theme in their equipped csv [e]. Only themes availabe in the
    user's inventory csv [i] will be available. Uses the shop options csv [s] to
    access item desriptions. *)
