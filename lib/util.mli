val print_from_file : string -> unit
(** [print_from_file path] prints the contexts of file at path [path] *)

val clear_screen : unit -> unit
(** [clear_screen ()] clears the terminal and history *)

val help : unit -> unit
(** [help ()] prints out the help page *)

val write_to : int -> int -> string -> string -> unit
(** [write_to time turns time_file turn_file] is the updated file containing
    highest and lowest times [time_file] file and the file containing the
    highest and lowest turns [turn_file] depending on the [time] and [turns] of
    the last user *)
