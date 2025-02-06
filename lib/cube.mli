module type Cube = sig
  type 'a face
  (** ['a face] abtract representation of the stickers on a single face of a
      cube *)

  type 'a cube = 'a face * 'a face * 'a face * 'a face * 'a face * 'a face
  (** ['a cube] is a six-tuple of ['a face]'s, used to represent the 6 sides of
      a nxn rubik's cube. *)

  val get_f_face : string cube -> string face
  (** [get_f_face cube] returns the front face of [cube] *)

  val flat_print_face : string face -> unit
  (** [flat_print_face face] prints the nine stickers on a single side of a
      rubiks cube. *)

  val flat_print_cube : string cube -> unit
  (** [flat_print_cube cube] prints the standard flat view of a rubiks cube. In
      order from top to bottom, left to write the faces are: (U)p face, (L)eft
      face, (F)ront face, (R)ight face, (B)ack face, (D)own face. *)

  val flat_print_wide_cube : string cube -> unit
  (** [flat_print_wide_cube cube] prints the standard flat view of a rubiks
      cube, with wider (two wide) formatting.. In order from top to bottom, left
      to write the faces are: (U)p face, (L)eft face, (F)ront face, (R)ight
      face, (B)ack face, (D)own face. *)

  val twod_print_cube : string cube -> unit
  (** [twod_print_cube cube] prints a '2D' view of the rubik's cube, showing the
      front, up, and right faces. *)

  type sticker

  val set_stickers : int -> sticker
  (**[set_stickers n] sets the cube stickers to be the nth option in catalog*)

  val set_stickers_test : int -> sticker
  (**[set_stickers n] sets the cube stickers to be the nth option in catalog*)

  val init_solved_cube : sticker -> string cube
  (** [init_solved_cube ()] initializes a solved cube with the standard colored
      squares as stickers. *)

  val init_debug_cube : unit -> string cube
  (** [init_debug_cube ()] initializes a solved cube with a unique two digit
      number to track every sticker on every face. *)

  val turn_u : string cube -> string cube
  (** [turn_u cube] returns the result of performing a clockwise turn on the up
      face. *)

  val turn_u_prime : string cube -> string cube
  (** [turn_u_prime cube] returns the result of performing a counterclockwise
      turn on the up face. *)

  val turn_d : string cube -> string cube
  (** [turn_d cube] returns the result of performing a clockwise turn on the
      down face. *)

  val turn_d_prime : string cube -> string cube
  (** [turn_d_prime cube] returns the result of performing a counterclockwise
      turn on the down face. *)

  val turn_l : string cube -> string cube
  (** [turn_l cube] returns the result of performing a clockwise turn on the
      left face. *)

  val turn_l_prime : string cube -> string cube
  (** [turn_l_prime cube] returns the result of performing a counterclockwise
      turn on the left face. *)

  val turn_f : string cube -> string cube
  (** [turn_f cube] returns the result of performing a clockwise turn on the
      front face. *)

  val turn_f_prime : string cube -> string cube
  (** [turn_f_prime cube] returns the result of performing a counterclockwise
      turn on the front face. *)

  val turn_r : string cube -> string cube
  (** [turn_r cube] returns the result of performing a clockwise turn on the
      right face. *)

  val turn_r_prime : string cube -> string cube
  (** [turn_r_prime cube] returns the result of performing a counterclockwise
      turn on the right face. *)

  val turn_b : string cube -> string cube
  (** [turn_b cube] returns the result of performing a clockwise turn on the
      back face. *)

  val turn_b_prime : string cube -> string cube
  (** [turn_b_prime cube] returns the result of performing a counterclockwise
      turn on the back face. *)

  val rot_x : 'a cube -> 'a cube
  (** [rot_x cube] returns the result of performing a clockwise rotation along
      the x axis. *)

  val rot_x_prime : 'a cube -> 'a cube
  (** [rot_x_prime cube] returns the result of performing a counterclockwise
      rotation along the x axis. *)

  val rot_y : 'a cube -> 'a cube
  (** [rot_y cube] returns the result of performing a clockwise rotation along
      the y axis. *)

  val rot_y_prime : 'a cube -> 'a cube
  (** [rot_y_prime cube] returns the result of performing a counterclockwise
      rotation along the y axis. *)

  val rot_z : 'a cube -> 'a cube
  (** [rot_z cube] returns the result of performing a clockwise rotation along
      the z axis. *)

  val rot_z_prime : 'a cube -> 'a cube
  (** [rot_z_prime cube] returns the result of performing a counterclockwise
      rotation along the z axis. *)

  val is_solved : 'a cube -> bool
  (** [is_solved cube] checks [cube] is solved. *)

  val print_face_from_string_list : string list list -> unit
  (** [print_face_from_string_list face_list] prints a face from [face_list].
      Fails with ["not"]*)

  val string_list_list_of_face : string face -> string list list
  (**[string_list_list_of_face face] is the string list list representation of
     [face]. *)

  val face_equal : 'a face -> 'a face -> ('a -> 'a -> bool) -> bool
  (** [face equal face1 face2 comp_fun] checks if two faces are equivalent using
      a [comp_fun] for ['a] *)

  val front_equal : 'a cube -> 'a cube -> ('a -> 'a -> bool) -> bool
  (** [front_equal] checks if the f face of two cubes are equivalent using a
      [comp_fun] for ['a]*)

  val face_from_list : 'a list list -> 'a face

  val null_face : string face
  (** [null_face] represents an empty face *)
end
