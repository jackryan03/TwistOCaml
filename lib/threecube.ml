open Cube

module Threecube : Cube = struct
  type 'a face = ('a * 'a * 'a) * ('a * 'a * 'a) * ('a * 'a * 'a)
  type 'a cube = 'a face * 'a face * 'a face * 'a face * 'a face * 'a face

  exception InvalidFaceIndex

  (* [get_face cube idx] takes a cube and an index, returing the face
     corresponding with the idx. Raises [InvalidFaceIndex] if [idx] not in
     {0-5}*)
  let get_face ((up, down, left, front, right, back) : 'a cube) idx =
    match idx with
    | 0 -> up
    | 1 -> down
    | 2 -> left
    | 3 -> front
    | 4 -> right
    | 5 -> back
    | _ -> raise InvalidFaceIndex

  (* [get_u_face cube] returns the up face of [cube]*)
  let get_u_face cube = get_face cube 0

  (* [get_d_face cube] returns the down face of [cube]*)
  let get_d_face cube = get_face cube 1

  (* [get_l_face cube] returns the left face of [cube]*)
  let get_l_face cube = get_face cube 2

  (* [get_f_face cube] returns the front face of [cube]*)
  let get_f_face cube = get_face cube 3

  (* [get_r_face cube] returns the right face of [cube]*)
  let get_r_face cube = get_face cube 4

  (* [get_b_face cube] returns the back face of [cube]*)
  let get_b_face cube = get_face cube 5

  exception InvalidStickerIndex

  (* [get_sticker cube idx] takes a cube and an index, returing the sticker
     corresponding with the idx. Raises [InvalidStickerIndex] if [idx] not in
     {(0-2, 0-2)}*)
  let get_sticker (((tl, tm, tr), (ml, mm, mr), (bl, bm, br)) : 'a face) (x, y)
      =
    match (x, y) with
    | 0, 0 -> tl
    | 0, 1 -> tm
    | 0, 2 -> tr
    | 1, 0 -> ml
    | 1, 1 -> mm
    | 1, 2 -> mr
    | 2, 0 -> bl
    | 2, 1 -> bm
    | 2, 2 -> br
    | _ -> raise InvalidStickerIndex

  (* [get_tl_sticker face] returns the top left sticker of [face]*)
  let get_tl_sticker face = get_sticker face (0, 0)

  (* [get_tm_sticker face] returns the top middle sticker of [face]*)
  let get_tm_sticker face = get_sticker face (0, 1)

  (* [get_tr_sticker face] returns the top right sticker of [face]*)
  let get_tr_sticker face = get_sticker face (0, 2)

  (* [get_ml_sticker face] returns the middle left sticker of [face]*)
  let get_ml_sticker face = get_sticker face (1, 0)

  (* [get_mm_sticker face] returns the middle middle sticker of [face]*)
  let get_mm_sticker face = get_sticker face (1, 1)

  (* [get_mr_sticker face] returns the middle right sticker of [face]*)
  let get_mr_sticker face = get_sticker face (1, 2)

  (* [get_bl_sticker face] returns the bottom left sticker of [face]*)
  let get_bl_sticker face = get_sticker face (2, 0)

  (* [get_nm_sticker face] returns the bottom middle sticker of [face]*)
  let get_bm_sticker face = get_sticker face (2, 1)

  (* [get_br_sticker face] returns the bottom right sticker of [face]*)
  let get_br_sticker face = get_sticker face (2, 2)

  (* [init_solved_face sticker] initializes a face with nine instances of string
     [sticker]*)
  let init_solved_face sticker : 'a face =
    ( (sticker, sticker, sticker),
      (sticker, sticker, sticker),
      (sticker, sticker, sticker) )

  let init_debug_cube unit : 'a cube =
    ( (("11", "12", "13"), ("14", "15", "16"), ("17", "18", "19")),
      (("61", "62", "63"), ("64", "65", "66"), ("67", "68", "69")),
      (("21", "22", "23"), ("24", "25", "26"), ("27", "28", "29")),
      (("31", "32", "33"), ("34", "35", "36"), ("37", "38", "39")),
      (("41", "42", "43"), ("44", "45", "46"), ("47", "48", "49")),
      (("51", "52", "53"), ("54", "55", "56"), ("57", "58", "59")) )

  (** [init_face ((tl, tm, tr), (ml, mm, mr), (bl, bm, br))] intializes a face
      with specific values*)
  let init_face (((tl, tm, tr), (ml, mm, mr), (bl, bm, br)) : 'a face) : 'a face
      =
    ((tl, tm, tr), (ml, mm, mr), (bl, bm, br))

  type sticker = {
    white : string;
    yellow : string;
    orange : string;
    green : string;
    red : string;
    blue : string;
  }

  let stick_help n =
    let slist = Csv.load "data/stickers.csv" in
    let setlist = List.nth slist (n - 1) in
    let stickerset =
      {
        white = List.nth setlist 0;
        yellow = List.nth setlist 1;
        orange = List.nth setlist 2;
        green = List.nth setlist 3;
        red = List.nth setlist 4;
        blue = List.nth setlist 5;
      }
    in
    stickerset

  let stick_help_test n =
    let slist = Csv.load "../data/stickers.csv" in
    let setlist = List.nth slist (n - 1) in
    let stickerset =
      {
        white = List.nth setlist 0;
        yellow = List.nth setlist 1;
        orange = List.nth setlist 2;
        green = List.nth setlist 3;
        red = List.nth setlist 4;
        blue = List.nth setlist 5;
      }
    in
    stickerset

  let set_stickers n : sticker = stick_help n
  let set_stickers_test n : sticker = stick_help_test n

  let init_solved_cube stickers : 'a cube =
    ( (*white*) init_solved_face stickers.white,
      (*yellow*) init_solved_face stickers.yellow,
      (*orange*) init_solved_face stickers.orange,
      (*green*) init_solved_face stickers.green,
      (*red*) init_solved_face stickers.red,
      (*blue*) init_solved_face stickers.blue )

  let flat_print_face face =
    Printf.printf
      {|
 %s | %s | %s 
---+---+---
 %s | %s | %s 
---+---+---
 %s | %s | %s 
|}
      (get_tl_sticker face) (get_tm_sticker face) (get_tr_sticker face)
      (get_ml_sticker face) (get_mm_sticker face) (get_mr_sticker face)
      (get_bl_sticker face) (get_bm_sticker face) (get_br_sticker face)

  let flat_print_cube cube =
    let uface = get_u_face cube in
    let dface = get_d_face cube in
    let lface = get_l_face cube in
    let fface = get_f_face cube in
    let rface = get_r_face cube in
    let bface = get_b_face cube in
    Printf.printf
      {|
              +---+---+---+
              | %s | %s | %s |
              +---•---•---+
              | %s | %s | %s |
              +---•---•---+
              | %s | %s | %s |
  +---+---+---+---+---+---+---+---+---+---+---+---+
  | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |
  +---•---•---+---•---•---+---•---•---+---•---•---+
  | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |
  +---•---•---+---•---•---+---•---•---+---•---•---+
  | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |
  +---+---+---+---+---+---+---+---+---+---+---+---+
              | %s | %s | %s |
              +---•---•---+
              | %s | %s | %s |
              +---•---•---+
              | %s | %s | %s |
              +---+---+---+
  |}
      (get_tl_sticker uface) (get_tm_sticker uface) (get_tr_sticker uface)
      (get_ml_sticker uface) (get_mm_sticker uface) (get_mr_sticker uface)
      (get_bl_sticker uface) (get_bm_sticker uface) (get_br_sticker uface)
      (get_tl_sticker lface) (get_tm_sticker lface) (get_tr_sticker lface)
      (get_tl_sticker fface) (get_tm_sticker fface) (get_tr_sticker fface)
      (get_tl_sticker rface) (get_tm_sticker rface) (get_tr_sticker rface)
      (get_tl_sticker bface) (get_tm_sticker bface) (get_tr_sticker bface)
      (get_ml_sticker lface) (get_mm_sticker lface) (get_mr_sticker lface)
      (get_ml_sticker fface) (get_mm_sticker fface) (get_mr_sticker fface)
      (get_ml_sticker rface) (get_mm_sticker rface) (get_mr_sticker rface)
      (get_ml_sticker bface) (get_mm_sticker bface) (get_mr_sticker bface)
      (get_bl_sticker lface) (get_bm_sticker lface) (get_br_sticker lface)
      (get_bl_sticker fface) (get_bm_sticker fface) (get_br_sticker fface)
      (get_bl_sticker rface) (get_bm_sticker rface) (get_br_sticker rface)
      (get_bl_sticker bface) (get_bm_sticker bface) (get_br_sticker bface)
      (get_tl_sticker dface) (get_tm_sticker dface) (get_tr_sticker dface)
      (get_ml_sticker dface) (get_mm_sticker dface) (get_mr_sticker dface)
      (get_bl_sticker dface) (get_bm_sticker dface) (get_br_sticker dface)

  let flat_print_wide_cube cube =
    let uface = get_u_face cube in
    let dface = get_d_face cube in
    let lface = get_l_face cube in
    let fface = get_f_face cube in
    let rface = get_r_face cube in
    let bface = get_b_face cube in
    Printf.printf
      {|
                     +----+----+----+
                     | %s | %s | %s |
                     +----•----•----+
                     | %s | %s | %s |
                     +----•----•----+
                     | %s | %s | %s |
      +----+----+----+----+----+----+----+----+----+----+----+----+
      | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |
      +----•----•----+----•----•----+----•----•----+----•----•----+
      | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |
      +----•----•----+----•----•----+----•----•----+----•----•----+
      | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |
      +----+----+----+----+----+----+----+----+----+----+----+----+
                     | %s | %s | %s |
                     +----•----•----+
                     | %s | %s | %s |
                     +----•----•----+
                     | %s | %s | %s |
                     +----+----+----+
      |}
      (get_tl_sticker uface) (get_tm_sticker uface) (get_tr_sticker uface)
      (get_ml_sticker uface) (get_mm_sticker uface) (get_mr_sticker uface)
      (get_bl_sticker uface) (get_bm_sticker uface) (get_br_sticker uface)
      (get_tl_sticker lface) (get_tm_sticker lface) (get_tr_sticker lface)
      (get_tl_sticker fface) (get_tm_sticker fface) (get_tr_sticker fface)
      (get_tl_sticker rface) (get_tm_sticker rface) (get_tr_sticker rface)
      (get_tl_sticker bface) (get_tm_sticker bface) (get_tr_sticker bface)
      (get_ml_sticker lface) (get_mm_sticker lface) (get_mr_sticker lface)
      (get_ml_sticker fface) (get_mm_sticker fface) (get_mr_sticker fface)
      (get_ml_sticker rface) (get_mm_sticker rface) (get_mr_sticker rface)
      (get_ml_sticker bface) (get_mm_sticker bface) (get_mr_sticker bface)
      (get_bl_sticker lface) (get_bm_sticker lface) (get_br_sticker lface)
      (get_bl_sticker fface) (get_bm_sticker fface) (get_br_sticker fface)
      (get_bl_sticker rface) (get_bm_sticker rface) (get_br_sticker rface)
      (get_bl_sticker bface) (get_bm_sticker bface) (get_br_sticker bface)
      (get_tl_sticker dface) (get_tm_sticker dface) (get_tr_sticker dface)
      (get_ml_sticker dface) (get_mm_sticker dface) (get_mr_sticker dface)
      (get_bl_sticker dface) (get_bm_sticker dface) (get_br_sticker dface)

  let twod_print_cube cube =
    let uface = get_u_face cube in
    let fface = get_f_face cube in
    let rface = get_r_face cube in
    Printf.printf
      {|
                   +----+----+----+
                 / %s | %s | %s / 
              / %s | %s | %s /    %s
           / %s | %s | %s /    %s
          +----+----+----+  %s    %s
          | %s | %s | %s |     %s
          +----•----•----+  %s    %s
          | %s | %s | %s |     %s
          +----•----•----+  %s
          | %s | %s | %s |
          +----+----+----+
          |}
      (get_tl_sticker uface) (get_tm_sticker uface) (get_tr_sticker uface)
      (get_ml_sticker uface) (get_mm_sticker uface) (get_mr_sticker uface)
      (get_tr_sticker rface) (get_bl_sticker uface) (get_bm_sticker uface)
      (get_br_sticker uface) (get_tm_sticker rface) (get_tl_sticker rface)
      (get_mr_sticker rface) (get_tl_sticker fface) (get_tm_sticker fface)
      (get_tr_sticker fface) (get_mm_sticker rface) (get_ml_sticker rface)
      (get_br_sticker rface) (get_ml_sticker fface) (get_mm_sticker fface)
      (get_mr_sticker fface) (get_bm_sticker rface) (get_bl_sticker rface)
      (get_bl_sticker fface) (get_bm_sticker fface) (get_br_sticker fface)

  (* [rot_face_cw face] returns the result of a clockwise rotation on face
     [face]*)
  let rot_face_cw (((tl, tm, tr), (ml, mm, mr), (bl, bm, br)) : 'a face) :
      'a face =
    ((bl, ml, tl), (bm, mm, tm), (br, mr, tr))

  (* [rot_face_ccw face] returns the result of a counterclockwise rotation on
     face [face]*)
  let rot_face_ccw (((tl, tm, tr), (ml, mm, mr), (bl, bm, br)) : 'a face) :
      'a face =
    ((tr, mr, br), (tm, mm, bm), (tl, ml, bl))

  let turn_r ((uface, dface, lface, fface, rface, bface) : 'a cube) : 'a cube =
    let new_uface =
      ( (get_tl_sticker uface, get_tm_sticker uface, get_tr_sticker fface),
        (get_ml_sticker uface, get_mm_sticker uface, get_mr_sticker fface),
        (get_bl_sticker uface, get_bm_sticker uface, get_br_sticker fface) )
    in
    let new_dface =
      ( (get_tl_sticker dface, get_tm_sticker dface, get_bl_sticker bface),
        (get_ml_sticker dface, get_mm_sticker dface, get_ml_sticker bface),
        (get_bl_sticker dface, get_bm_sticker dface, get_tl_sticker bface) )
    in
    let new_lface = lface in
    let new_fface =
      ( (get_tl_sticker fface, get_tm_sticker fface, get_tr_sticker dface),
        (get_ml_sticker fface, get_mm_sticker fface, get_mr_sticker dface),
        (get_bl_sticker fface, get_bm_sticker fface, get_br_sticker dface) )
    in
    let new_rface = rot_face_cw rface in
    let new_bface =
      ( (get_br_sticker uface, get_tm_sticker bface, get_tr_sticker bface),
        (get_mr_sticker uface, get_mm_sticker bface, get_mr_sticker bface),
        (get_tr_sticker uface, get_bm_sticker bface, get_br_sticker bface) )
    in
    (new_uface, new_dface, new_lface, new_fface, new_rface, new_bface)

  let turn_r_prime ((uface, dface, lface, fface, rface, bface) : 'a cube) :
      'a cube =
    let new_uface =
      ( (get_tl_sticker uface, get_tm_sticker uface, get_bl_sticker bface),
        (get_ml_sticker uface, get_mm_sticker uface, get_ml_sticker bface),
        (get_bl_sticker uface, get_bm_sticker uface, get_tl_sticker bface) )
    in
    let new_dface =
      ( (get_tl_sticker dface, get_tm_sticker dface, get_tr_sticker fface),
        (get_ml_sticker dface, get_mm_sticker dface, get_mr_sticker fface),
        (get_bl_sticker dface, get_bm_sticker dface, get_br_sticker fface) )
    in
    let new_lface = lface in
    let new_fface =
      ( (get_tl_sticker fface, get_tm_sticker fface, get_tr_sticker uface),
        (get_ml_sticker fface, get_mm_sticker fface, get_mr_sticker uface),
        (get_bl_sticker fface, get_bm_sticker fface, get_br_sticker uface) )
    in
    let new_rface = rot_face_ccw rface in
    let new_bface =
      ( (get_br_sticker dface, get_tm_sticker bface, get_tr_sticker bface),
        (get_mr_sticker dface, get_mm_sticker bface, get_mr_sticker bface),
        (get_tr_sticker dface, get_bm_sticker bface, get_br_sticker bface) )
    in
    (new_uface, new_dface, new_lface, new_fface, new_rface, new_bface)

  let turn_u ((uface, dface, lface, fface, rface, bface) : 'a cube) : 'a cube =
    let new_uface = rot_face_cw uface in
    let new_dface = dface in
    let new_lface =
      ( (get_tl_sticker fface, get_tm_sticker fface, get_tr_sticker fface),
        (get_ml_sticker lface, get_mm_sticker lface, get_mr_sticker lface),
        (get_bl_sticker lface, get_bm_sticker lface, get_br_sticker lface) )
    in
    let new_fface =
      ( (get_tl_sticker rface, get_tm_sticker rface, get_tr_sticker rface),
        (get_ml_sticker fface, get_mm_sticker fface, get_mr_sticker fface),
        (get_bl_sticker fface, get_bm_sticker fface, get_br_sticker fface) )
    in
    let new_rface =
      ( (get_tl_sticker bface, get_tm_sticker bface, get_tr_sticker bface),
        (get_ml_sticker rface, get_mm_sticker rface, get_mr_sticker rface),
        (get_bl_sticker rface, get_bm_sticker rface, get_br_sticker rface) )
    in
    let new_bface =
      ( (get_tl_sticker lface, get_tm_sticker lface, get_tr_sticker lface),
        (get_ml_sticker bface, get_mm_sticker bface, get_mr_sticker bface),
        (get_bl_sticker bface, get_bm_sticker bface, get_br_sticker bface) )
    in
    (new_uface, new_dface, new_lface, new_fface, new_rface, new_bface)

  let turn_u_prime ((uface, dface, lface, fface, rface, bface) : 'a cube) :
      'a cube =
    let new_uface = rot_face_ccw uface in
    let new_dface = dface in
    let new_lface =
      ( (get_tl_sticker bface, get_tm_sticker bface, get_tr_sticker bface),
        (get_ml_sticker lface, get_mm_sticker lface, get_mr_sticker lface),
        (get_bl_sticker lface, get_bm_sticker lface, get_br_sticker lface) )
    in
    let new_fface =
      ( (get_tl_sticker lface, get_tm_sticker lface, get_tr_sticker lface),
        (get_ml_sticker fface, get_mm_sticker fface, get_mr_sticker fface),
        (get_bl_sticker fface, get_bm_sticker fface, get_br_sticker fface) )
    in
    let new_rface =
      ( (get_tl_sticker fface, get_tm_sticker fface, get_tr_sticker fface),
        (get_ml_sticker rface, get_mm_sticker rface, get_mr_sticker rface),
        (get_bl_sticker rface, get_bm_sticker rface, get_br_sticker rface) )
    in
    let new_bface =
      ( (get_tl_sticker rface, get_tm_sticker rface, get_tr_sticker rface),
        (get_ml_sticker bface, get_mm_sticker bface, get_mr_sticker bface),
        (get_bl_sticker bface, get_bm_sticker bface, get_br_sticker bface) )
    in
    (new_uface, new_dface, new_lface, new_fface, new_rface, new_bface)

  let turn_f ((uface, dface, lface, fface, rface, bface) : 'a cube) : 'a cube =
    let new_uface =
      ( (get_tl_sticker uface, get_tm_sticker uface, get_tr_sticker uface),
        (get_ml_sticker uface, get_mm_sticker uface, get_mr_sticker uface),
        (get_br_sticker lface, get_mr_sticker lface, get_tr_sticker lface) )
    in
    let new_dface =
      ( (get_bl_sticker rface, get_ml_sticker rface, get_tl_sticker rface),
        (get_ml_sticker dface, get_mm_sticker dface, get_mr_sticker dface),
        (get_bl_sticker dface, get_bm_sticker dface, get_br_sticker dface) )
    in
    let new_lface =
      ( (get_tl_sticker lface, get_tm_sticker lface, get_tl_sticker dface),
        (get_ml_sticker lface, get_mm_sticker lface, get_tm_sticker dface),
        (get_bl_sticker lface, get_bm_sticker lface, get_tr_sticker dface) )
    in
    let new_fface = rot_face_cw fface in
    let new_rface =
      ( (get_bl_sticker uface, get_tm_sticker rface, get_tr_sticker rface),
        (get_bm_sticker uface, get_mm_sticker rface, get_mr_sticker rface),
        (get_br_sticker uface, get_bm_sticker rface, get_br_sticker rface) )
    in
    let new_bface = bface in
    (new_uface, new_dface, new_lface, new_fface, new_rface, new_bface)

  let turn_f_prime ((uface, dface, lface, fface, rface, bface) : 'a cube) :
      'a cube =
    let new_uface =
      ( (get_tl_sticker uface, get_tm_sticker uface, get_tr_sticker uface),
        (get_ml_sticker uface, get_mm_sticker uface, get_mr_sticker uface),
        (get_tl_sticker rface, get_ml_sticker rface, get_bl_sticker rface) )
    in
    let new_dface =
      ( (get_tr_sticker lface, get_mr_sticker lface, get_br_sticker lface),
        (get_ml_sticker dface, get_mm_sticker dface, get_mr_sticker dface),
        (get_bl_sticker dface, get_bm_sticker dface, get_br_sticker dface) )
    in
    let new_lface =
      ( (get_tl_sticker lface, get_tm_sticker lface, get_br_sticker uface),
        (get_ml_sticker lface, get_mm_sticker lface, get_bm_sticker uface),
        (get_bl_sticker lface, get_bm_sticker lface, get_bl_sticker uface) )
    in
    let new_fface = rot_face_ccw fface in
    let new_rface =
      ( (get_tr_sticker dface, get_tm_sticker rface, get_tr_sticker rface),
        (get_tm_sticker dface, get_mm_sticker rface, get_mr_sticker rface),
        (get_tl_sticker dface, get_bm_sticker rface, get_br_sticker rface) )
    in
    let new_bface = bface in
    (new_uface, new_dface, new_lface, new_fface, new_rface, new_bface)

  let turn_b ((uface, dface, lface, fface, rface, bface) : 'a cube) : 'a cube =
    let new_uface =
      ( (get_tr_sticker rface, get_mr_sticker rface, get_br_sticker rface),
        (get_ml_sticker uface, get_mm_sticker uface, get_mr_sticker uface),
        (get_bl_sticker uface, get_bm_sticker uface, get_br_sticker uface) )
    in
    let new_dface =
      ( (get_tl_sticker dface, get_tm_sticker dface, get_tr_sticker dface),
        (get_ml_sticker dface, get_mm_sticker dface, get_mr_sticker dface),
        (get_tl_sticker lface, get_ml_sticker lface, get_bl_sticker lface) )
    in
    let new_lface =
      ( (get_tr_sticker uface, get_tm_sticker lface, get_tr_sticker lface),
        (get_tm_sticker uface, get_mm_sticker lface, get_mr_sticker lface),
        (get_tl_sticker uface, get_bm_sticker lface, get_br_sticker lface) )
    in
    let new_fface = fface in
    let new_rface =
      ( (get_tl_sticker rface, get_tm_sticker rface, get_br_sticker dface),
        (get_ml_sticker rface, get_mm_sticker rface, get_bm_sticker dface),
        (get_bl_sticker rface, get_bm_sticker rface, get_bl_sticker dface) )
    in
    let new_bface = rot_face_cw bface in
    (new_uface, new_dface, new_lface, new_fface, new_rface, new_bface)

  let turn_b_prime ((uface, dface, lface, fface, rface, bface) : 'a cube) :
      'a cube =
    let new_uface =
      ( (get_bl_sticker lface, get_ml_sticker lface, get_tl_sticker lface),
        (get_ml_sticker uface, get_mm_sticker uface, get_mr_sticker uface),
        (get_bl_sticker uface, get_bm_sticker uface, get_br_sticker uface) )
    in
    let new_dface =
      ( (get_tl_sticker dface, get_tm_sticker dface, get_tr_sticker dface),
        (get_ml_sticker dface, get_mm_sticker dface, get_mr_sticker dface),
        (get_br_sticker rface, get_mr_sticker rface, get_tr_sticker rface) )
    in
    let new_lface =
      ( (get_bl_sticker dface, get_tm_sticker lface, get_tr_sticker lface),
        (get_bm_sticker dface, get_mm_sticker lface, get_mr_sticker lface),
        (get_br_sticker dface, get_bm_sticker lface, get_br_sticker lface) )
    in
    let new_fface = fface in
    let new_rface =
      ( (get_tl_sticker rface, get_tm_sticker rface, get_tl_sticker uface),
        (get_ml_sticker rface, get_mm_sticker rface, get_tm_sticker uface),
        (get_bl_sticker rface, get_bm_sticker rface, get_tr_sticker uface) )
    in
    let new_bface = rot_face_ccw bface in
    (new_uface, new_dface, new_lface, new_fface, new_rface, new_bface)

  let turn_d ((uface, dface, lface, fface, rface, bface) : 'a cube) : 'a cube =
    let new_uface = uface in
    let new_dface = rot_face_cw dface in
    let new_lface =
      ( (get_tl_sticker lface, get_tm_sticker lface, get_tr_sticker lface),
        (get_ml_sticker lface, get_mm_sticker lface, get_mr_sticker lface),
        (get_bl_sticker bface, get_bm_sticker bface, get_br_sticker bface) )
    in
    let new_fface =
      ( (get_tl_sticker fface, get_tm_sticker fface, get_tr_sticker fface),
        (get_ml_sticker fface, get_mm_sticker fface, get_mr_sticker fface),
        (get_bl_sticker lface, get_bm_sticker lface, get_br_sticker lface) )
    in
    let new_rface =
      ( (get_tl_sticker rface, get_tm_sticker rface, get_tr_sticker rface),
        (get_ml_sticker rface, get_mm_sticker rface, get_mr_sticker rface),
        (get_bl_sticker fface, get_bm_sticker fface, get_br_sticker fface) )
    in
    let new_bface =
      ( (get_tl_sticker bface, get_tm_sticker bface, get_tr_sticker bface),
        (get_ml_sticker bface, get_mm_sticker bface, get_mr_sticker bface),
        (get_bl_sticker rface, get_bm_sticker rface, get_br_sticker rface) )
    in
    (new_uface, new_dface, new_lface, new_fface, new_rface, new_bface)

  let turn_d_prime ((uface, dface, lface, fface, rface, bface) : 'a cube) :
      'a cube =
    let new_uface = uface in
    let new_dface = rot_face_ccw dface in
    let new_lface =
      ( (get_tl_sticker lface, get_tm_sticker lface, get_tr_sticker lface),
        (get_ml_sticker lface, get_mm_sticker lface, get_mr_sticker lface),
        (get_bl_sticker fface, get_bm_sticker fface, get_br_sticker fface) )
    in
    let new_fface =
      ( (get_tl_sticker fface, get_tm_sticker fface, get_tr_sticker fface),
        (get_ml_sticker fface, get_mm_sticker fface, get_mr_sticker fface),
        (get_bl_sticker rface, get_bm_sticker rface, get_br_sticker rface) )
    in
    let new_rface =
      ( (get_tl_sticker rface, get_tm_sticker rface, get_tr_sticker rface),
        (get_ml_sticker rface, get_mm_sticker rface, get_mr_sticker rface),
        (get_bl_sticker bface, get_bm_sticker bface, get_br_sticker bface) )
    in
    let new_bface =
      ( (get_tl_sticker bface, get_tm_sticker bface, get_tr_sticker bface),
        (get_ml_sticker bface, get_mm_sticker bface, get_mr_sticker bface),
        (get_bl_sticker lface, get_bm_sticker lface, get_br_sticker lface) )
    in
    (new_uface, new_dface, new_lface, new_fface, new_rface, new_bface)

  let turn_l ((uface, dface, lface, fface, rface, bface) : 'a cube) : 'a cube =
    let new_uface =
      ( (get_br_sticker bface, get_tm_sticker uface, get_tr_sticker uface),
        (get_mr_sticker bface, get_mm_sticker uface, get_mr_sticker uface),
        (get_tr_sticker bface, get_bm_sticker uface, get_br_sticker uface) )
    in
    let new_dface =
      ( (get_tl_sticker fface, get_tm_sticker dface, get_tr_sticker dface),
        (get_ml_sticker fface, get_mm_sticker dface, get_mr_sticker dface),
        (get_bl_sticker fface, get_bm_sticker dface, get_br_sticker dface) )
    in
    let new_lface = rot_face_cw lface in
    let new_fface =
      ( (get_tl_sticker uface, get_tm_sticker fface, get_tr_sticker fface),
        (get_ml_sticker uface, get_mm_sticker fface, get_mr_sticker fface),
        (get_bl_sticker uface, get_bm_sticker fface, get_br_sticker fface) )
    in
    let new_rface = rface in
    let new_bface =
      ( (get_tl_sticker bface, get_tm_sticker bface, get_bl_sticker dface),
        (get_ml_sticker bface, get_mm_sticker bface, get_ml_sticker dface),
        (get_bl_sticker bface, get_bm_sticker bface, get_tl_sticker dface) )
    in
    (new_uface, new_dface, new_lface, new_fface, new_rface, new_bface)

  let turn_l_prime ((uface, dface, lface, fface, rface, bface) : 'a cube) :
      'a cube =
    let new_uface =
      ( (get_tl_sticker fface, get_tm_sticker uface, get_tr_sticker uface),
        (get_ml_sticker fface, get_mm_sticker uface, get_mr_sticker uface),
        (get_bl_sticker fface, get_bm_sticker uface, get_br_sticker uface) )
    in
    let new_dface =
      ( (get_br_sticker bface, get_tm_sticker dface, get_tr_sticker dface),
        (get_mr_sticker bface, get_mm_sticker dface, get_mr_sticker dface),
        (get_tr_sticker bface, get_bm_sticker dface, get_br_sticker dface) )
    in
    let new_lface = rot_face_ccw lface in
    let new_fface =
      ( (get_tl_sticker dface, get_tm_sticker fface, get_tr_sticker fface),
        (get_ml_sticker dface, get_mm_sticker fface, get_mr_sticker fface),
        (get_bl_sticker dface, get_bm_sticker fface, get_br_sticker fface) )
    in
    let new_rface = rface in
    let new_bface =
      ( (get_tl_sticker bface, get_tm_sticker bface, get_bl_sticker uface),
        (get_ml_sticker bface, get_mm_sticker bface, get_ml_sticker uface),
        (get_bl_sticker bface, get_bm_sticker bface, get_tl_sticker uface) )
    in
    (new_uface, new_dface, new_lface, new_fface, new_rface, new_bface)

  let rot_x ((uface, dface, lface, fface, rface, bface) : 'a cube) : 'a cube =
    let new_fface = dface in
    let new_uface = fface in
    let new_bface = rot_face_cw (rot_face_cw uface) in
    let new_dface = rot_face_cw (rot_face_cw bface) in
    let new_lface = rot_face_ccw lface in
    let new_rface = rot_face_cw rface in
    (new_uface, new_dface, new_lface, new_fface, new_rface, new_bface)

  let rot_x_prime ((uface, dface, lface, fface, rface, bface) : 'a cube) :
      'a cube =
    let new_fface = uface in
    let new_dface = fface in
    let new_bface = rot_face_cw (rot_face_cw dface) in
    let new_uface = rot_face_cw (rot_face_cw bface) in
    let new_lface = rot_face_cw lface in
    let new_rface = rot_face_ccw rface in
    (new_uface, new_dface, new_lface, new_fface, new_rface, new_bface)

  let rot_y ((uface, dface, lface, fface, rface, bface) : 'a cube) : 'a cube =
    let new_fface = rface in
    let new_rface = bface in
    let new_bface = lface in
    let new_lface = fface in
    let new_uface = rot_face_cw uface in
    let new_dface = rot_face_ccw dface in
    (new_uface, new_dface, new_lface, new_fface, new_rface, new_bface)

  let rot_y_prime ((uface, dface, lface, fface, rface, bface) : 'a cube) :
      'a cube =
    let new_fface = lface in
    let new_lface = bface in
    let new_bface = rface in
    let new_rface = fface in
    let new_uface = rot_face_ccw uface in
    let new_dface = rot_face_cw dface in
    (new_uface, new_dface, new_lface, new_fface, new_rface, new_bface)

  let rot_z ((uface, dface, lface, fface, rface, bface) : 'a cube) : 'a cube =
    let new_rface = rot_face_cw uface in
    let new_uface = rot_face_cw lface in
    let new_lface = rot_face_cw dface in
    let new_dface = rot_face_cw rface in
    let new_fface = rot_face_cw fface in
    let new_bface = rot_face_ccw bface in
    (new_uface, new_dface, new_lface, new_fface, new_rface, new_bface)

  let rot_z_prime ((uface, dface, lface, fface, rface, bface) : 'a cube) :
      'a cube =
    let new_lface = rot_face_ccw uface in
    let new_uface = rot_face_ccw rface in
    let new_rface = rot_face_ccw dface in
    let new_dface = rot_face_ccw lface in
    let new_fface = rot_face_ccw fface in
    let new_bface = rot_face_cw bface in
    (new_uface, new_dface, new_lface, new_fface, new_rface, new_bface)

  let is_solved ((uface, dface, lface, fface, rface, bface) : 'a cube) : bool =
    if
      init_solved_face (get_tl_sticker uface) = uface
      && init_solved_face (get_tl_sticker dface) = dface
      && init_solved_face (get_tl_sticker lface) = lface
      && init_solved_face (get_tl_sticker rface) = rface
      && init_solved_face (get_tl_sticker bface) = bface
    then true
    else false

  let print_face_from_string_list (face_list : string list list) : unit =
    match face_list with
    | [ [ tl; tm; tr ]; [ ml; mm; mr ]; [ bl; bm; br ] ] ->
        Printf.printf
          {|
  %s | %s | %s 
  ---+---+---
  %s | %s | %s 
  ---+---+---
  %s | %s | %s 
  |}
          tl tm tr ml mm mr bl bm br
    | _ -> failwith "Not a face"

  let string_list_list_of_face face =
    [
      [ get_tl_sticker face; get_tm_sticker face; get_tr_sticker face ];
      [ get_ml_sticker face; get_mm_sticker face; get_mr_sticker face ];
      [ get_bl_sticker face; get_bm_sticker face; get_br_sticker face ];
    ]

  let face_equal (a : 'a face) (b : 'a face) fcn =
    List.equal (List.equal fcn)
      (string_list_list_of_face a)
      (string_list_list_of_face b)

  let front_equal (a : 'a cube) (b : 'a cube) fcn =
    let af = get_f_face a in
    let bf = get_f_face b in
    face_equal af bf fcn

  let tuple_from_list lst =
    match lst with
    | [] -> failwith "tuple_from_list given an empty list"
    | h :: t -> (
        match t with
        | [] -> failwith "tuple_from_list given list of 1 element"
        | h1 :: t1 -> (
            match t1 with
            | [] -> failwith "tuple_from_list given list of 2 elements"
            | h2 :: t2 -> (
                match t2 with
                | [] -> (h, h1, h2)
                | h :: t -> failwith "tuple_from_list given list of >3 elements"
                )))

  let face_from_list lst : 'a face =
    match lst with
    | [] -> failwith "face_from_list given an empty list"
    | h :: t -> (
        match t with
        | [] -> failwith "face_from_list given list of 1 element"
        | h1 :: t1 -> (
            match t1 with
            | [] -> failwith "face_from_list given list of 2 elements"
            | h2 :: t2 -> (
                match t2 with
                | [] ->
                    (tuple_from_list h, tuple_from_list h1, tuple_from_list h2)
                | h :: t -> failwith "face_from_list given list of >3 elements")
            ))

  let null_face = (("", "", ""), ("", "", ""), ("", "", ""))
end
