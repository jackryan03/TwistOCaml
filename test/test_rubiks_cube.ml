open OUnit2
open Rubiks_cube
open Threecube
open Twocube

let three_by_test_cube = Threecube.init_solved_cube (Threecube.set_stickers_test 1)
let two_by_test_cube = Twocube.init_solved_cube (Twocube.set_stickers_test 1)

let cw_ccw_once_same_test cw_move ccw_move test_cube =
  assert_equal (cw_move (ccw_move test_cube)) test_cube

let cw_ccw_twice_same_test cw_move ccw_move test_cube =
  assert_equal (cw_move (cw_move (ccw_move (ccw_move test_cube)))) test_cube

let cw_ccw_three_same_test cw_move ccw_move test_cube =
  assert_equal
    (cw_move (cw_move (cw_move (ccw_move (ccw_move (ccw_move test_cube))))))
    test_cube

let cw_ccw_four_same_test cw_move ccw_move test_cube =
  assert_equal
    (cw_move
       (cw_move
          (cw_move
             (cw_move (ccw_move (ccw_move (ccw_move (ccw_move test_cube))))))))
    test_cube

let four_times_same_test move test_cube =
  assert_equal (move (move (move (move test_cube)))) test_cube

let one_ccw_three_cw_same_test cw_move ccw_move test_cube =
  assert_equal (cw_move (cw_move (cw_move test_cube))) (ccw_move test_cube)

let two_ccw_two_cw_same_test cw_move ccw_move test_cube =
  assert_equal (cw_move (cw_move test_cube)) (ccw_move (ccw_move test_cube))

let move_multi_tests cw_move ccw_move test_cube =
  cw_ccw_once_same_test cw_move ccw_move test_cube;
  cw_ccw_twice_same_test cw_move ccw_move test_cube;
  cw_ccw_three_same_test cw_move ccw_move test_cube;
  cw_ccw_four_same_test cw_move ccw_move test_cube;
  cw_ccw_once_same_test ccw_move cw_move test_cube;
  cw_ccw_twice_same_test ccw_move cw_move test_cube;
  cw_ccw_three_same_test ccw_move cw_move test_cube;
  cw_ccw_four_same_test ccw_move cw_move test_cube;
  four_times_same_test cw_move test_cube;
  four_times_same_test ccw_move test_cube;
  one_ccw_three_cw_same_test cw_move ccw_move test_cube;
  one_ccw_three_cw_same_test ccw_move cw_move test_cube;
  two_ccw_two_cw_same_test cw_move ccw_move test_cube;
  two_ccw_two_cw_same_test cw_move ccw_move test_cube

let tests =
  "test suite"
  >::: [
         ( "is_solved with a solved cube 3x3" >:: fun _ ->
           assert_equal
             (Threecube.is_solved
                (Threecube.init_solved_cube (Threecube.set_stickers_test 1)))
             true );
         ( "is_solved with an unsolved cube 3x3" >:: fun _ ->
           assert_equal
             (Threecube.is_solved
                (Threecube.turn_b
                   (Threecube.init_solved_cube (Threecube.set_stickers_test 1))))
             false );
         ( "rot_x and rot_x_prime tests 3x3" >:: fun _ ->
           move_multi_tests Threecube.rot_x Threecube.rot_x_prime
             three_by_test_cube );
         ( "rot_y and rot_y_prime tests 3x3" >:: fun _ ->
           move_multi_tests Threecube.rot_y Threecube.rot_y_prime
             three_by_test_cube );
         ( "rot_z and rot_z_prime tests 3x3" >:: fun _ ->
           move_multi_tests Threecube.rot_z Threecube.rot_z_prime
             three_by_test_cube );
         ( "turn_b and turn_b_prime tests  3x3" >:: fun _ ->
           move_multi_tests Threecube.turn_b Threecube.turn_b_prime
             three_by_test_cube );
         ( "turn_d and turn_d_prime tests 3x3" >:: fun _ ->
           move_multi_tests Threecube.turn_d Threecube.turn_d_prime
             three_by_test_cube );
         ( "turn_l and turn_l_prime tests 3x3" >:: fun _ ->
           move_multi_tests Threecube.turn_l Threecube.turn_l_prime
             three_by_test_cube );
         ( "turn_f and turn_f_prime tests 3x3" >:: fun _ ->
           move_multi_tests Threecube.turn_f Threecube.turn_f_prime
             three_by_test_cube );
         ( "turn_u and turn_u_prime tests 3x3" >:: fun _ ->
           move_multi_tests Threecube.turn_u Threecube.turn_u_prime
             three_by_test_cube );
         ( "turn_r and turn_r_prime tests 3x3" >:: fun _ ->
           move_multi_tests Threecube.turn_r Threecube.turn_r_prime
             three_by_test_cube );
         ( "is_solved with a solved cube 2x2" >:: fun _ ->
           assert_equal
             (Twocube.is_solved
                (Twocube.init_solved_cube (Twocube.set_stickers_test 1)))
             true );
         ( "is_solved with an unsolved cube 2x2" >:: fun _ ->
           assert_equal
             (Twocube.is_solved
                (Twocube.turn_b
                   (Twocube.init_solved_cube (Twocube.set_stickers_test 1))))
             false );
         ( "rot_x and rot_x_prime tests 2x2" >:: fun _ ->
           move_multi_tests Twocube.rot_x Twocube.rot_x_prime two_by_test_cube
         );
         ( "rot_y and rot_y_prime tests 2x2" >:: fun _ ->
           move_multi_tests Twocube.rot_y Twocube.rot_y_prime two_by_test_cube
         );
         ( "rot_z and rot_z_prime tests 2x2" >:: fun _ ->
           move_multi_tests Twocube.rot_z Twocube.rot_z_prime two_by_test_cube
         );
         ( "turn_b and turn_b_prime tests 2x2" >:: fun _ ->
           move_multi_tests Twocube.turn_b Twocube.turn_b_prime two_by_test_cube
         );
         ( "turn_d and turn_d_prime tests 2x2" >:: fun _ ->
           move_multi_tests Twocube.turn_d Twocube.turn_d_prime two_by_test_cube
         );
         ( "turn_l and turn_l_prime tests 2x2" >:: fun _ ->
           move_multi_tests Twocube.turn_l Twocube.turn_l_prime two_by_test_cube
         );
         ( "turn_f and turn_f_prime tests 2x2" >:: fun _ ->
           move_multi_tests Twocube.turn_f Twocube.turn_f_prime two_by_test_cube
         );
         ( "turn_u and turn_u_prime tests 2x2" >:: fun _ ->
           move_multi_tests Twocube.turn_u Twocube.turn_u_prime two_by_test_cube
         );
         ( "turn_r and turn_r_prime tests 2x2" >:: fun _ ->
           move_multi_tests Twocube.turn_r Twocube.turn_r_prime two_by_test_cube
         );
       ]

let _ = run_test_tt_main tests
