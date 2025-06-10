open Core
open Hardcaml
open Signal

include struct
  open Hardcaml_hobby_boards
  module Board = Board
  module Nexys_a7_100t = Nexys_a7_100t
end

open Nexys_a7_100t

let%expect_test "" =
  let board = Board.create () in
  let clocks = Clock_and_reset.create board in
  let spec = Reg_spec.create ~clock:clocks.clock_100 () in
  let audio_amp = Audio_amplifier.create board in
  Audio_amplifier.complete
    board
    { Audio_amplifier.O.sd = reg spec vdd }
    { Audio_amplifier.T.pwm =
        { With_valid.valid = reg spec ~:(audio_amp.pwm); value = gnd }
    };
  Leds.complete board (Leds.O.Of_signal.zero ());
  print_s [%message (board : Board.t)];
  [%expect
    {|
    (board
     ((cores
       ((audio_amplifier
         ((inputs ())
          (outputs ((wire (names (aud_sd)) (width 1) (data_in register))))
          (input_tristates ((wire (names (aud_pwm)) (width 1))))
          (output_tristates
           ((wire (names (aud_pwm$valid)) (width 1) (data_in register))
            (wire (names (aud_pwm$value)) (width 1) (data_in 0b0))))
          (complete true)))
        (clocking
         ((inputs
           ((wire (names (clock_100)) (width 1))
            (wire (names (reset_n)) (width 1))))
          (outputs ()) (input_tristates ()) (output_tristates ())
          (complete true)))
        (leds
         ((inputs ())
          (outputs ((wire (names (leds)) (width 16) (data_in 0x0000))))
          (input_tristates ()) (output_tristates ()) (complete true)))))
      (pins ())))
    |}]
;;
