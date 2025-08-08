open Core

let nexys_a7_100t f =
  Command.basic
    ~summary:""
    [%map_open.Command
      let dir = flag "-dir" (optional string) ~doc:"[DIR] output files to DIR" in
      fun () ->
        let board = f () in
        Hardcaml_hobby_boards.Nexys_a7_100t.generate_top ?dir board]
;;

let arty_a7_35t f =
  Command.basic
    ~summary:""
    [%map_open.Command
      let dir = flag "-dir" (optional string) ~doc:"[DIR] output files to DIR" in
      fun () ->
        let board = f () in
        Hardcaml_hobby_boards.Arty_a7.generate_top ?dir ~part:`a35 board]
;;

let nexys_a7_100t =
  Command.group
    ~summary:""
    [ "blinker", nexys_a7_100t Hardcaml_hobby_boards_demos_nexys_a7_100t.Blinker.create
    ; "cylon", nexys_a7_100t Hardcaml_hobby_boards_demos_nexys_a7_100t.Cylon.create
    ; ( "seven-segment-display"
      , nexys_a7_100t
          Hardcaml_hobby_boards_demos_nexys_a7_100t.Seven_segment_display.create )
    ; "snakes", nexys_a7_100t Hardcaml_hobby_boards_demos_nexys_a7_100t.Snakes.create
    ; "uart", nexys_a7_100t Hardcaml_hobby_boards_demos_nexys_a7_100t.Uart.create
    ; "vga", nexys_a7_100t Hardcaml_hobby_boards_demos_nexys_a7_100t.Vga.create
    ]
;;

let arty_a7_35t =
  Command.group
    ~summary:""
    [ "blinker", arty_a7_35t Hardcaml_hobby_boards_demos_arty_a7_35t.Blinker.create ]
;;

let () =
  Command_unix.run
    (Command.group ~summary:"" [ "nexys-a7", nexys_a7_100t; "arty-a7", arty_a7_35t ])
;;
