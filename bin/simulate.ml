open! Core

let snakes =
  Command.basic
    ~summary:""
    [%map_open.Command
      let () = return () in
      fun () ->
        Hardcaml_hobby_boards_test.Test_snakes.test_snakes ()
        |> Hardcaml_waveterm_interactive.run]
;;

let vga =
  Command.basic
    ~summary:""
    [%map_open.Command
      let () = return () in
      fun () ->
        Hardcaml_hobby_boards_test.Test_vga.test_video_timing 10_000
        |> Hardcaml_waveterm_interactive.run]
;;

let uart_tx =
  Command.basic
    ~summary:""
    [%map_open.Command
      let () = return () in
      fun () ->
        Hardcaml_hobby_boards_test.Test_uart.test_tx_waves Eight Even One
        |> Hardcaml_waveterm_interactive.run]
;;

let uart =
  Command.basic
    ~summary:""
    [%map_open.Command
      let clocks_per_bit = flag "-clocks-per-bit" (optional_with_default 8 int) ~doc:"" in
      fun () ->
        let waves, result =
          Hardcaml_hobby_boards_test.Test_uart.send_string
            ~top:true
            ~clocks_per_bit
            Eight
            Even
            One
            "hello world"
        in
        print_s [%message result];
        Hardcaml_waveterm_interactive.run waves]
;;

let ethernet_rx =
  Command.basic
    ~summary:""
    [%map_open.Command
      let data_bits = flag "-data-bits" (optional_with_default 64 int) ~doc:"" in
      let data_lengths = [ data_bits ] in
      fun () ->
        fst (Hardcaml_hobby_boards_test.Test_ethernet.test_rx_waves data_lengths)
        |> Hardcaml_waveterm_interactive.run]
;;

let ethernet_tx =
  Command.basic
    ~summary:""
    [%map_open.Command
      let data_bits = flag "-data-bits" (optional_with_default (50 * 8) int) ~doc:"" in
      let data_lengths = [ data_bits ] in
      fun () ->
        Hardcaml_hobby_boards_test.Test_ethernet.test_tx_waves data_lengths
        |> Hardcaml_waveterm_interactive.run]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:""
       [ "ethernet-rx", ethernet_rx
       ; "ethernet-tx", ethernet_tx
       ; "snakes", snakes
       ; "uart-tx", uart_tx
       ; "uart", uart
       ; "vga", vga
       ])
;;
