open! Core
open Hardcaml
open Signal

include struct
  open Hardcaml_hobby_boards
  module Board = Board
  module Utils = Utils
  module Nexys = Nexys_a7_100t
  module Hardcaml_risc_v_hart = Hardcaml_risc_v_hart
  module Risc_v = Hardcaml_hobby_boards_hardcaml_risc_v
  module Uart = Uart
  module Uart_types = Uart_types
end

let uart_config =
  Uart.Config.
    { data_bits = Uart_types.Data_bits.Enum.of_enum (module Signal) Eight
    ; parity = Uart_types.Parity.Enum.of_enum (module Signal) None
    ; stop_bits = Uart_types.Stop_bits.Enum.of_enum (module Signal) One
    ; clocks_per_bit =
        of_unsigned_int ~width:port_widths.clocks_per_bit (100_000_000 / 115_200)
    }
;;

module Cpu =
  Risc_v.System_with_bram.Make
    (struct
      let register_width = Hardcaml_risc_v_hart.Register_width.B32
      let num_registers = 32
      let design_frequency = 100_000_000
    end)
    (struct
      let capacity_in_bytes = 65536
    end)
    (struct
      let num_harts = 1
      let io_controller = uart_config
    end)

let create () =
  let scope = Scope.create ~flatten_design:true () in
  let board = Board.create () in
  let clocking = Nexys.Clock_and_reset.create board in
  let clear = Utils.generate_clear clocking in
  let uart = Nexys.Uart.create board in
  let { Cpu.O.registers = _; uart_tx = cpu_uart_tx; _ } =
    Cpu.create
      ~read_latency:2
      ~build_mode:Synthesis
      scope
      { clock = clocking.clock_100; clear; uart_rx = uart.rxd }
  in
  Nexys.Uart.complete board { Nexys.Uart.O.rts = gnd; txd = cpu_uart_tx };
  board
;;
