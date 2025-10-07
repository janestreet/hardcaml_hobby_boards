open! Core
open Hardcaml
open Hardcaml_risc_v_hart
open Hardcaml_waveterm
open Hardcaml_hobby_boards
open Hardcaml_hobby_boards_hardcaml_risc_v
open Hardcaml_risc_v_test
open Hardcaml_io_framework_test
open! Bits

let debug = false

let uart_config =
  Uart.Config.
    { data_bits = Uart_types.Data_bits.Enum.of_enum (module Signal) Eight
    ; parity = Uart_types.Parity.Enum.of_enum (module Signal) None
    ; stop_bits = Uart_types.Stop_bits.Enum.of_enum (module Signal) One
    ; clocks_per_bit =
        Signal.of_unsigned_int ~width:port_widths.clocks_per_bit (200_000 / 50_000)
    }
;;

module Cpu_with_dma_memory =
  System_with_bram.Make
    (struct
      let register_width = Register_width.B32
      let num_registers = 32
      let design_frequency = 1000
    end)
    (struct
      let capacity_in_bytes = 32
    end)
    (struct
      let num_harts = 1
      let io_controller = uart_config
    end)

module With_transmitter = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; data_in_valid : 'a
      ; data_in : 'a [@bits 8]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { ethernet_txen : 'a
      ; ethernet_txd : 'a [@bits 2]
      ; uart_data_out_valid : 'a
      ; uart_data_out : 'a [@bits 8]
      ; registers : 'a Cpu_with_dma_memory.Registers.t list [@length 1]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope { I.clock; clear; data_in_valid; data_in } =
    let { Uart.Tx.O.txd; _ } =
      Uart.Tx.hierarchical
        scope
        { Uart.Tx.I.clocking = { clock; clear }
        ; config = uart_config
        ; data_in_valid
        ; data_in = Signal.uextend ~width:9 data_in
        }
    in
    let { Cpu_with_dma_memory.O.registers
        ; ethernet_txen
        ; ethernet_txd
        ; uart_tx = cpu_uart_tx
        ; _
        }
      =
      Cpu_with_dma_memory.hierarchical
        ~read_latency:1
        ~build_mode:Simulation
        scope
        { clock; clear; uart_rx = txd }
    in
    let { Uart.Rx.O.data_out_valid; data_out; _ } =
      Uart.Rx.hierarchical
        ~align_rxdata_to_lsb:true
        scope
        { Uart.Rx.I.clocking = { clock; clear }
        ; enable = Signal.vdd
        ; config = uart_config
        ; rxd = cpu_uart_tx
        }
    in
    { O.registers
    ; ethernet_txen
    ; ethernet_txd
    ; uart_data_out_valid = data_out_valid
    ; uart_data_out = Signal.sel_bottom ~width:8 data_out
    }
  ;;
end

module Sim = Cyclesim.With_interface (With_transmitter.I) (With_transmitter.O)

type sim =
  { sim : Sim.t
  ; waveform : Waveform.t
  ; name : string
  }

let create_sim name : sim =
  let sim =
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (With_transmitter.create
         (Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()))
  in
  let waveform, sim = Waveform.create sim in
  { sim; waveform; name }
;;

let finalize_sim sim =
  if debug
  then
    Waveform.Serialize.marshall
      sim.waveform
      ("~/waves/dma_" ^ sim.name ^ ".hardcamlwaveform")
  else ()
;;

let clear_registers ~(inputs : t ref With_transmitter.I.t) sim =
  inputs.clear := one 1;
  Cyclesim.cycle sim;
  inputs.clear := zero 1
;;

let send_dma_message ~address ~packet (sim : Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let whole_packet = Opcode_helper.dma_packet ~address packet in
  (* Send the DMA message through byte by byte. Uart_tx will transmit a
     byte once every ~10 cycles (this is dependent on the number of stop bits
     and the parity bit. *)
  let rec loop_for n =
    if n = 0
    then ()
    else (
      Cyclesim.cycle sim;
      loop_for (n - 1))
  in
  List.iter
    ~f:(fun input ->
      inputs.data_in_valid := vdd;
      inputs.data_in := of_int_trunc ~width:8 input;
      Cyclesim.cycle sim;
      inputs.data_in_valid := of_int_trunc ~width:1 0;
      loop_for 100)
    whole_packet
;;

let test ~num_cycles ~data { sim; waveform = _; name = _ } =
  let inputs = Cyclesim.inputs sim in
  (* Send a clear signal to initialize any CPU IO controller state back to
     default so we're ready to receive. *)
  clear_registers ~inputs sim;
  send_dma_message ~address:0 ~packet:data sim;
  for _ = 0 to 1000 do
    Cyclesim.cycle sim
  done;
  let outputs = Cyclesim.outputs sim in
  clear_registers ~inputs sim;
  let data = ref [] in
  let rec loop_for cycles =
    if cycles = 0
    then ()
    else (
      Cyclesim.cycle sim;
      if to_bool !(outputs.ethernet_txen) then data := !(outputs.ethernet_txd) :: !data;
      if to_bool !(outputs.uart_data_out_valid)
      then (
        let char = to_char !(outputs.uart_data_out) in
        printf "%c" (if Char.is_print char then char else '_'));
      loop_for (cycles - 1))
  in
  printf "RECEIVED FROM CPU VIA UART: ";
  loop_for num_cycles;
  printf "\n";
  printf "RECEIVED FROM CPU VIA ETHERNET: ";
  let data = !data in
  let packet_bits =
    String.to_list "Hello world!"
    |> List.map ~f:Char.to_int
    |> List.map ~f:(of_int_trunc ~width:8)
    |> concat_msb
  in
  Ethernet_utils.For_testing.check_tx_data
    ~output_data:data
    ~expected_data:packet_bits
    ~print_data_as_string:true
    ();
  match outputs.registers with
  | [ outputs ] ->
    let outputs =
      Cpu_with_dma_memory.Registers.map ~f:(fun t -> to_int_trunc !t) outputs
    in
    print_s [%message "" ~_:(outputs : int Cpu_with_dma_memory.Registers.t)];
    Test_util.print_ram sim
  | _ -> raise_s [%message "BUG: Unexpected number of harts"]
;;

let%expect_test "hello world via DMA round-trip" =
  let hello_world_program = Opcode_helper.hello_world_program in
  let sim = create_sim "test_dma_hello_world_ethernet" in
  test ~num_cycles:5000 ~data:hello_world_program sim;
  finalize_sim sim;
  [%expect
    {|
    RECEIVED FROM CPU VIA UART: D__Hello world!
    RECEIVED FROM CPU VIA ETHERNET: Hello world!
    ((pc 16)
     (general
      (0 0 0 0 0 1 16 12 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
    ("00000000  93 02 00 00 13 03 00 01  93 03 c0 00 73 00 00 00  |............s...|"
     "00000010  48 65 6c 6c 6f 20 77 6f  72 6c 64 21 00 00 00 00  |Hello world!....|")
    |}]
;;
