open Core
open Hardcaml
open Hardcaml_waveterm

include struct
  open Hardcaml_hobby_boards
  module Rx = Ethernet.Rx
  module Tx = Ethernet.Tx
end

let sim_frame sim (inputs : _ Rx.I.t) (outputs : _ Rx.O.t) data_width =
  let open Bits in
  let input_data_width = width !(inputs.rxd) in
  Cyclesim.cycle sim;
  inputs.clocking.clear := gnd;
  inputs.rxd := of_string "11";
  inputs.crsdv := vdd;
  inputs.rxerr := gnd;
  Cyclesim.cycle sim;
  for _ = 0 to 31 do
    inputs.rxd := of_string "01";
    inputs.crsdv := Bits.vdd;
    Cyclesim.cycle sim
  done;
  inputs.rxd := of_string "11";
  inputs.crsdv := vdd;
  Cyclesim.cycle sim;
  let data =
    sresize ~width:data_width (of_string "128'hBBBB_4444_DDDD_1111_AAAA_5555_CCCC_3333")
  in
  let input_data = split_msb ~exact:true ~part_width:8 data |> concat_lsb in
  let output_data = ref [] in
  let check_outputs () =
    if to_bool !(outputs.axi_tx.tvalid)
    then
      if to_bool !(outputs.axi_tx.tlast)
      then (
        let valid_bytes = leading_ones !(outputs.axi_tx.tkeep) in
        let byte_mask = sll (ones 32) ~by:((4 - to_int_trunc valid_bytes) * 8) in
        output_data := (!(outputs.axi_tx.tdata) &: byte_mask) :: !output_data)
      else output_data := !(outputs.axi_tx.tdata) :: !output_data
    else ()
  in
  for i = 0 to (data_width / input_data_width) - 1 do
    inputs.rxd := input_data.:[((i + 1) * input_data_width) - 1, i * input_data_width];
    inputs.crsdv := vdd;
    Cyclesim.cycle sim;
    check_outputs ()
  done;
  inputs.crsdv := Bits.gnd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  check_outputs ();
  output_data
;;

let test_rx_waves (data_lengths : int list) =
  let module Sim = Cyclesim.With_interface (Rx.I) (Rx.O) in
  let open Bits in
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create ~config:Cyclesim.Config.trace_all (Rx.create scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs ~clock_edge:Before sim in
  inputs.clocking.clear := vdd;
  let result =
    List.map data_lengths ~f:(fun data_width ->
      !(sim_frame sim inputs outputs data_width) |> List.rev)
  in
  Cyclesim.cycle sim;
  waves, result
;;

let expect_test_rx_waves data_lengths =
  let _waves, result = test_rx_waves data_lengths in
  print_s [%message (result : Bits.Hex.t List.t List.t)]
;;

let sim_data sim (inputs : _ Tx.I.t) (outputs : _ Tx.O.t) data_width =
  let open Bits in
  let input_data_width = 32 in
  let byte_width = 8 in
  let input_data_bytes = input_data_width / byte_width in
  let output_data_width = 2 in
  let preamble_sfd_bits = 64 in
  let fcs_bits = 32 in
  let ifg_bits = 96 in
  inputs.clocking.clear := gnd;
  Cyclesim.cycle sim;
  let data = random ~width:data_width in
  let output_data = ref [] in
  let check_outputs () =
    if to_bool !(outputs.txen) then output_data := !(outputs.txd) :: !output_data else ()
  in
  let last_valid_bits = data_width % input_data_width in
  let last_valid_bytes, max_index =
    if last_valid_bits <> 0
    then last_valid_bits / byte_width, data_width / input_data_width
    else input_data_bytes, (data_width / input_data_width) - 1
  in
  for i = 0 to max_index do
    if i = max_index && last_valid_bytes <> 4
    then
      inputs.data_stream.tdata
      := data.:[(last_valid_bytes * 8) - 1, 0]
         @: zero (input_data_width - (last_valid_bytes * 8))
    else
      inputs.data_stream.tdata
      := data.:[( data_width - (i * input_data_width) - 1
                , data_width - ((i + 1) * input_data_width) )];
    inputs.data_stream.tvalid := vdd;
    inputs.data_stream.tstrb := zero input_data_bytes;
    inputs.data_stream.tkeep
    := sll (ones input_data_bytes) ~by:(input_data_bytes - last_valid_bytes);
    inputs.data_stream.tlast := of_bool (i = max_index);
    inputs.data_stream.tuser := gnd @: of_bool (i = 0);
    Cyclesim.cycle sim;
    check_outputs ()
  done;
  inputs.data_stream.tvalid := gnd;
  Cyclesim.cycle sim;
  let timeout_count = ref 0 in
  let max_timeout_count =
    (data_width + preamble_sfd_bits + fcs_bits) / output_data_width
  in
  while to_bool !(outputs.txen) && !timeout_count < max_timeout_count do
    output_data := !(outputs.txd) :: !output_data;
    timeout_count := !timeout_count + 1;
    Cyclesim.cycle sim
  done;
  for _ = 0 to (ifg_bits / output_data_width) - 1 do
    [%test_result: Bits.t] !(outputs.txen) ~expect:gnd;
    Cyclesim.cycle sim
  done;
  let preamble_sfd_value = of_string "64'h5555_5555_5555_5557" in
  let preamble_received = sel_top (concat_lsb !output_data) ~width:preamble_sfd_bits in
  let data_received =
    bswap
      (concat_msb !output_data
       |> drop_top ~width:fcs_bits
       |> drop_bottom ~width:preamble_sfd_bits)
  in
  [%test_result: Bits.Hex.t]
    (preamble_received @: data_received)
    ~expect:(preamble_sfd_value @: data)
;;

let test_tx_waves (data_lengths : int list) =
  let module Sim = Cyclesim.With_interface (Tx.I) (Tx.O) in
  let open Bits in
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create ~config:Cyclesim.Config.trace_all (Tx.create scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs ~clock_edge:Before sim in
  inputs.clocking.clear := vdd;
  inputs.data_stream.tvalid := gnd;
  Cyclesim.cycle sim;
  List.iter data_lengths ~f:(fun data_width -> sim_data sim inputs outputs data_width);
  Cyclesim.cycle sim;
  waves
;;

let expect_test_tx_waves data_lengths = ignore (test_tx_waves data_lengths : Waveform.t)

let%expect_test "test_rx_single_frame" =
  expect_test_rx_waves [ 64 ];
  [%expect {| (result ((32'haaaa5555 32'hcccc3333))) |}]
;;

let%expect_test "test_rx_tkeep_frames" =
  expect_test_rx_waves [ 48 ];
  [%expect {| (result ((32'h5555cccc 32'h33330000))) |}]
;;

let%expect_test "test_rx_multiple_frames" =
  expect_test_rx_waves [ 128; 48; 56; 40; 256 ];
  [%expect
    {|
    (result
     ((32'hbbbb4444 32'hdddd1111 32'haaaa5555 32'hcccc3333)
      (32'h5555cccc 32'h33330000) (32'haa5555cc 32'hcc333300)
      (32'h55cccc33 32'h33000000)
      (32'hffffffff 32'hffffffff 32'hffffffff 32'hffffffff 32'hbbbb4444
       32'hdddd1111 32'haaaa5555 32'hcccc3333)))
    |}]
;;

let%expect_test "test_tx_single_frame" = expect_test_tx_waves [ 64 * 8 ]
let%expect_test "test_tx_tkeep_frames" = expect_test_tx_waves [ 50 * 8 ]

let%expect_test "test_multiple_frames" =
  expect_test_tx_waves [ 64 * 8; 78 * 8; 128 * 8; 223 * 8 ]
;;
