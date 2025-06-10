open! Base
open! Hardcaml
open Board
open Core

let hardcaml_circuit name (board : (_, Board.Core.t) Hashtbl.t) =
  let cores = Hashtbl.to_alist board in
  Circuit.create_exn
    ~name
    (List.map cores ~f:(fun (_, core) -> core.outputs @ core.output_tristates)
     |> List.concat)
;;

let port_name signal =
  match Signal.names signal with
  | [ name ] -> name
  | names ->
    (* Shouldn't happen as these were already circuit ports *)
    raise_s [%message "Top level circuit port requires one name" (names : string list)]
;;

let map_inputs cores ~f =
  List.map cores ~f:(fun core -> List.map core.inputs ~f) |> List.concat
;;

let map_input_tristates cores ~f =
  List.map cores ~f:(fun core -> List.map core.input_tristates ~f) |> List.concat
;;

let map_outputs cores ~f =
  List.map cores ~f:(fun core -> List.map core.outputs ~f) |> List.concat
;;

let structural_circuit name board circuit =
  Structural.reset_circuit_database ();
  Structural.create_circuit [%string "%{name}_top"] (fun () ->
    let input_set =
      Set.of_list (module String) (List.map (Circuit.inputs circuit) ~f:port_name)
    in
    let cores = Hashtbl.data board in
    let i =
      map_inputs cores ~f:(fun i ->
        let name = port_name i in
        name, Structural.mk_input name (Signal.width i))
    in
    let o =
      map_outputs cores ~f:(fun o ->
        let name = port_name o in
        name, Structural.mk_output name (Signal.width o))
    in
    let i_t =
      map_input_tristates cores ~f:(fun t ->
        let name = port_name t in
        name, Structural.mk_tristate name (Signal.width t))
    in
    let o_t =
      map_input_tristates cores ~f:(fun t ->
        let name = port_name t in
        ( ([%string "%{name}$value"], Structural.mk_wire (Signal.width t))
        , ([%string "%{name}$valid"], Structural.mk_wire 1) ))
    in
    let i = i @ i_t |> List.filter ~f:(fun (name, _) -> Set.mem input_set name) in
    let o = List.concat (o :: List.map o_t ~f:(fun (d, v) -> [ d; v ])) in
    List.iter2_exn i_t o_t ~f:(fun (_, t) ((_, d), (_, v)) ->
      Structural.(t <-- mux v [ z (width d); d ]));
    Structural.inst name ~i ~o)
;;

let generate_xdc_pins (pins : Pin.t list) board =
  let cores = Hashtbl.data board in
  let find_pin name =
    match List.find pins ~f:(fun pin -> String.equal name pin.name) with
    | None -> raise_s [%message "Failed to find pin" (name : string) (pins : Pin.t list)]
    | Some pin -> pin
  in
  let pin_to_xdc (pin : Pin.t) =
    [%rope
      "set_property -dict { PACKAGE_PIN %{pin.loc#String} IOSTANDARD \
       %{pin.iostandard#Iostandard} } [ get_ports { %{pin.name#String} } ];\n"]
  in
  let port_to_xdc p =
    let width = Signal.width p in
    if width = 1
    then [ port_name p |> find_pin |> pin_to_xdc ]
    else
      List.init width ~f:(fun idx ->
        [%string "%{port_name p}[%{idx#Int}]"] |> find_pin |> pin_to_xdc)
  in
  let i = map_inputs cores ~f:port_to_xdc |> List.concat in
  let o = map_outputs cores ~f:port_to_xdc |> List.concat in
  let t = map_input_tristates cores ~f:port_to_xdc |> List.concat in
  Rope.concat [ Rope.concat i; Rope.concat o; Rope.concat t ]
;;

let generate_tcl ~name ~part =
  [%rope
    {|# vivado -mode batch -source %{name#String}.tcl
read_verilog %{name#String}.v
read_xdc %{name#String}.xdc
synth_design -top %{name#String}_top -part %{part#String}
opt_design
#write_checkpoint -force %{name#String}.synth.dcp
place_design
#write_checkpoint -force %{name#String}.place.dcp
route_design
#write_checkpoint -force %{name#String}.route.dcp
#report_utilization -hierarchical -file %{name#String}.utilization.rpt
report_timing_summary -file %{name#String}.timing.rpt
write_bitstream -force %{name#String}.bit
set WNS [get_property SLACK [get_timing_paths -max_paths 1 -nworst 1 -setup]]
puts "WNS=$WNS"
|}]
;;

let generate ?custom_constraints ~name ~part ~pins board =
  let cores = Board.cores board in
  Hashtbl.iteri cores ~f:(fun ~key:core ~data ->
    if not data.complete then raise_s [%message "Not completed" (core : string)]);
  let hardcaml_circuit = hardcaml_circuit name cores in
  let structural_circuit = structural_circuit name cores hardcaml_circuit in
  let xdc_pin_constraints = generate_xdc_pins pins cores in
  let xdc_constraints =
    match custom_constraints with
    | None -> xdc_pin_constraints
    | Some custom -> Rope.concat [ xdc_pin_constraints; custom ]
  in
  let tcl = generate_tcl ~name ~part in
  Stdio.Out_channel.write_all
    (name ^ ".v")
    ~data:
      (Rope.concat
         [ Rtl.create Verilog [ hardcaml_circuit ] |> Rtl.full_hierarchy
         ; Structural.to_verilog structural_circuit
         ]
       |> Rope.to_string);
  Stdio.Out_channel.write_all (name ^ ".xdc") ~data:(Rope.to_string xdc_constraints);
  Stdio.Out_channel.write_all (name ^ ".tcl") ~data:(Rope.to_string tcl)
;;
