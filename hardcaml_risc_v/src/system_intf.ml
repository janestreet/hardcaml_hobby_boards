open Hardcaml_hobby_boards
open Hardcaml

module type Config = sig
  val num_harts : int
  val io_controller : Signal.t Uart.Config.t
end

module type Memory_config = sig
  val capacity_in_bytes : int
end
