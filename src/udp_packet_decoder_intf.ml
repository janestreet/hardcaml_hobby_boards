(** Module that takes in ethernet frames (including ethernet header and FCS field) and
    extracts the data field of any UDP packet as an axi stream *)

open! Base
open! Hardcaml
open! Ethernet_types

module type Udp_packet_decoder = sig
  module I : sig
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; axi_rx : 'a Ethernet.Axi32.Source.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { axi_tx : 'a Ethernet.Axi32.Source.t
      ; ready : 'a Ethernet.Axi32.Dest.t
      }
    [@@deriving hardcaml]
  end

  val create : Scope.t -> Interface.Create_fn(I)(O).t
  val hierarchical : ?instance:string -> Scope.t -> Interface.Create_fn(I)(O).t
end
