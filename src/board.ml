open! Base
open! Hardcaml

include struct
  open Board_intf
  module M_IOT = M_IOT
  module M_I = M_I
  module M_O = M_O
  module M_T = M_T
  module M_IO = M_IO
  module M_IT = M_IT
  module M_OT = M_OT

  module type Core_name = Core_name
  module type Core = Core
end

module Core = struct
  type t =
    { inputs : Signal.t list
    ; outputs : Signal.t list
    ; input_tristates : Signal.t list
    ; output_tristates : Signal.t list
    ; complete : bool
    }
  [@@deriving sexp_of]
end

type t =
  { cores : (string, Core.t) Hashtbl.t
  ; pins : Pin.t list
  }
[@@deriving sexp_of, fields ~getters]

let add_inputs { cores; pins = _ } core inputs input_tristates =
  match
    Hashtbl.add
      cores
      ~key:core
      ~data:
        { inputs; outputs = []; input_tristates; output_tristates = []; complete = false }
  with
  | `Ok -> ()
  | `Duplicate -> raise_s [%message "Cannot inputs already created" (core : string)]
;;

let set_outputs { cores; pins = _ } core outputs output_tristates =
  match Hashtbl.find cores core with
  | None -> raise_s [%message "Cannot complete core" (core : string)]
  | Some e ->
    Hashtbl.set
      cores
      ~key:core
      ~data:{ e with outputs; output_tristates; complete = true }
;;

let create () =
  let cores = Hashtbl.create (module String) in
  { cores; pins = [] }
;;

module Make_IOT (C : Core_name) (I : Interface.S) (O : Interface.S) (T : Interface.S) =
struct
  module T_enabled = With_valid.Fields.Make (T)

  let create board =
    let inputs = I.Of_signal.inputs () in
    let tristate_inputs = T.Of_signal.inputs () in
    add_inputs board C.core (I.to_list inputs) (T.to_list tristate_inputs);
    inputs, tristate_inputs
  ;;

  let complete board outputs tristate_outputs =
    let outputs = O.Of_signal.outputs outputs in
    let tristate_outputs = T_enabled.Of_signal.outputs tristate_outputs in
    set_outputs board C.core (O.to_list outputs) (T_enabled.to_list tristate_outputs)
  ;;
end

module Make_I (C : Core_name) (I : Interface.S) = struct
  include Make_IOT (C) (I) (Interface.Empty) (Interface.Empty)

  let create board =
    let i, _ = create board in
    complete board Interface.Empty.Empty Interface.Empty.Empty;
    i
  ;;
end

module Make_O (C : Core_name) (O : Interface.S) = struct
  include Make_IOT (C) (Interface.Empty) (O) (Interface.Empty)

  let complete board o =
    let _, _ = create board in
    complete board o Interface.Empty.Empty
  ;;
end

module Make_T (C : Core_name) (T : Interface.S) = struct
  include Make_IOT (C) (Interface.Empty) (Interface.Empty) (T)

  let create board =
    let _, t = create board in
    t
  ;;

  let complete board t = complete board Interface.Empty.Empty t
end

module Make_IO (C : Core_name) (I : Interface.S) (O : Interface.S) = struct
  include Make_IOT (C) (I) (O) (Interface.Empty)

  let create board =
    let inputs, _ = create board in
    inputs
  ;;

  let complete board outputs = complete board outputs Interface.Empty.Empty
end

module Make_IT (C : Core_name) (I : Interface.S) (T : Interface.S) = struct
  include Make_IOT (C) (I) (Interface.Empty) (T)

  let create board =
    let inputs, tristates = create board in
    inputs, tristates
  ;;

  let complete board tristates = complete board Interface.Empty.Empty tristates
end

module Make_OT (C : Core_name) (O : Interface.S) (T : Interface.S) = struct
  include Make_IOT (C) (Interface.Empty) (O) (T)

  let create board =
    let _, tristates = create board in
    tristates
  ;;

  let complete board outputs tristates = complete board outputs tristates
end
