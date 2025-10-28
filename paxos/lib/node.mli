open Base
open Types
open Message

(** Representation of a single node participating in coordination. *)
module Node : sig

  (** GADT encoding the operational state of the node.
      this allows us to capture computational phases and control with great specificity what
      the legal state transitions will be. *)
  module State : sig
    type _ t =
      | Idle : unit t
      | AwaitingPermission : { pending : Types.node_id list } -> unit t
      | Proposing : { proposal : string } -> string t
      | Committed : { value : string } -> string t
    [@@deriving sexp]
  end

  (** Existential wrapper so we can store heterogeneous GADT states inside a node record. *)
  type 'v state_box = StateBox : 's State.t -> 'v state_box

  type 'v t = {
    id : Types.node_id;
    inbox : 'v Message.t Queue.t;
    state : 'v state_box;
  }

  val create : Types.node_id -> 'v t
  val handle_message : 'v t -> 'v Message.t -> 'v t
end
