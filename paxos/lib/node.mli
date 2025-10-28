(**
    Node abstraction and typed internal state.

    Design notes:
    - The node is parameterized by the value type ['v]. All messages the node
      receives and processes are of type ['v Message.t].
    - The internal State is a small GADT placed inside [Node.State]. The GADT
      constructors that carry data are polymorphic in ['v] to preserve generality.
    - We store the GADT inside an existential wrapper so a node record can hold
      an instance of any state variant while keeping the outer node polymorphic
      in ['v].
*)

open Base
open Types
open Message

module Node : sig
  module State : sig

    (** Typed node-phase GADT. We shall keep it intentionally small for v0. *)
    type _ t =
      | Idle : 'v t
      | AwaitingPermission : { pending : Types.node_id list } -> unit t
      | Proposing : { proposal : 'v } -> 'v t
    [@@deriving sexp]
  end

  (** Existential wrapper type for storing a GADT value while preserving
      the relationship with the value-type parameter ['v]. *)
  type 'v state_box = StateBox : 'v State.t -> 'v state_box

  (** Node record: parameterized by ['v].  *)
  type 'v t = {
    id : Types.node_id;
    inbox : 'v Message.t Queue.t;
    state : 'v state_box;
  }

  val create : Types.node_id -> 'v t

  (** handle_message :
      - Polymorphic over the payload type 'v (see the locally-abstract type in the implementation).
      - Inspects the node's current State and the incoming message and returns an updated node.
      - Pure (for v0): does not perform I/O or side-effects. The returned node is a copy with updated fields.

      Future / Monadic extension (docstring):
      -------------------------------------
      When integrating async runtimes, this operation can be lifted to a monadic form:

        val handle_message_async :
          'v t -> 'v Message.t -> 'v t Deferred.t   (* Async *)
        val handle_message_lwt :
          'v t -> 'v Message.t -> 'v t Lwt.t        (* Lwt *)

      The monadic variant allows asynchronous side effects (persistence, networking,
      timers) while keeping the core transition logic pure in tests.
  *)
  val handle_message : 'v t -> 'v Message.t -> 'v t
end
