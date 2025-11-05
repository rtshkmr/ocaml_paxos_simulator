(**
    Node abstraction and typed internal state.

    A node simulates a Paxos participant (proposer, acceptor, learner).

    Design notes:
    - The node is parameterized by the value type ['v]. All messages the node
      receives and processes are of type ['v Message.t].
    - The internal State is a small GADT placed inside [Node.State]. The ADT
      constructors that carry data are polymorphic in ['v] to preserve generality.
    - We store the ADT inside an existential wrapper so a node record can hold
      an instance of any state variant while keeping the outer node polymorphic
      in ['v].
*)

open Base
open Types
open Message
open Event_bus

module type S = sig
  (** The value module determines the concrete type of values used in
      proposals and messages throughout the node. This is injected as a functor
      parameter but re-exposed here as a module for internal use to ensure all
      value-dependent types consistently refer to the same underlying type. *)
  module V : Value.S

  (** The storage module provides the persistence backend for acceptor records.
      Like [V], it is re-exposed here to ensure all internal types using [Storage.t]
      are consistent with the injected module. *)
  module Storage : Storage.S

  (** The event bus module provides communication mechanisms. This module includes
      the generic polymorphic type ['a t] representing buses parameterized by
      the message type, along with operations on buses. It is re-exposed to
      accurately type bus usage internally. *)
  module Bus : sig
    include module type of Event_bus
  end

  (** Roles a node can play in the Paxos protocol. *)
  type role = Proposer | Acceptor | Learner

  (** A list of [role]s representing the roles assigned to a node. *)
  type roles = role list

  val role_of_string : string -> role

  (** Acceptor_record module for local acceptor state snapshot *)
  module Acceptor_record : sig
    (** The value a local acceptor holds as part of the Paxos state.

        - [promised] is the highest proposal id this acceptor has promised not to
          accept proposals less than.
        - [accepted] is the optional last accepted proposal id and value pair.
    *)
    type value =
      { promised: Types.proposal_id option
      ; accepted: (Types.proposal_id * V.t) option }
    [@@deriving sexp]
  end

  (** Internal ADT representing the various states of a node during the Paxos
      consensus process. Each constructor optionally carries data typed using
      [V.t], ensuring the node's state is parametrically tied to the concrete
      value type chosen in [V]. *)
  module State : sig
    type promise = Types.node_id * (Types.proposal_id * V.t) option
    [@@deriving sexp]

    (* nack = source * proposal_id * hint *)
    type nack =
      Types.node_id
      * (Types.proposal_id * V.t) option
      * Types.proposal_id option
    [@@deriving sexp]

    type proposer_state =
      | Inactive
      | Idle
      | Preparing
      | WaitingForPromises of
          { proposal: Types.proposal_id
          ; promises_received: promise list
          ; nacks_received: nack list }
      | Accepting of
          {proposal: Types.proposal_id; value: V.t; acks: Types.node_id list}
      | Decided of V.t
    [@@deriving sexp]

    type acceptor_state = Inactive | Idle | Accepting of Acceptor_record.value

    type learner_state = Learned of V.t option [@@deriving sexp]

    type role_state =
      { proposer: proposer_state
      ; acceptor: acceptor_state
      ; learner: learner_state }
    [@@deriving sexp]

    val idle_of : unit -> role_state

    val inactive_of : unit -> role_state

    type quorum_result =
      | NotReached
      | MajorityNacks of (Types.proposal_id * V.t) option
      | MajorityGrants of (Types.proposal_id * V.t) option

    val is_quorum_reached : role_state -> int -> quorum_result
  end

  val state_of_string_opt : string option -> State.role_state option

  (** Configuration for simulation semantics, including mutable cluster_size tracking. *)
  type simulation_config = {mutable cluster_size: int option ref}

  (** Node runtime configuration consisting of simulation settings, assigned roles,
      and a persistence storage backend. The types here are tied to the [Storage]
      module injected. *)
  type config =
    { simulation: simulation_config
    ; roles: roles
    ; storage: Storage.t
    ; topics: Types.topic list }

  (** Abstract type representing a node instance. Concrete shape is opaque. *)
  type t

  val create :
       ?state:State.role_state
    -> id:Types.node_id
    -> config:config
    -> bus:V.t Message.t Bus.t
    -> unit
    -> t
  (** [create ~id ~config ~bus ?topics ?state ()] creates a new node.
      - [id]: unique identifier for the node.
      - [config]: configuration including roles and storage.
      - [bus]: event bus for inter-node communication, parametrized over
        messages of type [V.t Message.t].
      - [topics]: optional list of topics to subscribe to.
      - [state]: optional initial internal state of the node.

      Note: The types of [bus] and messages depend on the injected [V] and [Bus]
      modules, ensuring tight coupling between node messaging and value representation. *)

  val set_node_state : t -> State.role_state -> unit
  (** Update the internal state of a node. *)

  val id : t -> Types.node_id
  (** Return the unique identifier of a node. *)

  val roles : t -> roles
  (** Return the roles assigned to a node. *)

  val state : t -> State.role_state
  (** Return the current internal state of a node. *)

  val handle_coordination : t -> V.t Message.t -> unit
  (** Handle a coordination message received by the node. *)

  val handle_simulation_control : t -> V.t Message.t -> unit
  (** Handle a simulation control message received by the node. *)

  val handle_time : t -> V.t Message.t -> unit

  val seek_permission :
       msg_id:int
    -> time:int
    -> bus:V.t Message.t Bus.t
    -> t
    -> proposal:Types.proposal_id
    -> value:V.t
    -> unit
  (** Proposal function for the node to propose a value.
      It takes the message id, time, communication bus (parametrized on message
      type matching [V.t]), the node, proposal id, and value to propose.

      Based on our design, this enqueues to the bus instead of synchronously dispatching (i.e. it will get added to the current buffer).
   *)

  val suggest :
       msg_id:int
    -> time:int
    -> bus:V.t Message.t Bus.t
    -> t
    -> proposal:Types.proposal_id
    -> value:V.t
    -> unit

  val dump_state : t -> Sexp.t
  (** Dump the current state of the node as an s-expression for debugging. *)

  val make_config :
       topics:Types.topic list
    -> roles:roles
    -> storage:Storage.t
    -> cluster_size:int option
    -> config
  (** Construct a configuration record for the node.
      - [roles]: list of roles to assign.
      - [storage]: storage backend instance.
      - [cluster_size]: optional cluster_size, must be positive if given. *)

  val make_node_idle :
       msg_id:int
    -> time:int
    -> bus:'a Message.t Bus.t
    -> 'b
    -> node_id:int
    -> unit
  (** Convenience function to make a node idle in simulation control. *)
end

(** The functor for constructing node implementations parameterized by:
    - [V]: the value type (module of type [Value.S])
    - [Storage]: persistence backend for acceptor records (module of type [Storage.S])
    - [Bus]: event bus used for communication (module type extending [Event_bus])

    The functor uses destructive substitution
    ([with module V := V], etc.) to ensure the output signature's modules and
    types are *precisely* tied to the injected modules, avoiding redundant
    declarations and exposing a minimal clean interface.

    This means all usages of [V.t], [Storage.t], and [Bus.t] inside the output
    signature correspond exactly to the types from the input modules passed to
    the functor, maintaining strong type consistency and modularity.
*)
module Make_node : functor
  (V : Value.S)
  (Storage : Storage.S)
  (Bus : sig
     include module type of Event_bus
   end)
  -> S with module V := V with module Storage := Storage with module Bus := Bus
