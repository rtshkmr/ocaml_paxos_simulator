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

  module Acceptor_record : Acceptor_record.S

  module State : Node_state.S

  (** Abstract type representing a node instance. Concrete shape is opaque. *)
  type t

  val id_of : t -> Types.node_id

  val alias_of : t -> string

  include Has_spec with type t := t

  (** Roles a node can play in the Paxos protocol. *)
  type role = Proposer | Acceptor | Learner

  (** A list of [role]s representing the roles assigned to a node. *)
  type roles = role list

  val role_of_str : string -> role option

  type assertion = V.t Types.paxos_assertion_state [@@deriving sexp]

  type promise = assertion option [@@deriving sexp]

  (** Configuration for simulation semantics, including mutable cluster_size tracking. *)
  type runtime_config = {mutable cluster_size: int option ref}

  (** Node runtime configuration consisting of simulation settings, assigned roles,
      and a persistence storage backend. The types here are tied to the [Storage]
      module injected. *)
  type config =
    { runtime: runtime_config
    ; roles: roles
    ; storage: Storage.t
    ; topics: Types.topic list }

  val register_node_with_bus : V.t Message.t Bus.t -> t -> t

  val roles : t -> roles
  (** Return the roles assigned to a node. *)

  val state : t -> State.role_state
  (** Return the current internal state of a node. *)

  val handle_coordination : t -> V.t Message.t -> unit
  (** Handle a coordination message received by the node. *)

  val handle_simulation_control : t -> V.t Message.t -> unit
  (** Handle a simulation control message received by the node. *)

  val handle_time : t -> V.t Message.t -> unit

  val propose :
       t
    -> msg_id:int
    -> time:int
    -> bus:V.t Message.t Bus.t
    -> assertion:V.t Types.paxos_assertion_state
    -> unit

  val suggest :
       msg_id:int
    -> time:int
    -> bus:V.t Message.t Bus.t
    -> t
    -> assertion:V.t Types.paxos_assertion_state
    -> unit

  val sexp_of_role_state : t -> Sexp.t

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

  val get_cluster_size : t -> int
  (** convenience cluster size getter *)

  type spec =
    { node_id: int
    ; node_alias: string
    ; topic_strs: string list
    ; initial_cluster_size: int
    ; initial_state: string option
    ; storage_config: string option }
  [@@deriving sexp, yojson]

  val of_spec : spec -> t
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
