open Base
open Types
open Message
open Log

(** Paxos peer node with concurrent Proposer/Acceptor/Learner roles.

    {b Architecture:} Each node maintains independent FSMs for three roles:
    - {b Proposer}: Drives consensus (Idle → Preparing → WaitingForPromises →
      ProposerAccepting → Decided)
    - {b Acceptor}: Responds to proposals (Idle → Accepting with
      promises/accepted state)
    - {b Learner}: Observes decided values (accumulates assertions)

    {b Key invariants:}
    - Acceptor state persisted on every transition (for crash recovery)
    - Proposer may suggest only after majority promises
    - Nodes can be Inactive (simulating partition/crash)

    {b Role concurrency example:} Node A can simultaneously: 1. Wait for
    promises on proposal (5, A) as proposer 2. Accept proposal (7, B) as
    acceptor 3. Learn value V from decided messages as learner

    This models real distributed systems where roles aren't mutually exclusive.

    {b Usage:}
    {[
      module N = Node.Make_node(MyValue)(Event_bus) in
      let node = N.of_spec my_spec in
      let node = N.register_node_with_bus bus node in
      N.propose ~msg_id:1 ~time:0 ~bus ~assertion node
    ]}

    See docs/paxos.org for protocol walkthrough. *)
module type S = sig
  module V : Value.S
  (** Concrete value type used in Paxos proposals and messages. Re-exposed to
      ensure all value-dependent components share the same type definition. *)

  module Bus : Event_bus.S
  (** Event bus providing message-passing facilities for simulation. Its types
      and operations are re-exposed to maintain correct typing of bus instances.
  *)

  module State : Node_state.S
  module Storage : Storage.S with type snapshot_payload = State.role_state

  type t

  val id_of : t -> Types.node_id
  val alias_of : t -> string

  include Has_spec with type t := t

  (** Roles a node can play in the Paxos protocol. *)
  type role = Proposer | Acceptor | Learner

  val role_of_str : string -> role option
  val logger_of : t -> Logger.t

  type assertion [@@deriving sexp]
  type promise = assertion option [@@deriving sexp]

  type runtime_config = { cluster_size : int ref }
  (** Configuration for simulation semantics, including mutable cluster_size
      tracking. *)

  type config = {
    runtime : runtime_config;
    roles : role list;
    topics : Types.topic list;
  }
  (** Node runtime configuration consisting of simulation settings, assigned
      roles, and a persistence storage backend. The types here are tied to the
      [Storage] module injected. *)

  val register_node_with_bus : V.t Message.t Bus.t -> t -> t
  (** Register the node with a bus so it can send and receive messages. *)

  val deregister_node_from_bus : V.t Message.t Bus.t -> t -> t
  (** Remove the node from a bus, disabling message delivery. *)

  val handle_coordination : t -> V.t Message.t -> unit
  (** Handle coordination-layer messages (Paxos protocol messages). *)

  val handle_simulation_control : t -> V.t Message.t -> unit
  (** Handle simulator-level control messages (node activation, deactivation,
      etc.). *)

  val handle_time : t -> V.t Message.t -> unit
  (** Handle time-step events emitted by the simulator. *)

  val propose :
    msg_id:int ->
    time:int ->
    bus:V.t Message.t Bus.t ->
    assertion:V.t Types.paxos_assertion_state ->
    t ->
    unit
  (** Initiates a proposal. This is a Paxos Phase 1 proposal. *)

  val suggest :
    msg_id:int ->
    time:int ->
    bus:V.t Message.t Bus.t ->
    assertion:V.t Types.paxos_assertion_state ->
    t ->
    unit
  (** Initiates a suggestion, which is a Paxos Phase 2 suggestion. *)

  val sexp_of_role_state : t -> Sexp.t

  val get_cluster_size : t -> int
  (** convenience cluster size getter *)

  type spec = {
    node_id : int;
    node_alias : string;
    topic_strs : string list;
    initial_cluster_size : int;
    initial_state : string option;
    storage_config : string option;
  }
  [@@deriving sexp, yojson]

  val of_spec : spec -> t
  val dump_state : t -> string
  val dump_spec : t -> string
end

(** The functor for constructing node implementations parameterized by:
    - [V]: the value type (module of type [Value.S])
    - [Storage]: persistence backend for acceptor records (module of type
      [Storage.S])
    - [Bus]: event bus used for communication (module type extending
      [Event_bus])

    The functor uses sharing constraints ([with module V = V], etc.) to ensure
    the output signature's modules and types are *precisely* tied to the
    injected modules, avoiding redundant declarations and exposing a minimal
    clean interface.

    This means:
    - (1) all usages of [V.t] and value-dependent types inside the output
      signature refer exactly to the caller-supplied [V] module;
    - (2) all event-bus types and operations ([Bus.t], functions, etc.) are
      shared exactly with the provided [Bus] module, ensuring correct typing of
      communication across the system;

    All types originating from [V], [Storage], and [Bus] remain strictly
    consistent between the caller and the node implementation, preserving
    modularity and avoiding type mismatches. *)
module Make_node : functor (V : Value.S) (Bus : Event_bus.S) ->
  S with module V = V with module Bus = Bus
