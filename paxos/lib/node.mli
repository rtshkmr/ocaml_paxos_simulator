(**
    Node abstraction and typed internal state.

    A node simulates a paxos participant (proposer, acceptor, learner).

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
open Event_bus
(** The functor for constructing node implementations parameterized by:
    - [V]: the value type (module of type [Value.S])
    - [Storage]: persistence backend for acceptor records
    - [Bus]: event bus used for communication
*)
module Make_node :
  functor (V : Value.S)
  -> functor (Storage : Storage.S)
  -> functor (Bus : sig include module type of Event_bus end)
  -> sig
       type role = Proposer | Acceptor | Learner
       type roles = role list

       module State : sig
         type t =
           | Idle
           | Echo
           | Preparing of { current_proposal : Types.proposal_id; awaiting : Types.node_id list }
           | WaitingForPromises of {
               proposal : Types.proposal_id;
               promises_received : (Types.node_id * (Types.proposal_id * V.t) option) list;
             }
           | Accepting of { proposal : Types.proposal_id; value : V.t; acks : Types.node_id list }
           | AcceptedLocally of { proposal : Types.proposal_id; value : V.t }
           | Decided of V.t
         [@@deriving sexp]
       end

       type t

       val create :
         ?topics:Types.topic list ->
         ?state:State.t ->
         id:Types.node_id ->
         roles:roles ->
         storage:Storage.t ->
         bus:(V.t Message.t) Bus.t ->
         unit -> t

       val set_node_state : t -> State.t -> unit
       val id : t -> Types.node_id
       val roles : t -> roles
       val state : t -> State.t
       val handle_message : t -> V.t Message.t -> unit
       (* val propose : t -> proposal:Types.proposal_id -> value:V.t -> unit *)
       val propose :
         bus:(V.t Message.t) Bus.t ->
         t ->
         proposal:Types.proposal_id ->
         value:V.t ->
         unit
       val dump_state : t -> Sexp.t
     end


(* module Node : sig *)
(*   type role = Proposer | Acceptor | Learner *)
(*   type roles = role list *)


(*   (\** State represents a node's logical progress in the Paxos protocol. *)
(*       At the moment, this is not an exhaustive formalisation of every transition, but we TODO: reconsider the Node's Paxos State again. It will be clearer after we have the simulation subsystem up. *)
(* *\) *)
(*   module State : sig *)
(*     type 'v t = *)
(*       | Idle *)
(*       | Preparing of { current_proposal : Types.proposal_id; awaiting : Types.node_id list } *)
(*       | WaitingForPromises of { *)
(*           proposal : Types.proposal_id; *)
(*           promises_received : (Types.node_id * (Types.proposal_id * 'v) option) list; *)
(*         } *)
(*       | Accepting of { proposal : Types.proposal_id; value : 'v; acks : Types.node_id list } *)
(*       | AcceptedLocally of { proposal : Types.proposal_id; value : 'v } *)
(*       | Decided of 'v *)
(*     [@@deriving sexp] *)
(*   end *)

(*   type 'v t *)

(*   val create : *)
(*     id:Types.node_id -> *)
(*     roles:roles -> *)
(*     value_module:(module Value.S with type t = 'v) -> *)
(*     storage:(module Storage.S with type key = Types.node_id and type value = (Types.proposal_id option * Types.proposal_id option * 'v option)) -> *)
(*     bus: 'v Event_bus.t -> *)
(*     unit -> 'v t *)

(*   (\* Accessor functions: *\) *)
(*   val id : _ t -> Types.node_id *)
(*   val roles : _ t -> roles *)
(*   val state : 'v t -> 'v State.t *)

(*   (\** Node subscribes its handlers to the bus. This is done during `create` or lazily. *\) *)
(*   val handle_message : 'v t -> 'v Message.t -> unit *)

(*   (\** External driver may nudge the node to start a proposal *\) *)
(*   val propose : 'v t -> proposal:Types.proposal_id -> value:'v -> unit *)

(*   (\** For tests/inspection *\) *)
(*   val dump_state : 'v t -> Sexp.t *)



(*   (\* (\\** handle_message : *\) *)
(*   (\*     - Polymorphic over the payload type 'v (see the locally-abstract type in the implementation). *\) *)
(*   (\*     - Inspects the node's current State and the incoming message and returns an updated node. *\) *)
(*   (\*     - Pure (for v0): does not perform I/O or side-effects. The returned node is a copy with updated fields. *\) *)

(*   (\*     Future / Monadic extension (docstring): *\) *)
(*   (\*     ------------------------------------- *\) *)
(*   (\*     When integrating async runtimes, this operation can be lifted to a monadic form: *\) *)

(*   (\*       val handle_message_async : *\) *)
(*   (\*         'v t -> 'v Message.t -> 'v t Deferred.t   (\\* Async *\\) *\) *)
(*   (\*       val handle_message_lwt : *\) *)
(*   (\*         'v t -> 'v Message.t -> 'v t Lwt.t        (\\* Lwt *\\) *\) *)

(*   (\*     The monadic variant allows asynchronous side effects (persistence, networking, *\) *)
(*   (\*     timers) while keeping the core transition logic pure in tests. *\) *)
(*   (\* *\\) *\) *)
(*   (\* val handle_message : 'v t -> 'v Message.t -> 'v t *\) *)
(* end *)
