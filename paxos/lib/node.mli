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
         quorum:int option ->
         unit
       val dump_state : t -> Sexp.t
     end

