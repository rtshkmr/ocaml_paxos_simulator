open Base
open Types
open Message

module Node = struct
  module State = struct
    type _ t =
      | Idle : 'v t
      | AwaitingPermission : { pending : Types.node_id list } -> unit t
      | Proposing : { proposal : 'v } -> 'v t
    [@@deriving sexp]
  end

  type 'v state_box = StateBox : 'v State.t -> 'v state_box

  type 'v t = {
    id : Types.node_id;
    inbox : 'v Message.t Queue.t;
    state : 'v state_box;
  }

  let create (id : Types.node_id) : 'v t =
    { id; inbox = Queue.create (); state = StateBox State.Idle }


   let enqueue_message (node : 'v t) (msg : 'v Message.t) : 'v t =
    let new_inbox = Queue.copy node.inbox in
    Queue.enqueue new_inbox msg;
   { node with inbox = new_inbox }

  let dequeue_message (node : 'v t) : 'v Message.t option * 'v t =
    match Queue.dequeue node.inbox with
    | Some msg ->
        let new_inbox = Queue.copy node.inbox in
        (Some msg, { node with inbox = new_inbox })
    | None -> (None, node)

  (** STUB: implement this*)
  let subscribe (node: 'v t) (_topic: Types.topic) = node

  (** STUB: implement this*)
  let unsubscribe (node: 'v t) (_topic: Types.topic) = node

  (**
     [handle_message node msg]

     Pure state-transition function.

     It inspects the node's current state and the incoming message and returns
     an updated node record.  The function is intentionally pure and synchronous.

     Implementation note:
     - We use a locally abstract type for the implementation so OCaml treats the
       function as polymorphic in ['v], matching the signature in the .mli.

     Future note: when we move to an async runtime, this function should be turned
     into a monadic function that returns 'v t in the desired monad (Lwt/Async).

    FIXME: this  is likely wrong, need to check correctness with the paxos write up notes.
  *)
  let handle_message : type v. v t -> v Message.t -> v t =
   fun node msg ->
    let (StateBox current_state) = node.state in
    match (current_state, msg) with
    (* Idle node receives a permission request: respond with PermissionGranted *)
    | State.Idle, PermissionRequest { meta; from = _ } ->
        let response = Message.PermissionGranted { meta; from = node.id } in
        let (_, new_node) = dequeue_message node in
        enqueue_message new_node response

    (* Idle node receives a Suggestion: become Proposing *)
    | State.Idle, Suggestion { value; _ } ->
        let new_state = State.Proposing { proposal = value } in
        { node with state = StateBox new_state }

    (* Proposer receives PermissionGranted: for the v0 scaffold we simply
       remain or move to a state that could later trigger AcceptReqs.
       Here: we go back to Idle to indicate we've acted (simplified). *)
    | State.Proposing _, PermissionGranted _ ->
        { node with state = StateBox State.Idle }

    (* Proposer receives Nack: move to Idle (or later we could implement backoff) *)
    | State.Proposing _, Nack _ ->
        { node with state = StateBox State.Idle }

    (* Suggestion received while in Proposing: ignore or update proposal (keep simple) *)
    | State.Proposing { proposal = _ }, Suggestion _ ->
        node

    (* default: ignore *)
    | _, _ -> node
end
