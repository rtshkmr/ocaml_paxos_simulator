[@@@ocaml.warning "-32-33-27-69"] (** TODO: remove unused variable warnings*)

open Base
open Event_bus
open Types
open Message

module Make_node (V : Value.S)
  (Storage : Storage.S)
  (Bus : sig
     include module type of Event_bus
     (* For type compatibility we assume the Event_bus was compiled with 'a t etc *)
   end) =
struct
  type role = Proposer | Acceptor | Learner
  type roles = role list

  module State = struct
    type t =
      | Idle
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

  type t = {
    id : Types.node_id;
    roles : roles;
    mutable state : State.t;
    storage : Storage.t;
    mutable subs : Bus.sub_handle list;
    transitions : string list ref;  (* light-weight history for debugging *)
  }

  let create ~id ~roles ~storage ~bus () =
    let node = {
      id;
      roles;
      state = State.Idle;
      storage;
      subs = [];
      transitions = ref [];
    } in

    (* subscribe generic handler on coordination topics *)
    let handler (msg : V.t Message.t) =
      (* wrapper calling node_handle *)
      let () = (* call node's message handler *)
        match msg with
        | PermissionRequest _ -> ()
        | PermissionGranted _ -> ()
        | Suggestion _ -> ()
        | Accepted _ -> ()
        | Nack _ -> ()
      in ()
    in

    (* For v0 we subscribe to Coordination and Suggestion topics (example) *)
    let h1 = Bus.subscribe bus ~topic:Types.Coordination (fun m -> handler (m : V.t Message.t)) in
    let h2 = Bus.subscribe bus ~topic:Types.Suggestion (fun m -> handler (m : V.t Message.t)) in
    node.subs <- [h1; h2];
    node

  let id t = t.id
  let roles t = t.roles
  let state t = t.state

  let dump_state t = State.sexp_of_t t.state

  (* helper to persist acceptor record *)
  module Acceptor_record = struct
    type value = {
      promised : Types.proposal_id option;
      accepted : (Types.proposal_id * V.t) option;
    } [@@deriving sexp]
  end

  (* TODO Node's message handler: skeleton (detailed logic to be filled) *)
  let handle_message (t : t) (msg : V.t Message.t) =
    (* pattern match and implement acceptor / proposer behavior *)
    match msg with
    | PermissionRequest { from; proposal; _ } ->
      (* as an acceptor, consult storage, decide whether to promise *)
      (* Pseudocode:
         let open Storage in
         match Storage.load t.storage ~key:t.id with
         | Ok (Some record) -> check record.promised
         | Ok None -> grant and persist promised=proposal
      *)
      ()
    | PermissionGranted _ -> ()
    | Suggestion { from; proposal; value; _ } ->
      ()
    | Accepted _ -> ()
    | Nack _ -> ()
  ;;

  (* Node propose: create PermissionRequest and rely on simulator/bus to broadcast *)
  let propose ~bus t ~proposal ~value =
    (* Build PermissionRequest for this node *)
    let msg = Message.make_permission_request ~topic:Types.Coordination ~from:t.id ~proposal in
    (* For v0 we'll have simulator broadcast on behalf of node; but provide direct publish too *)
    Bus.enqueue bus ~topic:Types.Coordination (msg : V.t Message.t)

  (* unsubscribe helpers *)
  let shutdown t ~bus =
    List.iter t.subs ~f:(fun h -> Bus.unsubscribe bus h);
    t.subs <- []
end
