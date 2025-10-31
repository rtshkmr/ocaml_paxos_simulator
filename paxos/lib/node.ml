[@@@ocaml.warning "-32-33-27-69"] (** TODO: remove unused variable warnings*)

open Base
open Event_bus
open Types
open Message

(** Make_node is functor that allows us to create a Node.
    This allows us to effectively bind together an Value, Storage and Bus over which communication happens.
*)
module Make_node (V : Value.S)
    (Storage : Storage.S)
    (Bus : sig
       include module type of Event_bus
         (* For type compatibility we assume the Event_bus was compiled with 'a t etc *)
     end) =
struct
  type role = Proposer | Acceptor | Learner
  type roles = role list

  (** v0: for simple paxos, we shall just keep it to proposal id, can expand to (proposal_id, node_id) for multi-paxos *)
  type proposal_key = Types.proposal_id
  type inbox_entry = {
    mutable messages: V.t Message.t list;
  } [@@deriving sexp_of]

  type inbox = (proposal_key, inbox_entry) Hashtbl.t

  module State = struct
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

  type t = {
    id : Types.node_id;
    roles : roles;
    inbox : inbox;
    mutable state : State.t;
    storage : Storage.t;
    mutable subs : (Types.topic, (Bus.sub_handle, V.t Message.t Bus.t) Hashtbl.t) Hashtbl.t;
    transitions : string list ref;  (* light-weight history for debugging *)
  }

  let id t = t.id
  let roles t = t.roles
  let state t = t.state
  let buses_for_topic t topic =
    match Hashtbl.find t.subs topic with
    | Some table -> Hashtbl.fold table ~init:[] ~f:(fun ~key:_ ~data:bus acc -> bus :: acc)
    | None -> []

  let dump_state t = State.sexp_of_t t.state

  let get_or_create_inbox_entry (node: t) (key: proposal_key) =
    match Hashtbl.find node.inbox key with
    | Some inbox_entry -> inbox_entry
    | None ->
      let new_inbox_entry = { messages = [] } in
      Hashtbl.set node.inbox ~key ~data:new_inbox_entry;
      new_inbox_entry

  let dump_inbox (node : t) =
    let alist = Hashtbl.to_alist node.inbox in
    let sexp =
      List.sexp_of_t
        (Sexplib.Conv.sexp_of_pair
           Types.sexp_of_proposal_id
           sexp_of_inbox_entry)
        alist
    in
    Stdio.printf "Inbox for node %d:\n%s\n%!" node.id (Sexplib.Sexp.to_string_hum sexp)

  (* helper to persist acceptor record *)
  module Acceptor_record = struct
    type value = {
      promised : Types.proposal_id option;
      accepted : (Types.proposal_id * V.t) option;
    } [@@deriving sexp]
  end

  let set_node_state (node : t) (new_state : State.t) : unit =
    Stdio.printf "Node %d changed state from %s to %s\n%!"
      node.id
      (Sexplib.Sexp.to_string (State.sexp_of_t node.state))
      (Sexplib.Sexp.to_string (State.sexp_of_t new_state));
    node.state <- new_state


  (* Node propose: create PermissionRequest and rely on simulator/bus to broadcast *)
  let propose ~bus t ~proposal ~value =
    (* Build PermissionRequest for this node *)
    let msg = Message.make_permission_request ~topic:Types.Coordination ~from:t.id ~proposal ~value in
    (* For v0 we'll have simulator broadcast on behalf of node; but provide direct publish too *)
    Bus.enqueue bus ~topic:Types.Coordination (msg : V.t Message.t)

  (* unsubscribe helpers *)
  let shutdown t =
    Stdio.print_endline ("Shutting down node: " ^ Int.to_string t.id);
    Hashtbl.iteri t.subs ~f:(fun ~key:_ ~data:inner_table ->
        Hashtbl.iteri inner_table ~f:(fun ~key:handle ~data:bus ->
            Bus.unsubscribe bus handle
          );
        Hashtbl.clear inner_table
      );
    Hashtbl.clear t.subs

  let process_inboxes (node: t) : unit =
    dump_inbox node;
    Hashtbl.iteri node.inbox ~f:(fun ~key:proposal_id ~data:inbox_entry ->
        (* TODO: check if quorum/majority is reached on collected responses *)
        let quorum_reached = false  (* TODO add logic to placeholder *) in
        if quorum_reached then begin
          (* TODO: trigger next step, e.g., send Accept or decide value *)
          Stdio.printf "Node %d quorum reached for proposal %s\n%!" node.id (Sexp.to_string (Types.sexp_of_proposal_id proposal_id));
          (* Clear inbox or mark done for this proposal *)
        end else begin
          (* Quorum not reached yet, keep collecting *)
          Stdio.printf "Node %d quorum NOT YET reached for proposal %s\n%!" node.id (Sexp.to_string (Types.sexp_of_proposal_id proposal_id));
          ()
        end
      )

  let handle_message (node: t) (msg: V.t Message.t) =
    match Message.proposal_id_of msg with
    | None -> ()
    | Some key -> let inbox_entry = get_or_create_inbox_entry node key
      in
      inbox_entry.messages <- msg::inbox_entry.messages;
      process_inboxes node


  let create ?(topics=[]) ?(state=State.Idle) ~id ~roles ~storage ~bus () =
    let node = {
      id;
      roles;
      state;
      storage;
      subs= Hashtbl.Poly.create ();
      inbox= Hashtbl.Poly.create ();
      transitions = ref [];
    } in
    let handler (msg: V.t Message.t) = handle_message node msg in
    List.iter topics ~f:(fun topic ->
        let topic_table =
          match Hashtbl.find node.subs topic with
          | Some table -> table
          | None ->
            let table = Hashtbl.Poly.create () in
            Hashtbl.add_exn node.subs ~key:topic ~data:table;
            table
        in
        let handle = Bus.subscribe bus ~topic (fun msg -> handler msg) in
        Hashtbl.add_exn topic_table ~key:handle ~data:bus
      );

    Stdio.eprintf "Node %d subscribed to topics %s\n%!" node.id (topics |> List.map ~f:(fun topic -> Sexp.to_string (Types.sexp_of_topic topic))
                                                                 |> String.concat ~sep:", ");
    node

end
