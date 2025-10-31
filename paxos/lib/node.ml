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

  type simulation_config = {
    mutable quorum: int option ref;
  }
  type config = {
    simulation: simulation_config;
    roles : roles;
    storage : Storage.t;
  }

  type t = {
    id : Types.node_id;
    config: config;
    inbox : inbox;
    mutable state : State.t;
    mutable subs : (Types.topic, (Bus.sub_handle, V.t Message.t Bus.t) Hashtbl.t) Hashtbl.t;
    transitions : string list ref;  (* light-weight history for debugging *)
  }

  let id t = t.id
  let roles t = t.config.roles
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
    let coord_msg = Message.make_permission_request ~topic:Types.Coordination ~from:t.id ~proposal ~value in
    let msg = Message.Coordination coord_msg in
    (* For v0 we'll have simulator broadcast on behalf of node; but provide direct publish too *)
    Bus.enqueue bus ~topic:Types.Coordination msg

  let make_node_idle ~bus t ~node_id =
    (* Build PermissionRequest for this node *)
    let sim_ctrl_msg = Message.make_sim_control_idle_node node_id in
    let msg = Message.Control sim_ctrl_msg in
    (* For v0 we'll have simulator broadcast on behalf of node; but provide direct publish too *)
    Bus.publish bus ~topic:Types.Simulation_control msg



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

  (** Returns true if the quorum (threshold) exists and a majority of non-acks is reached.
   TODO: this probably needs a check on the type of message. Check what the response is gonna be like for a permission granted.
         maybe we can just pass in a predicate function into this as a param.
  *)
let is_quorum_reached (node: t) (inbox_entry: inbox_entry) ~(predicate: 'v Message.t -> bool) =
  match !(node.config.simulation.quorum) with
  | None -> false
  | Some total ->
    let quorum_threshold = (total / 2) + 1 in
    let msgs_rcvd = inbox_entry.messages in
    let num_ack = List.count msgs_rcvd ~f:predicate in
    num_ack >= quorum_threshold

let process_inboxes (node: t) : unit =
  dump_inbox node;
  Hashtbl.iteri node.inbox ~f:(fun ~key:proposal_id ~data:inbox_entry ->
      let needs_quorum msg =
        match msg with
        | Message.Coordination coordination_msg -> (
            match coordination_msg with
            | Message.PermissionGranted _
            | Message.Accepted _ -> true
            | Message.PermissionRequest _
            | Message.Suggestion _
            | Message.Nack _ -> false )
        | Message.Control _ -> false
      in
      let quorum_reached = is_quorum_reached node inbox_entry ~predicate:needs_quorum in
      if quorum_reached then begin
        Stdio.printf "Node %d quorum reached for proposal %s\n%!" node.id (Sexp.to_string (Types.sexp_of_proposal_id proposal_id));
        (* Clear inbox or mark done for this proposal *)
      end else begin
        Stdio.printf "Node %d quorum NOT YET reached for proposal %s\n%!" node.id (Sexp.to_string (Types.sexp_of_proposal_id proposal_id));
        ()
      end
    )

  let handle_coordination (node: t) (msg: V.t Message.t) =
    match node.state, Message.proposal_id_of msg with
    | _ , None -> ()
    | State.Idle, _ -> Stdio.printf "XXXX attempted to coordinate with Node %d but that node is idle\n%!" node.id
    | _ , Some key -> let inbox_entry = get_or_create_inbox_entry node key
      in
      inbox_entry.messages <- msg::inbox_entry.messages;
      process_inboxes node


  let handle_simulation_control (node: t) (msg: V.t Message.t) =
    match msg with
    | Message.Control (MakeNodeIdle {node_id; _}) when node_id = node.id ->
      node.state <- State.Idle;
      Stdio.printf "---> Node %d was made idle\n%!" node.id

    | _ -> Stdio.printf "---> Node %d received simulation control but did nothing \n%!" node.id

  let default_config ~roles ~storage = {
    roles;
    storage;
    simulation={quorum=ref None;}
  }

  (** a callback that we can use for communicating via the bus
      this works because the node would have been bound to the callback, event bus can remain passive about it.
  *)
  type bus_registrable_callback = V.t Message.t -> unit
  (** this allows us to choose handlers based on the topic *)
  let get_handler_for_topic (node: t) (topic: Types.topic) : bus_registrable_callback =
    match topic with
    | Types.Coordination -> handle_coordination node
    | Types.Simulation_control -> handle_simulation_control node
    | _ -> handle_coordination node

  let create  ?(topics=[]) ?(state=State.Idle) ~id ~config ~bus () =
    let node = {
      id;
      state;
      config;
      subs= Hashtbl.Poly.create ();
      inbox= Hashtbl.Poly.create ();
      transitions = ref [];
    } in
    List.iter topics ~f:(fun topic ->
        let topic_table =
          match Hashtbl.find node.subs topic with
          | Some table -> table
          | None ->
            let table = Hashtbl.Poly.create () in
            Hashtbl.add_exn node.subs ~key:topic ~data:table;
            table
        in
        let callback msg = get_handler_for_topic node topic msg in
        let subscription_handle = Bus.subscribe bus ~topic callback in
        Hashtbl.add_exn topic_table ~key:subscription_handle ~data:bus
      );

    Stdio.eprintf "Node %d subscribed to topics %s\n%!" node.id (topics |> List.map ~f:(fun topic -> Sexp.to_string (Types.sexp_of_topic topic))
                                                                 |> String.concat ~sep:", ");
    node

  let make_config ~roles ~storage ~quorum  : config =
    let quorum_opt =
      match quorum with
      | None -> None
      | Some x when x > 0 -> Some x
      | _ -> invalid_arg "Quorum must be > 0 or None"
    in
    {
      simulation = { quorum = ref quorum_opt };
      roles;
      storage
    }

end
