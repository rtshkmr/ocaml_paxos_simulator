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

  (* TODO Node's message handler: skeleton (detailed logic to be filled) *)
  let handle_message (node : t) (msg : V.t Message.t) =
    (* pattern match and implement acceptor / proposer behavior *)
    match node.state, msg with
    | State.Idle, _ ->
      Stdio.print_endline ("Bro I'm idle - Node" ^ ( Int.to_string node.id ));
      ()
    | State.Echo, PermissionRequest { from; proposal; _ } ->
      Stdio.printf ">>> Rcv @ echo node %d: \n\t%s\n %!" node.id
        (Sexplib.Sexp.to_string (Message.sexp_of_t V.sexp_of_t msg));

      let response_topic = Message.topic_of msg in
      let buses = buses_for_topic node response_topic in
         (* TODO: TEST: this is just to echo back the same thing -- it will keep echoing each other back infinitely lol.*)
        List.iter buses ~f:(fun bus -> (Bus.enqueue bus ~topic:response_topic msg));
        Stdio.printf "Node %d just enqueued a response\n%!" node.id;
        set_node_state node State.Idle;
      ()
    | _, PermissionRequest { from; proposal; _ } ->
      (* as an acceptor, consult storage, decide whether to promise *)
      (* Pseudocode:
         let open Storage in
         match Storage.load t.storage ~key:t.id with
         | Ok (Some record) -> check record.promised
         | Ok None -> grant and persist promised=proposal
      *)
      Stdio.printf "Echo received message at node %d: %s\n %!" node.id
        (Sexplib.Sexp.to_string (Message.sexp_of_t V.sexp_of_t msg));

      (* let response_topic = Message.topic_of msg in *)
      (* let buses = buses_for_topic t response_topic in *)
      (*    (\* TODO: TEST: this is just to echo back the same thing -- it will keep echoing each other back infinitely lol.*\) *)
      (*   List.iter buses ~f:(fun bus -> (Bus.publish bus ~topic:response_topic msg)); *)
      ()
    | _, PermissionGranted _ -> ()
    | _,Suggestion { from; proposal; value; _ } ->
      ()
    | _, Accepted _ -> ()
    | _, Nack _ -> ()


  let create ?(topics=[]) ?(state=State.Idle) ~id ~roles ~storage ~bus () =
    let node = {
    id;
    roles;
    state;
    storage;
    subs= Hashtbl.Poly.create ();
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
