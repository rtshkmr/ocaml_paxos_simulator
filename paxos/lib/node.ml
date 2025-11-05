[@@@ocaml.warning "-32-37-33-27-69"] (** TODO: remove unused variable warnings*)

open Base
open Event_bus
open Types
open Message

module type S = sig
  module V : Value.S

  module Storage : Storage.S

  module Bus : sig
    include module type of Event_bus
  end

  type role = Proposer | Acceptor | Learner

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

  (* -- TODO: actually implement the FSM state changes for simple paxos. Refer to the notes in this response for a rough starting ground: https://www.perplexity.ai/search/i-m-writing-out-this-functor-i-yCdty5gmQjelAtQTW4J97g#15 *)
  module State : sig
    type proposer_state =
      | Inactive
      | Idle
      | Preparing
      | WaitingForPromises of {
          proposal: Types.proposal_id; promises_received:
            (Types.node_id * (Types.proposal_id * V.t) option) list;
        }
      | Accepting of {
          proposal: Types.proposal_id;
          value: V.t;
          acks: Types.node_id list;
        }
      | Decided of V.t
    [@@deriving sexp]

    type acceptor_state = Inactive | Idle | Accepting of Acceptor_record.value

    type learner_state = Learned of V.t option [@@deriving sexp]

    type role_state =
      { proposer: proposer_state
      ; acceptor: acceptor_state
      ; learner: learner_state }
    [@@deriving sexp]

    val idle_of: unit -> role_state
    val inactive_of: unit -> role_state

  end

  val state_of_string_opt:string option -> State.role_state option

  type simulation_config = {mutable cluster_size: int option ref}

  type config = {simulation: simulation_config; roles: roles; storage: Storage.t;  topics: Types.topic list}

  type t

  val create :
    ?state:State.role_state
    -> id:Types.node_id
    -> config:config
    -> bus:V.t Message.t Bus.t
    -> unit
    -> t

  val set_node_state : t -> State.role_state -> unit

  val id : t -> Types.node_id

  val roles : t -> roles

  val state : t -> State.role_state


  val handle_coordination : t -> V.t Message.t -> unit

  val handle_simulation_control : t -> V.t Message.t -> unit

  val handle_time : t -> V.t Message.t -> unit

  val propose :
    msg_id:int
    -> time:int
    -> bus:V.t Message.t Bus.t
    -> t
    -> proposal:Types.proposal_id
    -> value:V.t
    -> unit

  val dump_state : t -> Sexp.t

  val make_config :
    topics: Types.topic list -> roles:roles -> storage:Storage.t -> cluster_size:int option -> config

  val make_node_idle :
    msg_id:int
    -> time:int
    -> bus:'a Message.t Bus.t
    -> 'b
    -> node_id:int
    -> unit
end

module Make_node (V : Value.S)
    (Storage : Storage.S)
    (Bus : sig
       include module type of Event_bus
     end): S with module V := V with module Storage := Storage with module Bus := Bus = struct
  type role = Proposer | Acceptor | Learner
  type roles = role list

  (** v0: for simple paxos, we shall just keep it to proposal id, can expand to (proposal_id, node_id) for multi-paxos *)
  type proposal_key = Types.proposal_id
  type inbox_entry = {
    mutable messages: V.t Message.t list;
  } [@@deriving sexp_of]

  type inbox = (proposal_key, inbox_entry) Hashtbl.t

  let role_of_string = function
    | "Proposer" -> Proposer
    | "Acceptor" -> Acceptor
    | "Learner" -> Learner
    | s -> failwith ("Unknown role: " ^ s)

  module Acceptor_record = struct
    (** Local acceptor state snapshot for Paxos *)

    type value = {
      promised : Types.proposal_id option;
      accepted : (Types.proposal_id * V.t) option;
    }
    [@@deriving sexp]
  end


  module State = struct
    type proposer_state =
      | Inactive
      | Idle
      | Preparing
      | WaitingForPromises of {
          proposal: Types.proposal_id; promises_received:
            (Types.node_id * (Types.proposal_id * V.t) option) list;
        }
      | Accepting of {
          proposal: Types.proposal_id;
          value: V.t;
          acks: Types.node_id list;
        }
      | Decided of V.t
    [@@deriving sexp]

    type acceptor_state = Inactive | Idle | Accepting of Acceptor_record.value [@@deriving sexp]
    type learner_state = Learned of V.t option [@@deriving sexp]

    type role_state = {
      proposer: proposer_state;
      acceptor: acceptor_state;
      learner: learner_state;
    } [@@deriving sexp]

    let idle_of () = {
      proposer = Idle;
      acceptor = Idle;
      learner = Learned None;
    }

    let inactive_of () = {
      proposer = Inactive;
      acceptor = Inactive;
      learner = Learned None;
    }

    (* Getters and setters *)
    let get_proposer rs = rs.proposer
    let set_proposer rs p = { rs with proposer = p }
    let get_acceptor rs = rs.acceptor
    let set_acceptor rs a = { rs with acceptor = a }
    let get_learner rs = rs.learner
    let set_learner rs l = { rs with learner = l }

    (** GADT to encode which role and its sub-state type
        This encodes the association between a constructor (Proposer, Acceptor, Learner) and its precise sub-state type.
    *)
    type _ role_selector =
      | Proposer : proposer_state role_selector
      | Acceptor : acceptor_state role_selector
      | Learner : learner_state role_selector

    (** Polymorphic role_state accessor *)
    let get_role : type a. role_state -> a role_selector -> a = fun rs sel ->
      match sel with
      | Proposer -> rs.proposer
      | Acceptor -> rs.acceptor
      | Learner -> rs.learner

    (** Polymorphic role_state setter *)
    let set_role : type a. role_state -> a role_selector -> a -> role_state = fun rs sel v ->
      match sel with
      | Proposer -> { rs with proposer = v }
      | Acceptor -> { rs with acceptor = v }
      | Learner -> { rs with learner = v }

  end


  let state_of_string_opt = function
    | Some "Idle" -> Some (State.idle_of ())
    | Some "Inactive" -> Some (State.inactive_of ())
    | _ -> failwith "Unsupported initial state string"

  type simulation_config = {
    mutable cluster_size: int option ref;
  }
  type config = {
    simulation: simulation_config;
    roles : roles;
    storage : Storage.t;
    topics: Types.topic list;
  }

  type t = {
    id : Types.node_id;
    config: config;
    inbox : inbox;
    mutable state : State.role_state;
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

  let dump_state t = State.sexp_of_role_state t.state

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

  let set_node_state (node : t) (new_state : State.role_state) : unit =
    Stdio.printf "Node %d changed state from %s to %s\n%!"
      node.id
      (Sexplib.Sexp.to_string (State.sexp_of_role_state node.state))
      (Sexplib.Sexp.to_string (State.sexp_of_role_state new_state));
    node.state <- new_state

  (* Node propose: create PermissionRequest and rely on simulator/bus to broadcast *)
  let propose ~msg_id ~time ~bus t ~proposal ~value =
    (* Build PermissionRequest for this node *)
    let coord_msg = Message.make_permission_request ~msg_id ~time ~topic:Types.Coordination ~from:t.id ~proposal ~value in
    let msg = Message.Coordination coord_msg in
    let thunk = (Types.Coordination, None ), msg   in
    (* For v0 we'll have simulator broadcast on behalf of node; but provide direct publish too *)
    Bus.enqueue bus thunk

  let make_node_idle ~msg_id ~time ~bus t ~node_id =
    (* Build PermissionRequest for this node *)
    let time = 1 in (*TODO TEMP -- until we wire up simulator time-flow *)
    let sim_ctrl_msg = Message.make_sim_control_idle_node ~msg_id ~time ~node_id in
    let msg = Message.Control sim_ctrl_msg in
    let thunk = (Types.Simulation_control, Some node_id), msg in
    (* For v0 we'll have simulator broadcast on behalf of node; but provide direct publish too *)
    Bus.enqueue bus thunk

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
    match !(node.config.simulation.cluster_size) with
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
          | Message.Time _ -> false
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

  (** Polymorphic transition function for role state.

    Learning NOTE:
      1. Importance of locally abstract types for type safety
         - [(type a)] introduces a locally abstract type [a] scoped within the function, tied by GADT patterns to a specific substate type ([proposer_state], [acceptor_state], or [learner_state]).
         - Locally abstract types enable type-safe polymorphic dispatch: each constructor of the GADT carries different precise type information for ['a].
         - The function can only accept or return values consistent with ['a] as determined by the GADT constructor.
         - This is what makes GADT-based functions type-safe and flexible without unsafe casts or polymorphic variants.
  *)
  let transition_role_state node (type a) (sel : a State.role_selector) (new_substate : a) =
    let curr = node.state in
    let new_role_state = State.set_role curr sel new_substate in
    node.state <- new_role_state

  let handle_permission_request node msg =
    match msg with
    | Message.Coordination (PermissionRequest pr) ->
      let requestor_id = Message.sender_of msg in
      let proposal = pr.proposal in
      let topic = Message.topic_of msg in
      let msg_meta = Message.meta_of msg in
      let msg_id = 1 + msg_meta.id in
      let time = 1 + msg_meta.timestamp in

      let current_promised_opt =
        match State.get_role node.state State.Acceptor with
        | State.Inactive | State.Idle -> None
        | State.Accepting record -> record.promised
      in

      let open Types in
      let is_permissible =
        Option.is_none current_promised_opt
        || compare_proposal_id (Option.value_exn current_promised_opt) proposal < 0
      in

      let buses = buses_for_topic node topic in

      let enqueue_reply reply_msg =
        match buses with
        | [] -> failwith "Impossible case, should always have at least one bus"
        | bus :: _ -> Bus.enqueue bus ((topic, Some requestor_id), reply_msg)
      in

      if is_permissible then
      let updated_record = { Acceptor_record.promised = Some proposal; accepted = None } in
      node.state <-
        State.set_role node.state State.Acceptor (State.Accepting updated_record);
      let last_accepted = None in
      let raw_reply_msg =
        Message.make_permission_granted ~msg_id ~topic ~proposal ~time ~from:node.id ~last_accepted
      in
      enqueue_reply (Message.Coordination raw_reply_msg)
    else
      let nack_msg =
        Message.make_nack ~msg_id ~topic ~time ~from:node.id ~proposal ~hint:current_promised_opt
      in
      enqueue_reply (Message.Coordination nack_msg)
    | _ -> failwith "Met an impossible state when handling permission request."


  let handle_permission_request_ node msg =
    match msg with
    | Message.Coordination (PermissionRequest pr) ->
      let requestor_id = Message.sender_of msg in
      let proposal = pr.proposal in
      let topic = Message.topic_of msg in
      let msg_meta = Message.meta_of msg in
      let msg_id = 1 + msg_meta.id in
      let time = 1 + msg_meta.timestamp in
      let current_promised_opt = State.get_role node.state State.Acceptor |> function
        | State.Inactive | State.Idle -> None
        | State.Accepting record -> record.promised
      in
      let is_permissible = Option.is_none current_promised_opt || (Types.compare_proposal_id (Option.value_exn current_promised_opt) proposal) < 0 in
      if is_permissible then begin
        (* FIXME: instead of None, I'm wondering if it should be a copy over of the current_promised *)
        let updated_record = {Acceptor_record.promised = Some proposal; accepted = None } in
        let new_acceptor_state = State.Accepting updated_record in
        node.state <- State.set_role node.state State.Acceptor new_acceptor_state;
        let last_accepted = None in
        let raw_reply_msg = Message.make_permission_granted ~msg_id ~topic ~proposal ~time ~from:node.id ~last_accepted:last_accepted in
        let reply_msg = (Message.Coordination raw_reply_msg) in
        let thunk = ((topic, Some requestor_id), reply_msg) in
        (* FIXME: not sure why there's multiple busses registered, it was supposed to be a singleton, i'll just take first one *)
        let buses = buses_for_topic node topic in
        match buses with
        | [] -> failwith "Impossible case, should always have at least one bus"
        | bus :: _ ->
          Bus.enqueue bus thunk;
      end
      else begin
        let nack_msg = Message.make_nack ~msg_id ~topic ~time ~from:node.id ~proposal ~hint:current_promised_opt in
        let reply_msg = (Message.Coordination nack_msg) in
        let thunk =((topic, Some requestor_id), reply_msg) in
        let buses = buses_for_topic node topic in
        match buses with
        | [] -> failwith "Impossible case, should always have at least one bus"
        | bus :: _ ->
          Bus.enqueue bus thunk;
      end
    | _ -> failwith "Met an impossible state when handling permission request."


  let handle_permission_granted node msg =
    match node.state.proposer with
    | Idle -> () (* move to WaitingForPromises, update proposal, etc. *)
    | Preparing -> () (* handle promise acceptance *)
    | _ -> ()

  let handle_nack node msg =
    (* process rejection in proposer/acceptor logic *)
    ()

  let handle_suggestion node msg =
    (* acceptor may record proposal and value, proposer may prepare proposal, learner may learn *)
    ()

  let handle_accepted node msg =
    (* update proposer state or infer consensus *)
    ()

  let handle_coordination node msg =
    match Message.proposal_id_of msg with
    | None -> ()
    | Some proposal_id -> begin
        match msg with
        | Message.Coordination (PermissionRequest _) -> handle_permission_request node msg
        | Message.Coordination (PermissionGranted _) -> handle_permission_granted node msg
        | Message.Coordination (Nack _) -> handle_nack node msg
        | Message.Coordination (Suggestion _) -> handle_suggestion node msg
        | Message.Coordination (Accepted _) -> handle_accepted node msg
        | _ -> ()
      end

  let handle_simulation_control (node: t) (msg: V.t Message.t) =
    match msg with
    | Message.Control (MakeNodeIdle {node_id; _}) when node_id = node.id ->
      node.state <- State.idle_of ();
      Stdio.printf "---> Node %d was made idle\n%!" node.id

    | Message.Control (MakeNodeInactive {node_id; _}) when node_id = node.id ->
      node.state <- State.inactive_of ();
      Stdio.printf "---> Node %d was made inactive\n%!" node.id


    | _ -> Stdio.printf "---> Node %d received simulation control but did nothing \n%!" node.id

  let handle_time (node: t) (msg: V.t Message.t) =
    match msg with
    | Message.Time (Heartbeat {time; _}) -> Stdio.printf "---> Node %d received time msg time = %d! \n%!" node.id time;
    | _ -> Stdio.printf "---> Node %d received time msg! \n%!" node.id


  let default_config ~roles ~storage = {
    roles;
    storage;
    simulation={cluster_size=ref None;};
    topics=[Types.Coordination; Types.Time; Types.Simulation_control]
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
    | Types.Time -> handle_time node
    | _ -> handle_coordination node

  let create ?(state=(State.idle_of ())) ~id ~config ~bus () =
    let topics = config.topics in
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
        let node_id = node.id in
        let subscription_handle = Bus.subscribe bus ~topic ~node_id callback in
        Hashtbl.add_exn topic_table ~key:subscription_handle ~data:bus
      );

    Stdio.eprintf "Node %d subscribed to topics %s\n%!" node.id (topics |> List.map ~f:(fun topic -> Sexp.to_string (Types.sexp_of_topic topic))
                                                                 |> String.concat ~sep:", ");
    node

  let make_config ~topics ~roles ~storage ~cluster_size  : config =
    let cluster_size_opt =
      match cluster_size with
      | None -> None
      | Some x when x > 0 -> Some x
      | _ -> invalid_arg "cluster_size must be > 0 or None"
    in
    {
      topics;
      simulation = { cluster_size = ref cluster_size_opt };
      roles;
      storage
    }
end
