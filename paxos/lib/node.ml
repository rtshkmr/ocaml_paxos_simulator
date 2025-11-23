(*
IMPROVEMENT CONSIDERATIONS:
===========================
1. transition role state to be broken up into things with separate concerns
2. most imporantly, this feels like a godclass.
   It's doing a bunch of things that we could break into different submodules for:
    a) The typed Paxos state machine
    b) Subscriptions and bus wiring
    c) Logging
    d) I/O (snapshots)
    e) Protocol handlers
    f) Simulation control logic
    g) Activation & storage lifecycle
    h) Node construction
    i) Operational helper plumbing (id, alias, role_of_str)
    j) Message creation (proposal/suggestion wrappers)
    k) Node-level FSM transitions
    l) Time/heartbeat handlers
    m) Assertions/printing utilities

    I think a good end state in the medium term for the node.ml should be to be responsible for:
      1. Logging
      2. Routing
      3. Message formatting
      4. Transition invocation

3. there's multiple matches for node.state.proposer we can probably improve it.
   This is actually an indication of a design issue. We should be keeping the node_state functional and doing immutable state transformations. Then node can remain stateful and do the update within the node.
4. small stuff:
   - param destructuring sometimes too deep
   - many places do rudimentary pattern matching on optionals instead of using the option api

5. ref notes in docs/planning.org on skipped task Search for "separate pure FSM logic from node-level effects" subtree.
*)
open Base
open Event_bus
open Types
open Message
open Log
open Node_state
open Ansi.Formatter

(* open Make_mem_storage *)
open Make_file_storage

module type S = sig
  module V : Value.S

  module Bus : sig
    include module type of Event_bus
  end

  module State : Node_state.S

  module Storage : Storage.S with type snapshot_payload = State.role_state

  type t

  include Has_spec with type t := t

  val id_of : t -> Types.node_id

  val alias_of : t -> string

  type role = Proposer | Acceptor | Learner

  val role_of_str : string -> role option

  type assertion [@@deriving sexp]

  type promise = assertion option [@@deriving sexp]

  type runtime_config = {cluster_size: int ref}

  type config =
    {runtime: runtime_config; roles: role list; topics: Types.topic list}

  val register_node_with_bus : V.t Message.t Bus.t -> t -> t

  val deregister_node_from_bus : V.t Message.t Bus.t -> t -> t

  val handle_coordination : t -> V.t Message.t -> unit

  val handle_simulation_control : t -> V.t Message.t -> unit

  val handle_time : t -> V.t Message.t -> unit

  val propose :
       msg_id:int
    -> time:int
    -> bus:V.t Message.t Bus.t
    -> assertion:V.t Types.paxos_assertion_state
    -> t
    -> unit

  val suggest :
       msg_id:int
    -> time:int
    -> bus:V.t Message.t Bus.t
    -> assertion:V.t Types.paxos_assertion_state
    -> t
    -> unit

  val sexp_of_role_state : t -> Sexp.t

  val get_cluster_size : t -> int

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

module Make_node
    (V : Value.S)
    (Bus : sig
      include module type of Event_bus
    end) : S with module V = V with module Bus = Bus = struct
  module V = V
  module Bus = Bus
  module State = Make_node_state (V)

  (* TODO: add storage type to cli config / settings config, let the input file determine what type of storage to use.*)
  (* module Storage = Make_mem_storage (State) *)
  module Storage = Make_file_storage (State)

  type role = Proposer | Acceptor | Learner

  let role_of_str = function
    | "Proposer" ->
        Some Proposer
    | "Acceptor" ->
        Some Acceptor
    | "Learner" ->
        Some Learner
    | _s ->
        None

  (** v0: for simple paxos, we shall just keep it to proposal id, can expand to (proposal_id, node_id) for multi-paxos *)
  type proposal_key = Types.proposal_id [@@deriving sexp_of]

  type assertion = V.t Types.paxos_assertion_state [@@deriving sexp]

  let assertion_to_string a =
    a |> Types.sexp_of_paxos_assertion_state V.sexp_of_t |> Sexp.to_string_hum

  type promise = assertion option [@@deriving sexp]

  let promise_to_string p =
    p |> Option.value_map ~default:"Empty Promise" ~f:assertion_to_string

  type runtime_config = {cluster_size: int ref}

  type config =
    {runtime: runtime_config; roles: role list; topics: Types.topic list}

  type sub_handle_to_bus_registry =
    (Bus.sub_handle, V.t Message.t Bus.t) Hashtbl.t

  type topic_to_sub_handle_to_bus_registry =
    (Types.topic, sub_handle_to_bus_registry) Hashtbl.t

  type t =
    { id: Types.node_id
    ; alias: string
    ; config: config
    ; mutable state: State.role_state
    ; subs: topic_to_sub_handle_to_bus_registry
    ; logger: Logger.t
    ; storage: Storage.t ref }

  let msg_to_string (msg : V.t Message.t) =
    msg |> Message.sexp_of_t V.sexp_of_t |> Sexp.to_string

  let next_msg_id id = id + 1

  let next_time id = id + 1

  let id_of t = t.id

  let alias_of t = t.alias

  let buses_for_topic {subs; _} topic =
    match topic |> Hashtbl.find subs with
    | Some table ->
        Hashtbl.fold table ~init:[] ~f:(fun ~key:_ ~data:bus acc -> bus :: acc)
    | None ->
        []

  let bus_for_topic t topic = topic |> buses_for_topic t |> List.hd_exn

  let get_cluster_size {config= {runtime= {cluster_size; _}; _}; _} =
    !cluster_size

  let sexp_of_role_state {state; _} = state |> State.sexp_of_role_state

  (* TODO [quality] this has many many responsibilities, probably should break it up. *)

  (** Polymorphic transition function for role state. This is a classic GADT-indexed update, which is entirely side-effect focused.

      Learning NOTE:
      1. Importance of locally abstract types for type safety
      - [(type a)] introduces a locally abstract type [a] scoped within the function, tied by GADT patterns to a specific substate type ([proposer_state], [acceptor_state], or [learner_state]).
      - Locally abstract types enable type-safe polymorphic dispatch: each constructor of the GADT carries different precise type information for ['a].
      - The function can only accept or return values consistent with ['a] as determined by the GADT constructor.
      - This is what makes GADT-based functions type-safe and flexible without unsafe casts or polymorphic variants.
  *)
  let transition_role_state ({id; alias; logger; state; storage; _} as node)
      time (type a) (sel : a State.role_selector) (new_substate : a) =
    let new_state = new_substate |> State.set_role state sel in
    Logger.node_state_change ~node_id:id ~alias:(Some alias) logger
      ~old_state:(state |> State.state_to_str)
      ~new_state:(new_state |> State.state_to_str) ;
    node.state <- new_state ;
    let updated_storage =
      match node.state |> Storage.persist_snapshot !storage time with
      | Ok updated_storage ->
          updated_storage
      | Error e ->
          (* TODO [LOG] shift to logger *)
          Stdio.eprintf "Warning: failed to persist snapshot: %s\n%!"
            (Error.to_string_hum e) ;
          !(node.storage)
    in
    node.storage := updated_storage

  (** Represents the act of a node driving the first step of the paxos process (asking for permission).
      This means that the node's state as a Proposer will change from [ Idle ] to [ Peparing ], as we create the message then dispatch it. Once done dispatching,
      the node will change its state to [ WaitingForPromises ], marking it ready to receive responses (both grants and nacks) for that proposal.

      As such, every paxos process (i.e. an attempt to assert a value and seek consensus) can be uniquely identified via its [proposal_id]
   *)
  let propose ~msg_id ~time ~bus ~assertion ({id; state= {proposer; _}; _} as t)
      =
    match proposer with
    | State.Idle ->
        assertion |> State.Preparing
        |> transition_role_state t time State.Proposer ;
        let msg =
          Message.make_permission_request ~msg_id ~from:id ~assertion ~time
        in
        ((Types.Coordination, None), msg) |> Bus.enqueue bus ;
        {assertion; promises_received= []; nacks_received= []}
        |> State.WaitingForPromises
        |> transition_role_state t time State.Proposer
    | _ ->
        failwith "we can only propose if we are currently idle"

  let announce_decision ({id; state= {proposer; _}; _} as t : t) ~msg_id ~time
      decided_assertion =
    match proposer with
    | State.Decided _decided_val ->
        let msg =
          Message.make_decided ~msg_id ~time ~from:id ~decided_assertion
        in
        let bus = bus_for_topic t Types.Coordination in
        ((Types.Coordination, None), msg) |> Bus.enqueue bus
    | _ ->
        failwith
          "a node can only announce what it has learned when it has decided "

  (* TODO: [FSM] need to have a state change within the node after suggesting? This should allow us to capture the incoming accepted or something *)
  let suggest ~msg_id ~time ~bus ~assertion {id; _} =
    let msg = Message.make_suggestion ~msg_id ~time ~from:id ~assertion in
    ((Types.Coordination, None), msg) |> Bus.enqueue bus

  (* TODO [LOG] this needs to shift to the logger *)
  let log_node_state_control ({id; alias; _} : t) directive_msg =
    let open Printf in
    let tag =
      sprintf "[%s (node %d):] " alias id |> bold |> bright_red |> underline
    in
    let msg =
      sprintf "I have been controlled:%s" directive_msg
      |> italic |> bright_magenta
    in
    Stdio.print_endline (tag ^ msg)

  (* TODO [LOG] this needs to shift to the logger, or use the logger in some way *)
  let noop_ignore ({id; alias; _} : t) (reason : string) =
    let open Printf in
    let tag =
      sprintf "[%s (node %d):] " alias id |> bold |> bright_red |> underline
    in
    let decision =
      sprintf "will be ignoring this msg because: %s" reason
      |> italic |> bright_magenta
    in
    Stdio.print_endline (tag ^ decision)

  let with_active ~node ~ignore_reason f =
    if node.state |> State.is_inactive then ignore_reason |> noop_ignore node
    else f ()

  (* ==== microloggers==== *)
  let log_flow node ~routine ~msg =
    Logger.subroutine_flow ~node_id:(Some node.id) ~alias:(Some node.alias)
      node.logger ~routine ~msg ()

  let log_rcvd_suggestion ~node ~(msg : V.t Message.suggestion_msg) =
    let proposal = msg.assertion.proposal |> Types.proposal_id_to_string in
    let value = msg.assertion.value |> V.to_string in
    let log_msg =
      Printf.sprintf "received suggestion from %d proposal=%s value=%s" msg.from
        proposal value
    in
    log_flow node ~routine:Stdlib.__FUNCTION__ ~msg:log_msg

  let log_rcvd_accepted ~node ~(msg : V.t Message.accepted_msg) =
    let proposal = msg.assertion.proposal |> Types.proposal_id_to_string in
    let value = msg.assertion.value |> V.to_string in
    let log_msg =
      Printf.sprintf "received accepted from %d proposal=%s value=%s" msg.from
        proposal value
    in
    log_flow node ~routine:Stdlib.__FUNCTION__ ~msg:log_msg

  let log_rcvd_nack ~node ~(msg : V.t Message.nack_msg) =
    let proposal =
      msg.rejected_assertion.proposal |> Types.proposal_id_to_string
    in
    let hint = msg.hint |> promise_to_string in
    let log_msg =
      Printf.sprintf "received NACK from %d proposal=%s hint=%s" msg.from
        proposal hint
    in
    log_flow node ~routine:Stdlib.__FUNCTION__ ~msg:log_msg

  (* TODO: [LOG] example of helper, to evaluate whether this is a good pattern *)
  (* (\* TODO [LOG] figure out how to extract helper for this log msg, it prevents us from reading the function properly *\) *)
  let log_rcvd_permission_request ~node
      ~(msg : V.t Message.permission_request_msg) =
    let proposal = msg.assertion.proposal |> Types.proposal_id_to_string in
    let value = msg.assertion.value |> V.to_string in
    let msg =
      Printf.sprintf "received permission request from %d proposal=%s value=%s"
        msg.from proposal value
    in
    log_flow node ~routine:Stdlib.__FUNCTION__ ~msg

  let log_rcvd_permission_granted ~node
      ~(msg : V.t Message.permission_granted_msg) =
    let proposal = msg.assertion.proposal |> Types.proposal_id_to_string in
    let last =
      Option.value_map msg.last_accepted ~default:"None" ~f:assertion_to_string
    in
    let log_msg =
      Printf.sprintf
        "received permission granted from %d proposal=%s last_accepted=%s"
        msg.from proposal last
    in
    log_flow node ~routine:Stdlib.__FUNCTION__ ~msg:log_msg

  let log_rcvd_simulation_control ~node ~(msg : V.t Message.t) =
    let {id; alias; _} = node in
    let log_msg =
      Printf.sprintf
        "%s:(node %d) received a control command from the simulation. msg=(%s)"
        alias id (msg |> msg_to_string)
    in
    log_flow ~routine:Stdlib.__FUNCTION__ node ~msg:log_msg

  let log_decision node ~msg =
    Logger.decision ~node_id:(Some node.id) ~alias:(Some node.alias) node.logger
      ~msg

  (* TODO [LOG] shift to logger? *)
  let log_waiting_for_promises ({id; alias; _} as node) =
    let log_msg =
      Printf.sprintf
        "%s (node %d)'s proposer state has been waiting for promises. it will \
         accumulate this then check if a quorum is achieved!"
        alias id
    in
    log_decision node ~msg:log_msg

  let log_reached_grant_quorum ({id; alias; _} as node)
      ({assertion= {proposal; _}; _} : V.t Message.permission_granted_msg)
      ({value; _} : assertion) =
    let log_msg =
      Printf.sprintf
        "%s:(Node %i) realises that grant quorum has been reached, will carry \
         on and suggest the chosen value=(%s) with proposal=(%s)"
        alias id (value |> V.to_string)
        (proposal |> Types.proposal_id_to_string)
    in
    log_decision node ~msg:log_msg

  (* TODO [LOG] improve this usage *)
  let log_reached_nack_quorum ({id; alias; _} as node) (hint : promise) =
    let log_msg =
      Printf.sprintf
        "%s (Node %i) realises that nack quorum has been reached, will now \
         abort. The best hint we got was: %s"
        alias id (promise_to_string hint)
    in
    log_decision node ~msg:log_msg

  (* ====== HANDLERS ======== *)

  let handle_permission_request node msg =
    log_rcvd_permission_request ~node ~msg ;
    let ({meta= {topic; id= rcvd_msg_id; timestamp}; assertion; _}
          : V.t Message.permission_request_msg ) =
      msg
    in
    let msg_id = next_msg_id rcvd_msg_id in
    let time = next_time timestamp in
    let last_accepted = State.last_accepted_promise node.state in
    let reply =
      match State.is_proposal_permissible node.state assertion.proposal with
      | true ->
          log_decision node ~msg:"Permission can be granted" ;
          (* state transition *)
          State.Accepting {promised= Some assertion; accepted= last_accepted}
          |> transition_role_state node time State.Acceptor ;
          Message.make_permission_granted ~msg_id ~topic ~assertion ~time
            ~from:node.id ~last_accepted
      | false ->
          log_decision node
            ~msg:"permission request not permissible; sending NACK with hint" ;
          Message.make_nack ~msg_id ~topic ~time ~from:node.id
            ~rejected_assertion:assertion ~hint:last_accepted
    in
    let bus = bus_for_topic node topic in
    Bus.enqueue bus ((topic, Some msg.from), reply)

  let activate time node =
    let load_state () =
      match Storage.load_snapshot !(node.storage) with
      | Error e ->
          log_decision node
            ~msg:
              (Printf.sprintf "failed loading snapshot: %s"
                 (Error.to_string_hum e) ) ;
          Ok (State.idle_of ())
      | Ok None ->
          log_decision node ~msg:"no snapshot available; starting fresh" ;
          Ok (State.idle_of ())
      | Ok (Some state) ->
          log_decision node ~msg:"restoring state from snapshot" ;
          Ok state
    in
    match load_state () with
    | Error _e ->
        node.state <- State.idle_of ()
    | Ok restored -> (
        node.state <- restored ;
        match Storage.persist_snapshot !(node.storage) time restored with
        | Ok updated ->
            node.storage := updated
        | Error e ->
            log_decision node
              ~msg:
                (Printf.sprintf "persist snapshot failed after activation: %s"
                   (Error.to_string_hum e) ) )

  (** TODO: figure out how to abort.*)
  let abort node =
    (* TODO [FSM] Figuring out what aborting a paxos process means *)
    log_decision node ~msg:"aborting paxos process"

  let handle_permission_granted node msg =
    log_rcvd_permission_granted ~node ~msg ;
    let {meta= {topic; id= incoming_msg_id; timestamp}; last_accepted; _} :
        V.t Message.permission_granted_msg =
      msg
    in
    let msg_id = next_msg_id incoming_msg_id in
    let time = next_time timestamp in
    match node.state.proposer with
    | State.WaitingForPromises wfp -> (
        log_waiting_for_promises node ;
        {wfp with promises_received= last_accepted :: wfp.promises_received}
        |> State.WaitingForPromises
        |> transition_role_state node time State.Proposer ;
        match State.is_quorum_reached (get_cluster_size node) node.state with
        | State.MajorityGrants assertion ->
            log_reached_grant_quorum node msg assertion ;
            let bus = bus_for_topic node topic in
            suggest ~msg_id ~time ~bus ~assertion node ;
            State.ProposerAccepting {assertion; acks= []; nacks_received= []}
            |> transition_role_state node time State.Proposer
        | State.MajorityNacks hint ->
            log_reached_nack_quorum node hint ;
            abort node
        | State.NotReached ->
            log_decision node
              ~msg:"no quorum reached yet, we shall wait for more responses" )
    | _ ->
        noop_ignore node "not waiting for promises"

  let handle_suggestion node msg =
    log_rcvd_suggestion ~node ~msg ;
    let {meta= {topic; id= rcvd_msg_id; timestamp}; assertion; _} :
        V.t Message.suggestion_msg =
      msg
    in
    let msg_id = next_msg_id rcvd_msg_id in
    let time = next_time timestamp in
    let reply =
      match State.is_suggestion_acceptable node.state assertion.proposal with
      | true ->
          State.Accepting {promised= Some assertion; accepted= Some assertion}
          |> transition_role_state node time State.Acceptor ;
          Message.make_accepted ~msg_id ~topic ~time ~from:node.id ~assertion
      | false ->
          Message.make_nack ~msg_id ~topic ~time ~from:node.id
            ~rejected_assertion:assertion
            ~hint:(State.last_accepted_promise node.state)
    in
    let bus = bus_for_topic node topic in
    Bus.enqueue bus ((topic, Some msg.from), reply)

  let handle_accepted node msg =
    log_rcvd_accepted ~node ~msg ;
    let {meta= {id; timestamp; _}; from; _} : V.t Message.accepted_msg = msg in
    match node.state.proposer with
    | State.ProposerAccepting pa -> (
        let new_acks =
          if List.mem pa.acks from ~equal:( = ) then pa.acks
          else from :: pa.acks
        in
        State.ProposerAccepting {pa with acks= new_acks}
        |> transition_role_state node timestamp State.Proposer ;
        match State.is_quorum_reached (get_cluster_size node) node.state with
        | State.MajorityGrants assertion ->
            State.Decided assertion.value
            |> transition_role_state node timestamp State.Proposer ;
            announce_decision node ~msg_id:(next_msg_id id)
              ~time:(next_time timestamp) assertion
        | _ ->
            () )
    | _ ->
        noop_ignore node "not waiting for accepted msgs"

  let handle_decided node
      ({meta= {timestamp; _}; decided_assertion= new_assertion; _} :
        V.t Message.decided_msg ) =
    let (Learned curr_assertions) = State.get_role node.state State.Learner in
    Learned (new_assertion :: curr_assertions)
    |> transition_role_state node timestamp State.Learner

  let update_proposer_state_on_nack {state= {proposer; _}; _}
      ({hint; _} : V.t Message.nack_msg) =
    match proposer with
    | State.WaitingForPromises wp ->
        { wp with
          nacks_received=
            {rejected_assertion= wp.assertion; hint} :: wp.nacks_received }
        |> State.WaitingForPromises
    | State.ProposerAccepting pa ->
        { pa with
          nacks_received=
            {rejected_assertion= pa.assertion; hint} :: pa.nacks_received }
        |> State.ProposerAccepting
    | _ ->
        failwith
          "we should only be receiving nacks when proposer is in state \
           WaitingForPromises or ProposerAccepting "

  let handle_nack node msg =
    log_rcvd_nack ~node ~msg ;
    let {meta= {topic; id= rcvd_msg_id; timestamp}; _} : V.t Message.nack_msg =
      msg
    in
    update_proposer_state_on_nack node msg
    |> transition_role_state node timestamp State.Proposer ;
    match State.is_quorum_reached (get_cluster_size node) node.state with
    | State.MajorityGrants assertion ->
        let time = next_time timestamp in
        let bus = bus_for_topic node topic in
        suggest ~msg_id:(next_msg_id rcvd_msg_id) ~time ~bus node ~assertion ;
        State.ProposerAccepting {assertion; acks= []; nacks_received= []}
        |> transition_role_state node time State.Proposer
    | State.MajorityNacks _ ->
        abort node
    | State.NotReached ->
        ()

  let handle_coordination node msg =
    with_active ~node
      ~ignore_reason:
        "inactive right now and can't be reached to get coordinated..."
    @@ fun () ->
    log_flow ~routine:Stdlib.__FUNCTION__ ~msg:"...coordination is happening"
      node ;
    match msg |> Message.proposal_id_of with
    | None ->
        ()
    | Some _proposal_id -> (
      match msg with
      | Message.Coordination (PermissionRequest pr) ->
          pr |> handle_permission_request node
      | Message.Coordination (PermissionGranted pg) ->
          pg |> handle_permission_granted node
      | Message.Coordination (Suggestion s) ->
          s |> handle_suggestion node
      | Message.Coordination (Accepted a) ->
          a |> handle_accepted node
      | Message.Coordination (Nack n) ->
          n |> handle_nack node
      | Message.Coordination (Decided d) ->
          d |> handle_decided node
      | _ ->
          failwith
            "We can only cooordinate if we receive a coordination message." )

  (* TODO [quality] we can make this into a frozen hashtable of functions, similar to hydration here. *)
  let handle_simulation_control ({id; _} as node : t) (msg : V.t Message.t) =
    log_rcvd_simulation_control ~node ~msg ;
    match msg with
    | Message.Control (ActivateNode {node_id; meta= {timestamp; _}; _})
      when node_id = id ->
        activate timestamp node ;
        log_node_state_control node "I'm back in action"
    | Message.Control (MakeNodeIdle {node_id; _}) when node_id = id ->
        (* TODO: handle idling state *)
        node.state <- State.idle_of () ;
        log_node_state_control node "I am now idle"
    | Message.Control (MakeNodeInactive {node_id; _}) when node_id = id ->
        node.state <- State.inactive_of () ;
        log_node_state_control node "I am now inactive"
    | _ ->
        log_node_state_control node
          "but I shall do nothing about it and not change state"

  (* TODO: [extension-v1] wire this up to internal clock support *)
  (* TODO: [LOG] use the logger *)
  let handle_time ({id; alias; _} as node) (msg : V.t Message.t) =
    match msg with
    | Message.Time (Heartbeat {time; _}) ->
        let log_msg =
          Printf.sprintf
            "[%s (node %d)] felt simulation heartbeat for time=(%d)" alias id
            time
        in
        log_flow ~routine:Stdlib.__FUNCTION__ node ~msg:log_msg
    | _ ->
        ()

  (** this allows us to choose handlers based on the topic *)
  let get_handler_for_topic node topic : V.t Event_bus.bus_registrable_callback
      =
    (* TODO [LOG] this is ugly, there should be a better way to call the log for this. *)
    let msg =
      Printf.sprintf "by node %d for topic=(%s)" node.id
        (topic |> Types.topic_to_str)
    in
    log_flow ~routine:Stdlib.__FUNCTION__ ~msg node ;
    match topic with
    | Types.Coordination ->
        node |> handle_coordination
    | Types.Simulation_control ->
        node |> handle_simulation_control
    | Types.Time ->
        node |> handle_time
    | _ ->
        failwith "Unsupported topic for message passing"

  let register_node_with_bus bus
      ({config= {topics; _}; id= node_id; subs; _} as node) =
    List.iter topics ~f:(fun topic ->
        let callback msg = get_handler_for_topic node topic msg in
        (* let callback = get_handler_for_topic node topic in *)
        let subscription_handle =
          callback |> Bus.subscribe bus ~topic ~node_id
        in
        ( match Hashtbl.find subs topic with
        | Some table ->
            table
        | None ->
            let table = Hashtbl.Poly.create () in
            Hashtbl.add_exn subs ~key:topic ~data:table ;
            table )
        |> Hashtbl.add_exn ~key:subscription_handle ~data:bus ) ;
    node

  let deregister_node_from_bus bus ({config= {topics; _}; subs; _} as node) =
    List.iter topics ~f:(fun topic ->
        match Hashtbl.find subs topic with
        | Some table ->
            let keys_to_remove =
              Hashtbl.fold table ~init:[]
                ~f:(fun ~key:sub_handle ~data:sub_bus acc ->
                  if phys_equal sub_bus bus then sub_handle :: acc else acc )
            in
            Stdio.print_endline "...deregistering node, keys:" ;
            keys_to_remove
            |> List.map ~f:Event_bus.sexp_of_sub_handle
            |> List.map ~f:Sexp.to_string_hum
            |> List.iter ~f:Stdio.print_endline ;
            List.iter keys_to_remove ~f:(fun sub_handle ->
                Bus.unsubscribe bus sub_handle ;
                Hashtbl.remove table sub_handle ) ;
            if Hashtbl.is_empty table then Hashtbl.remove subs topic
        | None ->
            () ) ;
    node

  type spec =
    { node_id: int
    ; node_alias: string
    ; topic_strs: string list
    ; initial_cluster_size: int
    ; initial_state: string option [@default None] [@yojson_drop_default]
    ; storage_config: string option [@default None] [@yojson_drop_default] }
  [@@deriving sexp, yojson]

  let of_spec {node_id; node_alias; topic_strs; initial_cluster_size; _} =
    (* TODO: allow initial_state and storage config to be injected; *)
    let default_state = State.idle_of () in
    let config =
      { runtime= {cluster_size= ref initial_cluster_size}
      ; roles=
          ["Acceptor"; "Learner"; "Proposer"] |> List.filter_map ~f:role_of_str
      ; topics= topic_strs |> List.filter_map ~f:Types.topic_of_str }
    in
    { id= node_id
    ; alias= node_alias
    ; state= default_state
    ; config
    ; subs= Hashtbl.Poly.create ()
    ; storage= ref (Storage.create ~alias:node_alias ())
    ; logger= Logger.create Stdlib.__MODULE__ () }
end
