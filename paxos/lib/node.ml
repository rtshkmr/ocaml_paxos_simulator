[@@@ocaml.warning "-32-37-33-27-69"]

open Base
open Event_bus
open Types
open Message
open Log

module type S = sig
  type t

  include Has_spec with type t := t

  module V : Value.S

  module Storage : Storage.S

  module Bus : sig
    include module type of Event_bus
  end

  type role = Proposer | Acceptor | Learner

  type roles = role list

  val role_of_str : string -> role option

  type assertion = V.t Types.paxos_assertion_state [@@deriving sexp]

  type promise = assertion option [@@deriving sexp]

  (** Acceptor_record module for local acceptor state snapshot *)
  module Acceptor_record : sig
    (** The value a local acceptor holds as part of the Paxos state.

        - [promised] is the highest proposal id this acceptor has promised not to
          accept proposals less than.
        - [accepted] is the optional last accepted proposal id and value pair.
    *)
    type value = {promised: Types.proposal_id option; accepted: promise}
    [@@deriving sexp]
  end

  module State : sig
    type nack = {rejected_assertion: assertion; hint: promise} [@@deriving sexp]

    type waiting_for_promise_state =
      { assertion: assertion
      ; promises_received: promise list
      ; nacks_received: nack list }
    [@@deriving sexp]

    type proposer_accepting_state =
      {assertion: assertion; acks: Types.node_id list; nacks_received: nack list}
    [@@deriving sexp]

    type proposer_state =
      | Inactive
      | Idle
      | Preparing of assertion
      | WaitingForPromises of waiting_for_promise_state
      | ProposerAccepting of proposer_accepting_state
      | Decided of V.t
    [@@deriving sexp]

    type acceptor_state = Inactive | Idle | Accepting of Acceptor_record.value

    type learner_state = Learned of V.t option [@@deriving sexp]

    type role_state =
      { proposer: proposer_state
      ; acceptor: acceptor_state
      ; learner: learner_state }
    [@@deriving sexp]

    val idle_of : unit -> role_state

    val inactive_of : unit -> role_state

    type quorum_result =
      | NotReached
      | MajorityNacks of promise
      | MajorityGrants of assertion

    val is_quorum_reached : role_state -> int -> quorum_result
  end

  type runtime_config = {mutable cluster_size: int option ref}

  type config =
    { runtime: runtime_config
    ; roles: roles
    ; storage: Storage.t
    ; topics: Types.topic list }

  val register_node_with_bus : V.t Message.t Bus.t -> t -> t

  val roles : t -> roles

  val state : t -> State.role_state

  val handle_coordination : t -> V.t Message.t -> unit

  val handle_simulation_control : t -> V.t Message.t -> unit

  val handle_time : t -> V.t Message.t -> unit

  val propose :
       t
    -> msg_id:int
    -> time:int
    -> bus:V.t Message.t Bus.t
    -> assertion:V.t Types.paxos_assertion_state
    -> unit

  val suggest :
       msg_id:int
    -> time:int
    -> bus:V.t Message.t Bus.t
    -> t
    -> assertion:V.t Types.paxos_assertion_state
    -> unit

  val sexp_of_role_state : t -> Sexp.t

  val make_config :
       topics:Types.topic list
    -> roles:roles
    -> storage:Storage.t
    -> cluster_size:int option
    -> config

  val make_node_idle :
       msg_id:int
    -> time:int
    -> bus:'a Message.t Bus.t
    -> 'b
    -> node_id:int
    -> unit

  val get_cluster_size : t -> int

  type spec =
    { node_id: int
    ; node_alias: string
    ; topic_strs: string list
    ; initial_cluster_size: int
    ; initial_state: string option
    ; storage_config: string option }

  val of_spec : spec -> t
end

module Make_node
    (V : Value.S)
    (Storage : Storage.S)
    (Bus : sig
      include module type of Event_bus
    end) :
  S with module V := V with module Storage := Storage with module Bus := Bus =
struct
  type role = Proposer | Acceptor | Learner

  type roles = role list

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

  type inbox_entry = {mutable messages: V.t Message.t list} [@@deriving sexp_of]

  type inbox = (proposal_key, inbox_entry) Hashtbl.t

  type assertion = V.t Types.paxos_assertion_state [@@deriving sexp]

  let paxos_assertion_of_assertion (a : assertion) :
      V.t Types.paxos_assertion_state =
    {proposal= a.proposal; value= a.value}

  type promise = assertion option [@@deriving sexp]

  let paxos_promise_of_promise (p : promise) : V.t Types.paxos_promise =
    match p with
    | None ->
        None
    | Some a ->
        Some {value= a.value; proposal= a.proposal}

  let assertion_of_paxos_assertion (paxos_a : V.t Types.paxos_assertion_state) :
      assertion =
    {proposal= paxos_a.proposal; value= paxos_a.value}

  let promise_of_paxos_promise (paxos_p : V.t Types.paxos_promise) : promise =
    match paxos_p with
    | None ->
        None
    | Some paxos_a ->
        Some (assertion_of_paxos_assertion paxos_a)

  module Acceptor_record = struct
    (** Local acceptor state snapshot for Paxos *)
    type value = {promised: Types.proposal_id option; accepted: promise}
    [@@deriving sexp]
  end

  module State = struct
    type nack = {rejected_assertion: assertion; hint: promise} [@@deriving sexp]

    type waiting_for_promise_state =
      { assertion: assertion
      ; promises_received: promise list
      ; nacks_received: nack list }
    [@@deriving sexp]

    type proposer_accepting_state =
      {assertion: assertion; acks: Types.node_id list; nacks_received: nack list}
    [@@deriving sexp]

    type proposer_state =
      | Inactive
          (** this state encodes it's unavailability. When a node is inactivated, it is effectively killed -- it must look to its storage to resume partitipation thereafter *)
      | Idle
          (** An node may be idle to indicate that it can be a valid participant (by initiating a proposal)*)
      | Preparing of assertion
          (** A node that is initiating a proposal will be in the preparing state. It will be ready to .*)
      | WaitingForPromises of waiting_for_promise_state
          (** A node that is gathering responses to their proposal and is waiting to reach a quorum of responses.*)
      | ProposerAccepting of proposer_accepting_state
          (** A node that has suggested *)
      | Decided of V.t
          (** A node that has decided on the value that consensus has been achieved for*)
    [@@deriving sexp]

    type acceptor_state = Inactive | Idle | Accepting of Acceptor_record.value
    [@@deriving sexp]

    type learner_state = Learned of V.t option [@@deriving sexp]

    type role_state =
      { proposer: proposer_state
      ; acceptor: acceptor_state
      ; learner: learner_state }
    [@@deriving sexp]

    let idle_of () = {proposer= Idle; acceptor= Idle; learner= Learned None}

    let inactive_of () =
      {proposer= Inactive; acceptor= Inactive; learner= Learned None}

    (** GADT to encode which role and its sub-state type
        This encodes the association between a constructor (Proposer, Acceptor, Learner) and its precise sub-state type.
    *)
    type _ role_selector =
      | Proposer : proposer_state role_selector
      | Acceptor : acceptor_state role_selector
      | Learner : learner_state role_selector

    (** Polymorphic role_state accessor *)
    let get_role : type a. role_state -> a role_selector -> a =
     fun rs sel ->
      match sel with
      | Proposer ->
          rs.proposer
      | Acceptor ->
          rs.acceptor
      | Learner ->
          rs.learner

    (** Polymorphic role_state setter *)
    let set_role : type a. role_state -> a role_selector -> a -> role_state =
     fun rs sel v ->
      match sel with
      | Proposer ->
          {rs with proposer= v}
      | Acceptor ->
          {rs with acceptor= v}
      | Learner ->
          {rs with learner= v}

    type quorum_result =
      | NotReached
      | MajorityNacks of promise
      | MajorityGrants of assertion

    (* TODO Verify this quorum determination below for the WaitingForPromises is correct *)

    (** Consider the highest proposal_id from all promises received.
          case 1: it's None, use proposers own value
          case 2: it's Some, pick associated value for highest proposal id
    *)
    let is_quorum_reached_on_promise_wait
        {promises_received; nacks_received; assertion} cluster_size =
      let threshold = (cluster_size / 2) + 1 in
      if List.length promises_received >= threshold then
        let chosen_value =
          List.fold ~init:assertion
            ~f:(fun acc promise_rcvd ->
              match (acc, promise_rcvd) with
              | acc, None ->
                  acc
              | ( {proposal= best_proposal; value= best_val}
                , Some
                    ( { proposal= prev_accepted_proposal
                      ; value= prev_accepted_val } as prev_promise ) ) ->
                  if
                    Types.compare_proposal_id prev_accepted_proposal
                      best_proposal
                    > 0
                  then prev_promise
                  else acc )
            promises_received
        in
        MajorityGrants chosen_value
      else if List.length nacks_received >= threshold then
        let hint =
          List.fold ~init:None
            ~f:(fun acc {rejected_assertion; hint} ->
              match (acc, hint) with
              | _, None ->
                  acc
              | None, Some prev_accepted_assertion ->
                  hint
              | Some acc_assertion, Some prev_accepted_assertion ->
                  if
                    Types.compare_proposal_id prev_accepted_assertion.proposal
                      acc_assertion.proposal
                    > 0
                  then hint
                  else acc )
            nacks_received
        in
        MajorityNacks hint
      else NotReached

    let is_quorum_reached_on_proposer_accepting_wait
        {acks; nacks_received; assertion} cluster_size =
      let threshold = (cluster_size / 2) + 1 in
      if List.length acks >= threshold then MajorityGrants assertion
      else if List.length nacks_received >= threshold then
        let hint =
          List.fold ~init:None
            ~f:(fun acc {rejected_assertion; hint} ->
              match (acc, hint) with
              | _, None ->
                  acc
              | None, Some prev_accepted_assertion ->
                  hint
              | Some acc_assertion, Some prev_accepted_assertion ->
                  if
                    Types.compare_proposal_id prev_accepted_assertion.proposal
                      acc_assertion.proposal
                    > 0
                  then hint
                  else acc )
            nacks_received
        in
        MajorityNacks hint
      else NotReached

    let is_quorum_reached rs cluster_size =
      match get_role rs Proposer with
      | WaitingForPromises wfp ->
          is_quorum_reached_on_promise_wait wfp cluster_size
      | ProposerAccepting pa ->
          is_quorum_reached_on_proposer_accepting_wait pa cluster_size
      | _ ->
          assert false
    (* "We can only check for quorum reached on a nodes if that nodes is one of the states: [WaitingForPromises, ProposerAccepting]" *)
  end

  type runtime_config = {mutable cluster_size: int option ref}

  type config =
    { runtime: runtime_config
    ; roles: roles
    ; storage: Storage.t
    ; topics: Types.topic list }

  type t =
    { id: Types.node_id
    ; alias: string
    ; config: config
    ; inbox: inbox
    ; mutable state: State.role_state
    ; mutable subs:
        (Types.topic, (Bus.sub_handle, V.t Message.t Bus.t) Hashtbl.t) Hashtbl.t
    ; logger: string Logger.t
    ; transitions: string list ref
          (* TODO [REFACTOR] YAGNI:light-weight history for debugging *) }

  (* TODO: [REFACTOR] I think such accessors are useless, we can pattern-match destructure them anyway. Keep only if we wanna hide the internal state struct @ the interface boundary *)
  let roles t = t.config.roles

  let state t = t.state

  let buses_for_topic t topic =
    match Hashtbl.find t.subs topic with
    | Some table ->
        Hashtbl.fold table ~init:[] ~f:(fun ~key:_ ~data:bus acc -> bus :: acc)
    | None ->
        []

  let bus_for_topic t topic =
    match buses_for_topic t topic with
    | [] ->
        assert
          false (* "Impossible case, should always have at least one bus" *)
    | bus :: _ ->
        bus

  let get_cluster_size t =
    Option.value !(t.config.runtime.cluster_size) ~default:0

  let sexp_of_role_state t = State.sexp_of_role_state t.state

  (* DEPRECATED: consider removal of stateful inboxes *)
  let get_or_create_inbox_entry (node : t) (key : proposal_key) =
    match Hashtbl.find node.inbox key with
    | Some inbox_entry ->
        inbox_entry
    | None ->
        let new_inbox_entry = {messages= []} in
        Hashtbl.set node.inbox ~key ~data:new_inbox_entry ;
        new_inbox_entry

  (* DEPRECATED: consider removal of stateful inboxes *)
  let dump_inbox (node : t) =
    let alist = Hashtbl.to_alist node.inbox in
    let sexp =
      List.sexp_of_t
        (Sexplib.Conv.sexp_of_pair Types.sexp_of_proposal_id sexp_of_inbox_entry)
        alist
    in
    Stdio.printf "Inbox for node %d:\n%s\n%!" node.id
      (Sexplib.Sexp.to_string_hum sexp)

  (** Polymorphic transition function for role state.

      Learning NOTE:
      1. Importance of locally abstract types for type safety
      - [(type a)] introduces a locally abstract type [a] scoped within the function, tied by GADT patterns to a specific substate type ([proposer_state], [acceptor_state], or [learner_state]).
      - Locally abstract types enable type-safe polymorphic dispatch: each constructor of the GADT carries different precise type information for ['a].
      - The function can only accept or return values consistent with ['a] as determined by the GADT constructor.
      - This is what makes GADT-based functions type-safe and flexible without unsafe casts or polymorphic variants.
  *)
  let transition_role_state node (type a) (sel : a State.role_selector)
      (new_substate : a) =
    let curr = node.state in
    let new_role_state = State.set_role curr sel new_substate in
    let old_state_str =
      Sexplib.Sexp.to_string_hum (State.sexp_of_role_state curr) ~indent:4
    in
    let new_state_str =
      Sexplib.Sexp.to_string_hum
        (State.sexp_of_role_state new_role_state)
        ~indent:4
    in
    Logger.log_node_state_change node.logger node.id old_state_str new_state_str ;
    node.state <- new_role_state

  (** Represents the act of a node driving the first step of the paxos process (asking for permission).
      This means that the node's state as a Proposer will change from [ Idle ] to [ Peparing ], as we create the message then dispatch it. Once done dispatching,
      the node will change its state to [ WaitingForPromises ], marking it ready to receive responses (both grants and nacks) for that proposal.

      As such, every paxos process can be uniquely identified via its [proposal_id]
   *)
  let propose t ~msg_id ~time ~bus ~(assertion : V.t Types.paxos_assertion_state)
      =
    let proposal = assertion.proposal in
    let value = assertion.value in
    match t.state.proposer with
    | State.Idle ->
        {value; proposal} |> State.Preparing
        |> transition_role_state t State.Proposer ;
        let perm_request_msg =
          Message.make_permission_request ~msg_id ~topic:Types.Coordination
            ~from:t.id ~proposal ~value ~time
        in
        let msg = Message.Coordination perm_request_msg in
        let thunk = ((Types.Coordination, None), msg) in
        Bus.enqueue bus thunk ;
        let assertion : assertion = {value; proposal} in
        {assertion; promises_received= []; nacks_received= []}
        |> State.WaitingForPromises
        |> transition_role_state t State.Proposer
    | _ ->
        assert false (* we can only propose if we are currently idle *)

  (* TODO: [FSM] need to have a state change within the node after suggesting? This should allow us to capture the incoming accepted or something *)
  let suggest ~msg_id ~time ~bus t ~assertion =
    let coord_msg =
      Message.make_suggestion ~msg_id ~time ~topic:Types.Coordination ~from:t.id
        ~assertion
    in
    let msg = Message.Coordination coord_msg in
    let thunk = ((Types.Coordination, None), msg) in
    Bus.enqueue bus thunk

  (* TODO: this doesn't feel right. It should be the simulator that directly can do this (making of nodes idle). In that way, [ make_node_idle ] should just do the state transitions and any kind of savings or something? *)
  let make_node_idle ~msg_id ~time ~bus t ~node_id =
    let sim_ctrl_msg =
      Message.make_sim_control_idle_node ~msg_id ~time ~node_id
    in
    let msg = Message.Control sim_ctrl_msg in
    let thunk = ((Types.Simulation_control, Some node_id), msg) in
    Bus.enqueue bus thunk

  (* unsubscribe helpers *)
  let shutdown t =
    Stdio.print_endline ("Shutting down node: " ^ Int.to_string t.id) ;
    Hashtbl.iteri t.subs ~f:(fun ~key:_ ~data:inner_table ->
        Hashtbl.iteri inner_table ~f:(fun ~key:handle ~data:bus ->
            Bus.unsubscribe bus handle ) ;
        Hashtbl.clear inner_table ) ;
    Hashtbl.clear t.subs

  (* DEPRECATED *)
  let process_inboxes (node : t) : unit =
    dump_inbox node ;
    Hashtbl.iteri node.inbox ~f:(fun ~key:proposal_id ~data:inbox_entry ->
        let _needs_quorum msg =
          match msg with
          | Message.Coordination coordination_msg -> (
            match coordination_msg with
            | Message.PermissionGranted _ | Message.Accepted _ ->
                true
            | Message.PermissionRequest _
            | Message.Suggestion _
            | Message.Nack _ ->
                false )
          | Message.Control _ ->
              false
          | Message.Time _ ->
              false
        in
        let quorum_reached = true in
        (* let quorum_reached = is_quorum_reached node inbox_entry ~predicate:needs_quorum in *)
        if quorum_reached then
          Stdio.printf "Node %d quorum reached for proposal %s\n%!" node.id
            (Sexp.to_string (Types.sexp_of_proposal_id proposal_id))
        (* Clear inbox or mark done for this proposal *)
          else (
          Stdio.printf "Node %d quorum NOT YET reached for proposal %s\n%!"
            node.id
            (Sexp.to_string (Types.sexp_of_proposal_id proposal_id)) ;
          () ) )

  let is_permissible ~current_promised_opt proposal =
    match current_promised_opt with
    | None ->
        true
    (* TODO: [verify] the following condition: (*
      > Upon receipt of a Permission Request message:
      > The peer must grant permission for requests with Suggestion IDs equal to or higher than any they have previously granted permission for. In doing so, the peer implicitly promises to reject all Permission Request and Suggestion messages with lower Suggestion IDs. Consequently, requests with IDs less than the ID last granted permission to must be ignored or responded to with a Nack message.
    *)*)
    | Some promised ->
        Types.compare_proposal_id proposal promised >= 0

  let handle_permission_request node
      ({ meta= {topic; id; timestamp}
       ; from
       ; assertion= {proposal; value} as assertion } :
        V.t Message.permission_request_msg ) =
    match State.get_role node.state State.Acceptor with
    | State.Inactive ->
        Logger.log_reaction node.logger
          (Printf.sprintf "Node %d is Inactive so it's not going to do anything"
             node.id )
    | _ ->
        let log_msg =
          Printf.sprintf
            "... node %d received permission request from %d with proposal=%s \
             for value=(%s)"
            node.id from
            (Sexp.to_string_hum (Types.sexp_of_proposal_id proposal))
            (V.to_string value)
        in
        Logger.log_subroutine_flow node.logger Stdlib.__FUNCTION__ ~msg:log_msg
          () ;
        let msg_id = 1 + id in
        let time = 1 + timestamp in
        let current_promised_opt, prev_accepted_opt =
          match State.get_role node.state State.Acceptor with
          | State.Idle ->
              Logger.log_decision node.logger
                (Printf.sprintf
                   "Node %d Acceptor was idle; no current promised / \
                    previously accepted to report. Carrying on..."
                   node.id ) ;
              (None, None)
          | State.Accepting record ->
              (record.promised, record.accepted)
          | _ ->
              assert false
        in
        let reply_msg =
          if is_permissible ~current_promised_opt proposal then (
            let updated_record =
              { Acceptor_record.promised= Some proposal
              ; accepted= prev_accepted_opt }
            in
            updated_record |> State.Accepting
            |> transition_role_state node State.Acceptor ;
            let log_msg =
              Printf.sprintf
                "the permission request is permissible. new_state: (%s)"
                (Sexp.to_string_hum
                   (Acceptor_record.sexp_of_value updated_record) )
            in
            Logger.log_decision node.logger log_msg ;
            Message.make_permission_granted ~msg_id ~topic ~assertion ~time
              ~from:node.id ~last_accepted:prev_accepted_opt )
          else
            let log_msg =
              "the permission request is NOT permissible. we shall send a NACK \
               with hint"
            in
            Logger.log_decision node.logger log_msg ;
            Message.make_nack ~msg_id ~topic ~time ~from:node.id
              ~rejected_assertion:assertion
              ~hint:(prev_accepted_opt |> paxos_promise_of_promise)
        in
        let bus = bus_for_topic node topic in
        let thunk = ((topic, Some from), reply_msg |> Message.Coordination) in
        Bus.enqueue bus thunk

  (** TODO: figure out how to abort.*)
  let abort node =
    let open Color in
    (* TODO [FSM] Figuring out what aborting a paxos process means *)
    let log_msg =
      "the permission request is NOT permissible. we shall send a NACK with \
       hint" |> Color.red
    in
    Logger.log_decision node.logger log_msg

  (* FIXME: message and state struct mismatch *)
  let handle_permission_granted node
      ({ meta= {topic; id; timestamp}
       ; from
       ; assertion= {proposal; _}
       ; last_accepted } :
        V.t Message.permission_granted_msg ) =
    let last_accepted_str =
      match last_accepted with
      | None ->
          "None"
      | Some assertion ->
          Sexp.to_string_hum
            (Types.sexp_of_paxos_assertion_state V.sexp_of_t assertion)
    in
    let log_msg =
      Printf.sprintf
        "... node %d received permission granted from %d with proposal=%s for \
         last_accepted=(%s)"
        node.id from
        (Sexp.to_string_hum (Types.sexp_of_proposal_id proposal))
        last_accepted_str
    in
    Logger.log_subroutine_flow node.logger Stdlib.__FUNCTION__ ~msg:log_msg () ;
    let msg_id = 1 + id in
    let time = 1 + timestamp in
    match node.state.proposer with
    | State.WaitingForPromises wfp -> (
        let log_msg =
          Printf.sprintf
            "node %d's proposer state has been waiting for promises. it will \
             accumulate this then check if a quorum is achieved!"
            node.id
        in
        Logger.log_decision node.logger log_msg ;
        (* TODO [FSM] what should the grant info contain? *)
        let new_promise_rcvd = last_accepted |> promise_of_paxos_promise in
        {wfp with promises_received= new_promise_rcvd :: wfp.promises_received}
        |> State.WaitingForPromises
        |> transition_role_state node State.Proposer ;
        match State.is_quorum_reached node.state (get_cluster_size node) with
        | State.MajorityGrants assertion ->
            let bus = bus_for_topic node topic in
            let log_msg =
              Printf.sprintf
                "Node %i realises that quorum has been reached, will suggest \
                 the chosen value=(%s) with proposal=(%s)"
                node.id
                (V.to_string assertion.value)
                (Sexp.to_string_hum (Types.sexp_of_proposal_id proposal))
            in
            Logger.log_decision node.logger log_msg ;
            suggest ~msg_id ~time ~bus node ~assertion ;
            {assertion; acks= []; nacks_received= []}
            |> State.ProposerAccepting
            |> transition_role_state node State.Proposer
        | State.MajorityNacks _ ->
            Logger.log_decision node.logger
              "We reached a quorum and got majority nacks... time to abort" ;
            abort node
        | State.NotReached ->
            () )
    | _ ->
        assert
          false (* Met an impossible case when handling permission granted. *)

  let is_acceptable proposal current_promised_opt =
    Option.is_some current_promised_opt
    && Types.compare_proposal_id proposal
         (Option.value_exn current_promised_opt)
       >= 0

  let handle_suggestion node
      ({ meta= {topic; id; timestamp}
       ; from
       ; assertion= {proposal; value} as assertion } :
        V.t Message.suggestion_msg ) =
    let log_msg =
      Printf.sprintf
        "... node %d received suggestion from %d with proposal=%s for \
         value=(%s)"
        node.id from
        (Sexp.to_string_hum (Types.sexp_of_proposal_id proposal))
        (V.to_string value)
    in
    Logger.log_subroutine_flow node.logger Stdlib.__FUNCTION__ ~msg:log_msg () ;
    let msg_id = 1 + id in
    let time = 1 + timestamp in
    let current_promised_opt, prev_accepted_opt =
      match State.get_role node.state State.Acceptor with
      | State.Idle ->
          (None, None)
      | State.Accepting record ->
          (record.promised, record.accepted)
      | _ ->
          assert false
    in
    let reply_msg =
      if is_acceptable proposal current_promised_opt then (
        let updated_record =
          { Acceptor_record.promised= Some proposal
          ; accepted= Some {proposal; value} }
        in
        updated_record |> State.Accepting
        |> transition_role_state node State.Acceptor ;
        Message.make_accepted ~msg_id ~topic ~time ~from:node.id ~proposal
          ~value )
      else
        (* TODO [FSM] make_nack message needs to include curent_promised *)
        Message.make_nack ~msg_id ~topic ~time ~from:node.id
          ~rejected_assertion:assertion
          ~hint:(prev_accepted_opt |> paxos_promise_of_promise)
    in
    let bus = bus_for_topic node topic in
    let thunk = ((topic, Some from), Message.Coordination reply_msg) in
    Bus.enqueue bus thunk

  let handle_accepted node
      ({meta= {topic; id; timestamp}; from; assertion= {proposal; value}} :
        V.t Message.accepted_msg ) =
    let log_msg =
      Printf.sprintf
        "... node %d received accepted from %d for proposal=%s for value=(%s)"
        node.id from
        (Sexp.to_string_hum (Types.sexp_of_proposal_id proposal))
        (V.to_string value)
    in
    Logger.log_subroutine_flow node.logger Stdlib.__FUNCTION__ ~msg:log_msg () ;
    match node.state.proposer with
    | State.ProposerAccepting a -> (
        let new_acks =
          if List.mem a.acks from ~equal:( = ) then a.acks else from :: a.acks
        in
        {a with acks= new_acks} |> State.ProposerAccepting
        |> transition_role_state node State.Proposer ;
        match State.is_quorum_reached node.state (get_cluster_size node) with
        | State.MajorityGrants assertion ->
            assertion.value |> State.Decided
            |> transition_role_state node State.Proposer
            (* TODO: [FSM] determine if we should be broadcasting that the state is decided?? *)
            (* TODO: handle NACK optimisation later*)
        | _ ->
            (* BUG: seems like on nacks, can't calculate quorum reached properly. *)
            Stdio.print_endline "WALDO looks like can't be decided" )
    | _ ->
        failwith "Met an impossible case when handling accepted."

  (* TODO: [TEMP,REFACTOR] this is temp because the messages need to be better fittign to the state that is kept. *)
  let convert_hint_msg_to_hint_promise (hint_msg : V.t Types.paxos_promise) :
      promise =
    hint_msg |> promise_of_paxos_promise

  let handle_nack node
      ({ meta= {topic; id; timestamp}
       ; rejected_assertion= {proposal; _}
       ; from
       ; hint } :
        V.t Message.nack_msg ) =
    let hint_str = hint |> sexp_of_promise |> Sexp.to_string_hum in
    let log_msg =
      Printf.sprintf
        "... node %d received NACK from %d for proposal=%s for hint=(%s)"
        node.id from
        (Sexp.to_string_hum (Types.sexp_of_proposal_id proposal))
        hint_str
    in
    Logger.log_subroutine_flow node.logger Stdlib.__FUNCTION__ ~msg:log_msg () ;
    ( match node.state.proposer with
    | State.WaitingForPromises wp ->
        { wp with
          nacks_received=
            { rejected_assertion= wp.assertion
            ; hint= hint |> promise_of_paxos_promise }
            :: wp.nacks_received }
        |> State.WaitingForPromises
    | State.ProposerAccepting pa ->
        { pa with
          nacks_received=
            { rejected_assertion= pa.assertion
            ; hint= hint |> promise_of_paxos_promise }
            :: pa.nacks_received }
        |> State.ProposerAccepting
    | _ ->
        assert false
        (* we should only be receiving nacks proposer is in state WaitingForPromises or ProposerAccepting *)
    )
    |> transition_role_state node State.Proposer ;
    match State.is_quorum_reached node.state (get_cluster_size node) with
    | State.MajorityGrants best ->
        best
        |> fun assertion ->
        let msg_id = 1 + id in
        let time = 1 + timestamp in
        let bus = bus_for_topic node topic in
        suggest ~msg_id ~time ~bus node ~assertion ;
        {assertion; acks= []; nacks_received= []}
        |> State.ProposerAccepting
        |> transition_role_state node State.Proposer
    | State.MajorityNacks _ ->
        abort node
    | State.NotReached ->
        ()

  let handle_coordination node msg =
    Logger.log_subroutine_flow node.logger Stdlib.__FUNCTION__
      ~msg:"...coordination is happening" () ;
    match Message.proposal_id_of msg with
    | None ->
        ()
    | Some proposal_id -> (
      match msg with
      | Message.Coordination (PermissionRequest pr) ->
          handle_permission_request node pr
      | Message.Coordination (PermissionGranted pg) ->
          handle_permission_granted node pg
      | Message.Coordination (Suggestion s) ->
          handle_suggestion node s
      | Message.Coordination (Accepted a) ->
          handle_accepted node a
      | Message.Coordination (Nack n) ->
          handle_nack node n
      | _ ->
          () )

  let handle_simulation_control (node : t) (msg : V.t Message.t) =
    let log_msg =
      Printf.sprintf
        "node %d received a control command from the simulation. msg=(%s)"
        node.id
        (Sexp.to_string (Message.sexp_of_t V.sexp_of_t msg))
    in
    Logger.log_subroutine_flow node.logger Stdlib.__FUNCTION__ ~msg:log_msg () ;
    match msg with
    | Message.Control (MakeNodeIdle {node_id; _}) when node_id = node.id ->
        node.state <- State.idle_of () ;
        Stdio.printf "---> Node %d was made idle\n%!" node.id
    | Message.Control (MakeNodeInactive {node_id; _}) when node_id = node.id ->
        node.state <- State.inactive_of () ;
        Stdio.printf "---> Node %d was made inactive\n%!" node.id
    | _ ->
        Stdio.printf
          "---> Node %d received simulation control but did nothing \n%!"
          node.id

  (* TODO: [extension-v1] wire this up to internal clock support *)
  let handle_time (node : t) (msg : V.t Message.t) =
    match msg with
    | Message.Time (Heartbeat {time; _}) ->
        let log_msg =
          Printf.sprintf "node %d felt simulation heartbeat for time=(%d)"
            node.id time
        in
        Logger.log_subroutine_flow node.logger Stdlib.__FUNCTION__ ~msg:log_msg
          ()
    | _ ->
        ()

  (** this allows us to choose handlers based on the topic *)
  let get_handler_for_topic (node : t) (topic : Types.topic) :
      V.t Event_bus.bus_registrable_callback =
    let msg =
      Printf.sprintf "by node %d for topic=(%s)" node.id
        (Sexp.to_string_hum (Types.sexp_of_topic topic))
    in
    Logger.log_subroutine_flow node.logger Stdlib.__FUNCTION__ ~msg () ;
    match topic with
    | Types.Coordination ->
        handle_coordination node
    | Types.Simulation_control ->
        handle_simulation_control node
    | Types.Time ->
        handle_time node
    | _ ->
        failwith "Unsupported topic for message passing"

  let register_node_with_bus bus ({config= {topics; _}; id; _} as node) =
    List.iter topics ~f:(fun topic ->
        let topic_table =
          match Hashtbl.find node.subs topic with
          | Some table ->
              table
          | None ->
              let table = Hashtbl.Poly.create () in
              Hashtbl.add_exn node.subs ~key:topic ~data:table ;
              table
        in
        let callback msg = get_handler_for_topic node topic msg in
        let subscription_handle =
          callback |> Bus.subscribe bus ~topic ~node_id:id
        in
        Hashtbl.add_exn topic_table ~key:subscription_handle ~data:bus ) ;
    node

  let make_config ~topics ~roles ~storage ~cluster_size : config =
    let cluster_size_opt =
      match cluster_size with
      | None ->
          None
      | Some x when x > 0 ->
          Some x
      | _ ->
          invalid_arg "cluster_size must be > 0 or None"
    in
    {topics; runtime= {cluster_size= ref cluster_size_opt}; roles; storage}

  type spec =
    { node_id: int
    ; node_alias: string
    ; topic_strs: string list
    ; initial_cluster_size: int
    ; initial_state: string option
    ; storage_config: string option }

  let of_spec
      { node_id
      ; node_alias
      ; topic_strs
      ; initial_cluster_size
      ; initial_state
      ; storage_config } =
    (* TODO: allow initial_state to be injected; *)
    let default_state = State.idle_of () in
    let config =
      { runtime= {cluster_size= ref (Some initial_cluster_size)}
      ; roles=
          ["Acceptor"; "Learner"; "Proposer"] |> List.filter_map ~f:role_of_str
      ; storage= Storage.create ()
      ; topics= topic_strs |> List.filter_map ~f:Types.topic_of_str }
    in
    { id= node_id
    ; alias= node_alias
    ; state= default_state
    ; config
    ; subs= Hashtbl.Poly.create ()
    ; inbox= Hashtbl.Poly.create ()
    ; transitions= ref []
    ; logger= Logger.create () }
end
