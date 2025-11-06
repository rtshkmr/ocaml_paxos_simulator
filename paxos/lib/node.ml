[@@@ocaml.warning "-32-37-33-27-69"] (** TODO: remove unused variable warnings*)

open Base
open Event_bus
open Types
open Message
open Log

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
    type promise = (Types.node_id * (Types.proposal_id * V.t) option) [@@deriving sexp]
    (* nack = source * proposal_id * hint *)
    type nack = (Types.node_id * (Types.proposal_id * V.t) option * (Types.proposal_id option)) [@@deriving sexp]

    type waiting_for_promise_state =
      { proposal: Types.proposal_id
      ; promises_received: promise list
      ; nacks_received: nack list }
    [@@deriving sexp]

    type proposer_accepting_state =
      { proposal: Types.proposal_id
      ; value: V.t
      ; acks: Types.node_id list
      ; nacks_received: nack list }
    [@@deriving sexp]

    type proposer_state =
      | Inactive
      | Idle
      | Preparing
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

    val idle_of: unit -> role_state
    val inactive_of: unit -> role_state

    type quorum_result =
      | NotReached
      | MajorityNacks of (Types.proposal_id * V.t) option
      | MajorityGrants of (Types.proposal_id * V.t) option

    val is_quorum_reached : role_state -> int -> quorum_result
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

  val seek_permission :
    msg_id:int
    -> time:int
    -> bus:V.t Message.t Bus.t
    -> t
    -> proposal:Types.proposal_id
    -> value:V.t
    -> unit

  val suggest :
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

  val get_cluster_size: t -> int
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
    (* promise = source node id * (proposal id  * val) option *)
    type promise = (Types.node_id * (Types.proposal_id * V.t) option) [@@deriving sexp]
    (* nack = source * proposal_id * hint *)
    type nack =
      Types.node_id
      * (Types.proposal_id * V.t) option
      * Types.proposal_id option
    [@@deriving sexp]

    type waiting_for_promise_state =
      { proposal: Types.proposal_id
      ; promises_received: promise list
      ; nacks_received: nack list }
    [@@deriving sexp]

    type proposer_accepting_state =
      { proposal: Types.proposal_id
      ; value: V.t
      ; acks: Types.node_id list
      ; nacks_received: nack list }
    [@@deriving sexp]

    type proposer_state =
      | Inactive
      | Idle
      | Preparing (* TODO [FSM] we ended up using Idle instead of preparing. I think propose subroutine should do Idle -> proposing -> WFP instead*)
      | WaitingForPromises of waiting_for_promise_state
      | ProposerAccepting of proposer_accepting_state
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

    type quorum_result =
      | NotReached
      | MajorityNacks of (Types.proposal_id * V.t) option
      | MajorityGrants of (Types.proposal_id * V.t) option

    let is_quorum_reached_on_promise_wait (wfp:waiting_for_promise_state) ( cluster_size:int ) : quorum_result =
        let promises = wfp.promises_received in
        let nacks = wfp.nacks_received in
        let promises_count = List.length promises in
        let nacks_count = List.length nacks in
        let threshold = (cluster_size / 2) + 1 in

        if promises_count >= threshold then
          let best_value_opt =
            List.fold_left
              ~f:(fun acc (_, proposal_opt) ->
                  match proposal_opt, acc with
                  | Some (proposal_id, value), None -> Some (proposal_id, value)
                  | Some (proposal_id, value), Some (best_pid, best_val) ->
                    if Types.compare_proposal_id proposal_id best_pid > 0 then Some (proposal_id, value) else acc
                  | None, _ -> acc)
              ~init:None
              promises
          in
          MajorityGrants best_value_opt
        else if nacks_count >= threshold then
          let best_value_opt =
            List.fold_left
              ~f:(fun acc (_, hint_opt, _) ->
                  match hint_opt, acc with
                  | Some (proposal_id, value), None -> Some (proposal_id, value)
                  | Some (proposal_id, value), Some (best_pid, best_val) ->
                    if Types.compare_proposal_id proposal_id best_pid > 0 then Some (proposal_id, value) else acc
                  | None, _ -> acc)
              ~init:None
              nacks
          in
          MajorityNacks best_value_opt

        else
          NotReached

    let is_quorum_reached_on_proposer_accepting_wait (pa:proposer_accepting_state) ( cluster_size:int ) : quorum_result =
        let num_acks = List.length pa.acks in
        let nacks = pa.nacks_received in
        let num_nacks = List.length nacks in
        let threshold = (cluster_size / 2) + 1 in

        if num_acks >= threshold then
          let best_value_opt = Some (pa.proposal, pa.value) in
          MajorityGrants best_value_opt;

        else if num_nacks >= threshold then
          let best_value_opt =
            List.fold_left
              ~f:(fun acc (_, hint_opt, _) ->
                  match hint_opt, acc with
                  | Some (proposal_id, value), None -> Some (proposal_id, value)
                  | Some (proposal_id, value), Some (best_pid, best_val) ->
                    if Types.compare_proposal_id proposal_id best_pid > 0 then Some (proposal_id, value) else acc
                  | None, _ -> acc)
              ~init:None
              nacks in
            MajorityNacks best_value_opt
        else
          NotReached



    let is_quorum_reached (rs : role_state) (cluster_size : int) : quorum_result =
      match get_role rs Proposer with
      | WaitingForPromises wfp ->
        is_quorum_reached_on_promise_wait wfp cluster_size
      | ProposerAccepting pa ->
        is_quorum_reached_on_proposer_accepting_wait pa cluster_size
      | _ ->
        failwith "We can only check for quorum reached on a nodes if that nodes is in states [WaitingForPromises, ProposerAccepting]"
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
    logger: string Logger.t;
    transitions : string list ref;  (* light-weight history for debugging *)
  }

  let id t = t.id
  let roles t = t.config.roles
  let state t = t.state
  let buses_for_topic t topic =
    match Hashtbl.find t.subs topic with
    | Some table -> Hashtbl.fold table ~init:[] ~f:(fun ~key:_ ~data:bus acc -> bus :: acc)
    | None -> []

  let get_cluster_size t = Option.value !(t.config.simulation.cluster_size) ~default:0

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
  let seek_permission ~msg_id ~time ~bus t ~proposal ~value =
    (* Build PermissionRequest for this node *)
    let coord_msg = Message.make_permission_request ~msg_id ~time ~topic:Types.Coordination ~from:t.id ~proposal ~value in
    let msg = Message.Coordination coord_msg in
    let thunk = (Types.Coordination, None ), msg   in
    (* For v0 we'll have simulator broadcast on behalf of node; but provide direct publish too *)
    Bus.enqueue bus thunk

  let suggest ~msg_id ~time ~bus t ~proposal ~value =
    let coord_msg = Message.make_suggestion ~msg_id ~time ~topic:Types.Coordination ~from:t.id ~proposal ~value in
    let msg = Message.Coordination coord_msg in
    let thunk = (Types.Coordination, None ), msg   in
    Bus.enqueue bus thunk

  let make_node_idle ~msg_id ~time ~bus t ~node_id =
    let sim_ctrl_msg = Message.make_sim_control_idle_node ~msg_id ~time ~node_id in
    let msg = Message.Control sim_ctrl_msg in
    let thunk = (Types.Simulation_control, Some node_id), msg in
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

  (* DEPRECATED *)
  let process_inboxes (node: t) : unit =
    dump_inbox node;
    Hashtbl.iteri node.inbox ~f:(fun ~key:proposal_id ~data:inbox_entry ->
        let _needs_quorum msg =
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
        let quorum_reached = true in
        (* let quorum_reached = is_quorum_reached node inbox_entry ~predicate:needs_quorum in *)
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

  let is_permissible ~current_promised_opt proposal =
    match current_promised_opt with
    | None -> true
    | Some promised -> Types.compare_proposal_id promised proposal < 0

  let handle_permission_request node ({meta={topic; id; timestamp}; from; proposal; value}: V.t Message.permission_request_msg) =
    let msg = Printf.sprintf "... node %d received permission request from %d with proposal=%s for value=(%s)"  node.id from (Sexp.to_string_hum(Types.sexp_of_proposal_id proposal)) (V.to_string(value) ) in
    Logger.log_subroutine_flow node.logger Stdlib.__FUNCTION__ ~msg ();
    let msg_id = 1 + id in
    let time = 1 + timestamp in
    let (current_promised_opt, prev_accepted_opt) =
      match State.get_role node.state State.Acceptor with
      (* TODO [FSM]: when will it be inactive? do we transition out to inactive after? *)
      | State.Inactive | State.Idle -> Logger.log_decision node.logger ( Printf.sprintf "Node %d Acceptor was idle/inactive; no current promised / previously accepted to report. Carrying on..." node.id); (None, None)
      | State.Accepting record -> (record.promised, record.accepted)
    in
    let reply_msg =
      if is_permissible ~current_promised_opt proposal then (
        let updated_record = { Acceptor_record.promised = Some proposal; accepted = prev_accepted_opt } in
        node.state <- State.set_role node.state State.Acceptor (State.Accepting updated_record);

        let log_msg = Printf.sprintf "the permission request is permissible. new_state: ( %s )" (Sexp.to_string_hum(Acceptor_record.sexp_of_value(updated_record))) in
        Logger.log_decision node.logger log_msg ;
        Message.Coordination (Message.make_permission_granted ~msg_id ~topic ~proposal ~time ~from:node.id ~last_accepted:prev_accepted_opt)
      ) else
        let log_msg = "the permission request is NOT permissible. we shall send a NACK with hint" in
        Logger.log_decision node.logger log_msg ;
        Message.Coordination (Message.make_nack ~msg_id ~topic ~time ~from:node.id ~proposal ~hint:prev_accepted_opt)
    in
    buses_for_topic node topic
    |> function
    | [] -> failwith "Impossible case, should always have at least one bus"
    | bus :: _ -> Bus.enqueue bus ((topic, Some from), reply_msg)

  (** TODO: figure out how to abort.*)
  let abort node =
    let open Color in
    (* TODO [FSM] Figuring out what aborting a paxos process means *)
    let log_msg = ( "the permission request is NOT permissible. we shall send a NACK with hint" |> Color.red ) in
    Logger.log_decision node.logger log_msg

  let handle_permission_granted node ({meta={topic;id;timestamp}; from; proposal; last_accepted} : V.t Message.permission_granted_msg) =
   let last_accepted_str = match last_accepted with
      | None -> "None"
      | Some (proposal_key, v) ->
        let sexp = Sexplib.Sexp.List [
          Types.sexp_of_proposal_id proposal_key;
          V.sexp_of_t v
        ] in
        Sexplib.Sexp.to_string_hum sexp in
    let msg = Printf.sprintf "... node %d received permission granted from %d with proposal=%s for last_accepted=(%s)"  node.id from (Sexp.to_string_hum(Types.sexp_of_proposal_id proposal)) last_accepted_str in
    Logger.log_subroutine_flow node.logger Stdlib.__FUNCTION__ ~msg ();
    let msg_id = 1 + id in
    let time = 1 + timestamp in
    let handle_on_quorum_reached best_value_opt =
      let chosen_value =
        match best_value_opt with
        | Some (_, v) -> v
        (* TODO [FSM BUG 1]: proposer should be proposing his own value here *)
        | None -> V.t_of_sexp (Sexplib.Sexp.Atom "chosen placeholder value")
      in
      match buses_for_topic node topic with
      | [] -> failwith "Impossible case, should always have at least one bus"
      | bus :: _ ->
        let log_msg = Printf.sprintf "Node %i realises that quorum has been reached, will suggest the chosen value=(%s) with proposal=(%s)" node.id  (V.to_string(chosen_value)) (Sexp.to_string_hum(Types.sexp_of_proposal_id proposal)) in
        Logger.log_decision node.logger log_msg;
        suggest ~msg_id ~time ~bus node ~proposal ~value:chosen_value;
        let new_state =
        State.ProposerAccepting { proposal; value = chosen_value; acks = []; nacks_received = [] } in
        let log_msg = Printf.sprintf "node %d set it's state to %s" node.id (Sexp.to_string(State.sexp_of_proposer_state new_state)) in
        Logger.log_decision node.logger log_msg;
        new_state
        |> State.set_role node.state State.Proposer
        |> fun st -> node.state <- st
    in
    match node.state.proposer with
    | State.Idle ->
      let log_msg = Printf.sprintf "node %d's proposer state was still idle. this is the first permission granted it has received so it will start to accumulate more in its WaitingForPromises state until a quorum is achieved!" node.id in
      Logger.log_decision node.logger log_msg;
      node.state <-
        { proposal; promises_received = [(from, last_accepted)]; nacks_received = [] }
        |> State.WaitingForPromises
        |> State.set_role node.state State.Proposer
    | State.WaitingForPromises wfp ->
      let log_msg = Printf.sprintf "node %d's proposer state has been waiting for promises. it will accumulate this then check if a quorum is achieved!" node.id in
      Logger.log_decision node.logger log_msg;
      node.state <-
        { wfp with promises_received = (from, last_accepted) :: wfp.promises_received }
        |> State.WaitingForPromises
        |> State.set_role node.state State.Proposer;
      begin match State.is_quorum_reached node.state (get_cluster_size node) with
        | State.MajorityGrants best_value_opt -> handle_on_quorum_reached best_value_opt
        | State.MajorityNacks _ -> Logger.log_decision node.logger "We reached a quorum and got majority nacks... time to abort" ; abort node
        | State.NotReached -> ()
      end
    | _ -> failwith "Met an impossible case when handling permission granted."

  let is_acceptable proposal current_promised_opt = Option.is_some current_promised_opt && Types.compare_proposal_id proposal (Option.value_exn current_promised_opt) >= 0
  let handle_suggestion node ({meta={topic; id; timestamp}; from; proposal; value}: V.t Message.suggestion_msg) =
    let msg = Printf.sprintf "... node %d received suggestion from %d with proposal=%s for value=(%s)"  node.id from (Sexp.to_string_hum(Types.sexp_of_proposal_id proposal)) (V.to_string value)in
    Logger.log_subroutine_flow node.logger Stdlib.__FUNCTION__ ~msg ();
    let msg_id = 1 + id in
    let time = 1 + timestamp in
    let (current_promised_opt, prev_accepted_opt) =
      match State.get_role node.state State.Acceptor with
      | State.Inactive | State.Idle -> (None, None)
      | State.Accepting record -> (record.promised, record.accepted)
    in
    let reply_msg =
      if is_acceptable proposal current_promised_opt then
        let updated_record = { Acceptor_record.promised = Some proposal; accepted = Some (proposal, value) } in
        node.state <- State.set_role node.state State.Acceptor (State.Accepting updated_record);
        Message.Coordination(Message.make_accepted ~msg_id ~topic ~time ~from:node.id ~proposal ~value)
      else
        Message.Coordination(Message.make_nack ~msg_id ~topic ~time ~from:node.id ~proposal ~hint:prev_accepted_opt)
    in
    buses_for_topic node topic
    |> function
    | [] -> failwith "Impossible case, should always have at least one bus"
    | bus :: _ -> Bus.enqueue bus ((topic, Some from), reply_msg)


  let handle_accepted node ({meta={topic;id;timestamp}; from; proposal; value} : V.t Message.accepted_msg) =
    (* update proposer state or infer consensus *)
    let msg = Printf.sprintf "... node %d received accepted from %d for proposal=%s for value=(%s)"  node.id from (Sexp.to_string_hum(Types.sexp_of_proposal_id proposal)) (V.to_string value)in
    Logger.log_subroutine_flow node.logger Stdlib.__FUNCTION__ ~msg ();
    match node.state.proposer with
    | State.ProposerAccepting a ->
      let new_acks =
        if List.mem a.acks from ~equal:(=) then a.acks else from :: a.acks
      in
      node.state <- {a with acks = new_acks}
        |> State.ProposerAccepting
        |> State.set_role node.state State.Proposer;
      (match State.is_quorum_reached node.state (get_cluster_size node) with
       | State.MajorityGrants (Some (_, decided_value)) -> let decided_state = State.Decided decided_value in
         node.state <- State.set_role node.state State.Proposer decided_state;
         (* TODO: determine if we should be broadcasting that the state is decided?? *)
         (* TODO: handle NACK optimisation later*)
       | _ -> ())
    | _ -> failwith "Met an impossible case when handling accepted."

  let handle_nack node ( { meta={topic;id;timestamp}
                          ; proposal
                          ; from
                          ; hint}: V.t Message.nack_msg)
    =
    let hint_str = "TODO HINT STRING" in
    let msg = Printf.sprintf "... node %d received NACK from %d for proposal=%s for hint=(%s)"  node.id from (Sexp.to_string_hum(Types.sexp_of_proposal_id proposal)) hint_str in
    Logger.log_subroutine_flow node.logger Stdlib.__FUNCTION__ ~msg ();
    (* we update the node state first, accumulating the nack value: *)
    let cluster_size = get_cluster_size node in
    let nack = ((id, hint, Some proposal):State.nack)  in
    let update_proposer_state updated_state =
      node.state <- State.set_role node.state State.Proposer updated_state
    in
    (* Handle majority quorum grants by suggesting the best value*)
    let handle_majority_grants best_value_opt =
      let chosen_value =
        Option.value_map best_value_opt
          ~default:(V.t_of_sexp (Sexplib.Sexp.Atom "chosen placeholder value"))
          ~f:snd
      in
      let msg_id = 1 + id in
      let time = 1 + timestamp in
      match buses_for_topic node topic with
      | [] -> failwith "Impossible case, should always have at least one bus"
      | bus :: _ ->
        suggest ~msg_id ~time ~bus node ~proposal ~value:chosen_value;
        node.state <- {
          proposal;
          value = chosen_value;
          acks = [];
          nacks_received = [];
        } |> State.ProposerAccepting |> State.set_role node.state State.Proposer
    in
    (* main dispatching: *)
    match node.state.proposer with
    | State.WaitingForPromises wfp ->
      let updated_state = {wfp with nacks_received=( nack :: wfp.nacks_received )} |> State.WaitingForPromises in
      update_proposer_state updated_state;
      (match State.is_quorum_reached node.state cluster_size with
       | State.MajorityGrants best -> best |> handle_majority_grants
       | State.MajorityNacks _ -> abort node
       | State.NotReached -> ())
    | State.ProposerAccepting pa ->
      let updated_state = {pa with nacks_received=( nack :: pa.nacks_received )} |> State.ProposerAccepting  in
      update_proposer_state updated_state ;
      (match State.is_quorum_reached node.state cluster_size with
       | State.MajorityGrants best -> best |> handle_majority_grants
       | State.MajorityNacks _ -> abort node
       | State.NotReached -> ())
    | _ ->
      failwith "Met an impossible state when handling nacks -- only valid in WaitingForPromises or ProposerAccepting."

  let handle_coordination node msg =
    Logger.log_subroutine_flow node.logger Stdlib.__FUNCTION__ ~msg:"...coordination is happening" ();
    match Message.proposal_id_of msg with
    | None -> ()
    | Some proposal_id ->
        match msg with
        | Message.Coordination (PermissionRequest pr) -> handle_permission_request node pr
        | Message.Coordination (PermissionGranted pg) -> handle_permission_granted node pg
        | Message.Coordination (Suggestion s) -> handle_suggestion node s
        | Message.Coordination (Accepted a) -> handle_accepted node a
        | Message.Coordination (Nack n) -> handle_nack node n
        | _ -> ()

  let handle_simulation_control (node: t) (msg: V.t Message.t) =
    let log_msg = Printf.sprintf "node %d received a control command from the simulation. msg=(%s)" node.id (Sexp.to_string(Message.sexp_of_t V.sexp_of_t msg))  in
    Logger.log_subroutine_flow node.logger Stdlib.__FUNCTION__ ~msg:log_msg ();

    match msg with
    | Message.Control (MakeNodeIdle {node_id; _}) when node_id = node.id ->
      node.state <- State.idle_of ();
      Stdio.printf "---> Node %d was made idle\n%!" node.id

    | Message.Control (MakeNodeInactive {node_id; _}) when node_id = node.id ->
      node.state <- State.inactive_of ();
      Stdio.printf "---> Node %d was made inactive\n%!" node.id


    | _ -> Stdio.printf "---> Node %d received simulation control but did nothing \n%!" node.id

  (* TODO: [extension-v1] wire this up to internal clock support *)
  let handle_time (node: t) (msg: V.t Message.t) =
    match msg with
    | Message.Time (Heartbeat {time; _}) ->
      let msg = Printf.sprintf "node %d felt simulation heartbeat for time=(%d)" node.id time in
    Logger.log_subroutine_flow node.logger Stdlib.__FUNCTION__ ~msg ();
    | _ -> ()

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
    let msg = Printf.sprintf "by node %d for topic=(%s)" node.id (Sexp.to_string_hum (Types.sexp_of_topic topic)) in
    Logger.log_subroutine_flow node.logger Stdlib.__FUNCTION__ ~msg ();
    match topic with
    | Types.Coordination -> handle_coordination node
    | Types.Simulation_control -> handle_simulation_control node
    | Types.Time -> handle_time node
    | _ -> failwith "Unsupported topic for message passing"

  let create ?(state=(State.idle_of ())) ~id ~config ~bus () =
    let topics = config.topics in
    let node = {
      id;
      state;
      config;
      subs= Hashtbl.Poly.create ();
      inbox= Hashtbl.Poly.create ();
      transitions = ref [];
      logger=Logger.create ();
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
