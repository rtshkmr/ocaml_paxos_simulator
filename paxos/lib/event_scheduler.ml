open Base
open Time
open Sim_event
open Counter

module type S = sig
  type t

  val create : unit -> t

  val add_event : t -> Sim_event.t -> unit

  val pop_due_events : t -> Time.t -> Sim_event.t list

  val peek_next_event_time : t -> Time.t option
end

(**
  A deterministic event scheduler holding `Sim_event.t` values, ordered by logical time.
*)
module Event_scheduler : S = struct
  type e = {insert_id: int; event: Sim_event.t}

  type t = {events: e list ref; insert_counter: Counter.t}

  let create () = {events= ref []; insert_counter= Counter.create 0}

  let add_event t event =
    let insert_id = Counter.next t.insert_counter in
    let entry = {insert_id; event} in
    t.events := entry :: !(t.events)

  let pop_due_events t now =
    let is_due ({event= {time; _}; _} : e) = Time.compare time now <= 0 in
    let due, future = List.partition_tf !(t.events) ~f:is_due in
    t.events := future ;
    due
    |> List.sort ~compare:(fun a b ->
           let c = Time.compare a.event.time b.event.time in
           if c <> 0 then c else Int.compare a.insert_id b.insert_id )
    |> List.map ~f:(fun e -> e.event)

  let peek_next_event_time t =
    List.fold_left !(t.events) ~init:None
      ~f:(fun acc ({insert_id; event= {time; _}} : e) ->
        match acc with
        | None ->
            Some (time, insert_id)
        | Some (curr_time, curr_insert_id) ->
            let cmp_time = Time.compare time curr_time in
            if cmp_time < 0 then Some (time, insert_id)
            else if cmp_time = 0 && insert_id < curr_insert_id then
              Some (time, insert_id)
            else acc )
    |> Option.map ~f:fst
end
