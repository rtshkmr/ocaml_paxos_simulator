open Base
open Time
open Sim_event

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
  type t = Sim_event.t list ref

  let create () = ref []

  let add_event q event = q := event :: !q

  let pop_due_events q now =
    let is_due ({time; _} : Sim_event.t) = Time.compare time now <= 0 in
    let due, future = List.partition_tf !q ~f:is_due in
    q := future ;
    List.sort due ~compare:(fun a b -> Time.compare a.time b.time)

  let peek_next_event_time q =
    List.fold_left !q ~init:None ~f:(fun acc ({time; _} : Sim_event.t) ->
        match acc with None -> Some time | Some t -> Some (Int.min t time) )
end
