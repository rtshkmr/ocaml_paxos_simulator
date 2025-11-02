open Time
(**
  A simple deterministic event scheduler that orders events by logical time.
*)
module EventScheduler : sig
  type event = {time: Time.t; action: unit -> unit}

  type t

  val create : unit -> t
  (** Create an empty scheduler. *)

  val add_event : t -> event -> unit
  (** Add an event to the scheduler. *)

  val pop_due_events : t -> Time.t -> event list
  (** Return all events scheduled for execution at or before [now]. *)

  val peek_next_event_time : t -> Time.t option
  (** Return time of the next scheduled event, if any. *)
end = struct
  type event = {time: Time.t; action: unit -> unit}

  type t = event list ref

  let create () = ref []

  let add_event q ev = q := ev :: !q

  let pop_due_events q now =
    let due, future =
      List.partition (fun ev -> Time.compare ev.time now <= 0) !q
    in
    q := future ;
    List.sort (fun a b -> Time.compare a.time b.time) due

  let peek_next_event_time q =
    List.fold_left
      (fun acc ev ->
        match acc with None -> Some ev.time | Some t -> Some (min t ev.time) )
      None !q
end
