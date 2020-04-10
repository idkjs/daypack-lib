type search_type =
  | Time_slots of Time_slot_ds.t list
  | Years_ahead_start_unix_time of {
      start : int64;
      search_years_ahead : int;
    }
  | Years_ahead_start_tm of {
      time_zone_of_tm : Time.time_zone;
      start : Unix.tm;
      search_years_ahead : int;
    }

type days =
  [ `Weekdays of Time.weekday list
  | `Month_days of int list
  ]

type t = {
  years : int list;
  months : Time.month list;
  days : days;
  hours : int list;
  minutes : int list;
  seconds : int list;
}

type single_or_pairs =
  | Single_time_pattern of t
  | Paired_time_patterns of (t * t) list

val empty : t

val matching_tm_seq :
  search_in_time_zone:Time.time_zone -> search_type -> t -> Unix.tm Seq.t

val matching_time_slots :
  search_in_time_zone:Time.time_zone -> search_type -> t -> Time_slot_ds.t Seq.t

val next_match_tm :
  search_in_time_zone:Time.time_zone -> search_type -> t -> Unix.tm option

val next_match_unix_time :
  search_in_time_zone:Time.time_zone -> search_type -> t -> int64 option

val next_match_time_slot :
  search_in_time_zone:Time.time_zone ->
  search_type ->
  t ->
  (int64 * int64) option

val matching_time_slots_time_pattern_pair :
  search_in_time_zone:Time.time_zone ->
  search_type ->
  t * t ->
  Time_slot_ds.t Seq.t

val next_match_time_slot_time_pattern_pair :
  search_in_time_zone:Time.time_zone ->
  search_type ->
  t * t ->
  (int64 * int64) option

val matching_time_slots_time_pattern_pairs :
  search_in_time_zone:Time.time_zone ->
  search_type ->
  (t * t) list ->
  Time_slot_ds.t Seq.t

val next_match_time_slot_time_pattern_pairs :
  search_in_time_zone:Time.time_zone ->
  search_type ->
  (t * t) list ->
  (int64 * int64) option

val matching_time_slots_single_or_pairs :
  search_in_time_zone:Time.time_zone ->
  search_type ->
  single_or_pairs ->
  Time_slot_ds.t Seq.t

val next_match_time_slot_single_or_pairs :
  search_in_time_zone:Time.time_zone ->
  search_type ->
  single_or_pairs ->
  Time_slot_ds.t option

module Equal : sig
  val equal : t -> t -> bool
end

module Print : sig
  val debug_string_of_days : days -> string

  val debug_string_of_pattern :
    ?indent_level:int -> ?buffer:Buffer.t -> t -> string

  val debug_print_pattern : ?indent_level:int -> t -> unit
end

module Serialize : sig
  val pack_days : days -> Time_pattern_t.days

  val pack_pattern : t -> Time_pattern_t.t
end

module Deserialize : sig
  val unpack_days : Time_pattern_t.days -> days

  val unpack_pattern : Time_pattern_t.t -> t
end
