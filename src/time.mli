type time_zone =
  [ `Local
  | `UTC
  | `UTC_plus of int
  ]

type weekday =
  [ `Sun
  | `Mon
  | `Tue
  | `Wed
  | `Thu
  | `Fri
  | `Sat
  ]

type month =
  [ `Jan
  | `Feb
  | `Mar
  | `Apr
  | `May
  | `Jun
  | `Jul
  | `Aug
  | `Sep
  | `Oct
  | `Nov
  | `Dec
  ]

type weekday_range = weekday Range.range

type month_day_range = int Range.range

type day_range =
  | Weekday_range of weekday_range
  | Month_day_range of month_day_range

val first_mday : int

val tm_year_offset : int

val minute_to_second_multiplier : int64

val hour_to_second_multiplier : int64

val day_to_second_multiplier : int64

module Check : sig
  val check_unix_time : int64 -> bool

  val check_second : second:int -> bool

  val check_minute_second : minute:int -> second:int -> bool

  val check_hour_minute_second : hour:int -> minute:int -> second:int -> bool
end

val next_hour_minute : hour:int -> minute:int -> (int * int, unit) result

val next_weekday : weekday -> weekday

val tm_int_of_weekday : weekday -> int

val weekday_of_tm_int : int -> weekday

val cal_weekday_of_weekday : weekday -> CalendarLib.Calendar.day

val weekday_of_cal_weekday : CalendarLib.Calendar.day -> weekday

val tm_int_of_month : month -> int

val month_of_tm_int : int -> (month, unit) result

val human_int_of_month : month -> int

val month_of_human_int : int -> (month, unit) result

val cal_month_of_month : month -> CalendarLib.Calendar.month

val month_of_cal_month : CalendarLib.Calendar.month -> month

val month_compare : month -> month -> int

val month_lt : month -> month -> bool

val month_le : month -> month -> bool

val month_gt : month -> month -> bool

val month_ge : month -> month -> bool

val weekday_compare : weekday -> weekday -> int

val weekday_lt : weekday -> weekday -> bool

val weekday_le : weekday -> weekday -> bool

val weekday_gt : weekday -> weekday -> bool

val weekday_ge : weekday -> weekday -> bool

val zero_tm_sec : Unix.tm -> Unix.tm

val cal_time_zone_of_time_zone : time_zone -> CalendarLib.Time_Zone.t

val time_zone_of_cal_time_zone : CalendarLib.Time_Zone.t -> time_zone

val tm_of_unix_time : time_zone_of_tm:time_zone -> int64 -> Unix.tm

val unix_time_of_tm : time_zone_of_tm:time_zone -> Unix.tm -> int64

val normalize_tm : Unix.tm -> Unix.tm

val tm_change_time_zone :
  from_time_zone:time_zone -> to_time_zone:time_zone -> Unix.tm -> Unix.tm

val is_leap_year : year:int -> bool

val day_count_of_year : year:int -> int

val day_count_of_month : year:int -> month:month -> int

val weekday_of_month_day : year:int -> month:month -> mday:int -> weekday

val local_tm_to_utc_tm : Unix.tm -> Unix.tm

module Second_ranges : Ranges.S with type t := int

module Minute_ranges : Ranges.S with type t := int

module Hour_ranges : Ranges.S with type t := int

module Weekday_tm_int_ranges : Ranges.S with type t := int

module Weekday_ranges : Ranges.S with type t := weekday

module Month_day_ranges : Ranges.S with type t := int

module Month_tm_int_ranges : Ranges.S with type t := int

module Month_ranges : Ranges.S with type t := month

module Year_ranges : Ranges.S with type t := int

module Current : sig
  val cur_unix_time : unit -> int64

  val cur_tm_local : unit -> Unix.tm

  val cur_tm_utc : unit -> Unix.tm
end

module Interpret_string : sig
  val weekday_of_string : string -> (weekday, unit) result

  val month_of_string : string -> (month, unit) result
end

module Add : sig
  val add_days_unix_time : days:int -> int64 -> int64
end

module Serialize : sig
  val pack_weekday : weekday -> Time_t.weekday

  val pack_month : month -> Time_t.month
end

module Deserialize : sig
  val unpack_weekday : Time_t.weekday -> weekday
end

module To_string : sig
  val string_of_weekday : weekday -> string

  val string_of_month : month -> string

  val yyyymondd_hhmmss_string_of_tm : Unix.tm -> (string, unit) result

  val yyyymondd_hhmmss_string_of_unix_time :
    display_in_time_zone:time_zone -> int64 -> string

  val yyyymmdd_hhmmss_string_of_tm : Unix.tm -> (string, unit) result

  val yyyymmdd_hhmmss_string_of_unix_time :
    display_in_time_zone:time_zone -> int64 -> string

  val yyyymondd_hhmm_string_of_tm : Unix.tm -> (string, unit) result

  val yyyymondd_hhmm_string_of_unix_time :
    display_in_time_zone:time_zone -> int64 -> string

  val yyyymmdd_hhmm_string_of_tm : Unix.tm -> (string, unit) result

  val yyyymmdd_hhmm_string_of_unix_time :
    display_in_time_zone:time_zone -> int64 -> string
end

module Print : sig
  val debug_print_time :
    ?indent_level:int -> display_in_time_zone:time_zone -> int64 -> unit
end