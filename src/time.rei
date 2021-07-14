type tz_offset_s = int;

let tz_offset_s_utc: tz_offset_s;

type weekday = [ | `Sun | `Mon | `Tue | `Wed | `Thu | `Fri | `Sat];

type month = [
  | `Jan
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
];

type weekday_range = Range.range(weekday);

type month_day_range = Range.range(int);

type day_range =
  | Weekday_range(weekday_range)
  | Month_day_range(month_day_range);

let first_mday: int;

let tm_year_offset: int;

module Int64_multipliers: {
  let minute_to_seconds: int64;

  let hour_to_seconds: int64;

  let day_to_seconds: int64;
};

module Float_multipliers: {
  let minute_to_seconds: float;

  let hour_to_seconds: float;

  let day_to_seconds: float;
};

module Date_time: {
  type t = {
    year: int,
    month,
    day: int,
    hour: int,
    minute: int,
    second: int,
    tz_offset_s: int,
  };

  let of_ptime_date_time: ((Ptime.date, Ptime.time)) => result(t, unit);

  let to_ptime_date_time: t => (Ptime.date, Ptime.time);

  let to_unix_second: t => result(int64, unit);

  let of_unix_second:
    (~tz_offset_s_of_date_time: option(tz_offset_s), int64) =>
    result(t, unit);

  let min: t;

  let max: t;

  let compare: (t, t) => int;

  let set_to_first_sec: t => t;

  let set_to_last_sec: t => t;

  let set_to_first_min_sec: t => t;

  let set_to_last_min_sec: t => t;

  let set_to_first_hour_min_sec: t => t;

  let set_to_last_hour_min_sec: t => t;

  let set_to_first_day_hour_min_sec: t => t;

  let set_to_last_day_hour_min_sec: t => t;

  let set_to_first_month_day_hour_min_sec: t => t;

  let set_to_last_month_day_hour_min_sec: t => t;
};

module Check: {
  let unix_second_is_valid: int64 => bool;

  let second_is_valid: (~second: int) => bool;

  let minute_second_is_valid: (~minute: int, ~second: int) => bool;

  let hour_minute_second_is_valid:
    (~hour: int, ~minute: int, ~second: int) => bool;

  let date_time_is_valid: Date_time.t => bool;
};

let next_hour_minute: (~hour: int, ~minute: int) => result((int, int), unit);

let next_weekday: weekday => weekday;

let tm_int_of_weekday: weekday => int;

let weekday_of_tm_int: int => result(weekday, unit);

let tm_int_of_month: month => int;

let month_of_tm_int: int => result(month, unit);

let human_int_of_month: month => int;

let month_of_human_int: int => result(month, unit);

let compare_month: (month, month) => int;

let month_lt: (month, month) => bool;

let month_le: (month, month) => bool;

let month_gt: (month, month) => bool;

let month_ge: (month, month) => bool;

let compare_weekday: (weekday, weekday) => int;

let weekday_lt: (weekday, weekday) => bool;

let weekday_le: (weekday, weekday) => bool;

let weekday_gt: (weekday, weekday) => bool;

let weekday_ge: (weekday, weekday) => bool;

let zero_tm_sec: Unix.tm => Unix.tm;

/* val tm_of_unix_second : time_zone_of_tm:time_zone -> int64 -> Unix.tm

   val unix_second_of_tm : time_zone_of_tm:time_zone -> Unix.tm -> int64

   val normalize_tm : Unix.tm -> Unix.tm

   val tm_change_time_zone :
   from_time_zone:time_zone -> to_time_zone:time_zone -> Unix.tm -> Unix.tm */

let is_leap_year: (~year: int) => bool;

let day_count_of_year: (~year: int) => int;

let day_count_of_month: (~year: int, ~month: month) => int;

let weekday_of_month_day:
  (~year: int, ~month: month, ~mday: int) => result(weekday, unit);

/* val local_tm_to_utc_tm : Unix.tm -> Unix.tm */

module Second_ranges: Ranges.S with type t := int;

module Minute_ranges: Ranges.S with type t := int;

module Hour_ranges: Ranges.S with type t := int;

module Weekday_tm_int_ranges: Ranges.S with type t := int;

module Weekday_ranges: Ranges.S with type t := weekday;

module Month_day_ranges: Ranges.S with type t := int;

module Month_tm_int_ranges: Ranges.S with type t := int;

module Month_ranges: Ranges.S with type t := month;

module Year_ranges: Ranges.S with type t := int;

module Current: {
  let cur_unix_second: unit => int64;

  let cur_date_time:
    (~tz_offset_s_of_date_time: option(tz_offset_s)) =>
    result(Date_time.t, unit);

  let cur_tm_local: unit => Unix.tm;

  let cur_tm_utc: unit => Unix.tm;
};

module Of_string: {
  let weekday_of_string: string => result(weekday, unit);

  let month_of_string: string => result(month, unit);
};

module Add: {let add_days_unix_second: (~days: int, int64) => int64;};

module Serialize: {
  let pack_weekday: weekday => Time_t.weekday;

  let pack_month: month => Time_t.month;
};

module Deserialize: {let unpack_weekday: Time_t.weekday => weekday;};

module To_string: {
  module Format_string_parsers: {
    let inner: Date_time.t => MParser.t(string, unit);
  };

  let abbreviated_string_of_weekday: weekday => string;

  let full_string_of_weekday: weekday => string;

  let abbreviated_string_of_month: month => string;

  let full_string_of_month: month => string;

  /* val yyyymondd_hhmmss_string_of_tm : Unix.tm -> (string, unit) result */
  let yyyymondd_hhmmss_string_of_date_time: Date_time.t => string;

  let yyyymondd_hhmmss_string_of_unix_second:
    (~display_using_tz_offset_s: option(tz_offset_s), int64) =>
    result(string, unit);

  /* val yyyymmdd_hhmmss_string_of_tm : Unix.tm -> (string, unit) result */
  let yyyymmdd_hhmmss_string_of_date_time: Date_time.t => string;

  let yyyymmdd_hhmmss_string_of_unix_second:
    (~display_using_tz_offset_s: option(tz_offset_s), int64) =>
    result(string, unit);

  /* val yyyymondd_hhmm_string_of_tm : Unix.tm -> (string, unit) result */
  let yyyymondd_hhmm_string_of_date_time: Date_time.t => string;

  let yyyymondd_hhmm_string_of_unix_second:
    (~display_using_tz_offset_s: option(tz_offset_s), int64) =>
    result(string, unit);

  /* val yyyymmdd_hhmm_string_of_tm : Unix.tm -> (string, unit) result */
  let yyyymmdd_hhmm_string_of_date_time: Date_time.t => string;

  let yyyymmdd_hhmm_string_of_unix_second:
    (~display_using_tz_offset_s: option(tz_offset_s), int64) =>
    result(string, unit);

  let string_of_date_time:
    (~format: string, Date_time.t) => result(string, string);

  let string_of_unix_second:
    (
      ~format: string,
      ~display_using_tz_offset_s: option(tz_offset_s),
      int64
    ) =>
    result(string, string);

  let string_of_time_slot:
    (
      ~format: string,
      ~display_using_tz_offset_s: option(tz_offset_s),
      Time_slot.t
    ) =>
    result(string, string);
};

module Print: {
  let debug_print_time:
    (
      ~indent_level: int=?,
      ~display_using_tz_offset_s: option(tz_offset_s),
      int64
    ) =>
    unit;
};

module Date_time_set: Set.S with type elt = Date_time.t;
