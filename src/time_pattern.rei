type time_pattern = {
  years: list(int),
  months: list(Time.month),
  month_days: list(int),
  weekdays: list(Time.weekday),
  hours: list(int),
  minutes: list(int),
  seconds: list(int),
  unix_seconds: list(int64),
};

type time_pattern_error =
  | Invalid_years(list(int))
  | Invalid_month_days(list(int))
  | Invalid_hours(list(int))
  | Invalid_minutes(list(int))
  | Invalid_seconds(list(int))
  | Invalid_unix_seconds(list(int64));

type error =
  | Invalid_search_param(Search_param.error)
  | Invalid_time_pattern(time_pattern_error);

type time_range_pattern = Range.range(time_pattern);

/* type single_or_ranges =
 *   | Single_time_pattern of time_pattern
 *   | Time_range_patterns of time_range_pattern list */

let empty: time_pattern;

module Check: {
  let check_time_pattern: time_pattern => result(unit, time_pattern_error);

  let check_time_range_pattern:
    time_range_pattern => result(unit, time_pattern_error);
};

module Single_pattern: {
  let matching_date_times:
    (~allow_search_param_override: bool, Search_param.t, time_pattern) =>
    result(Seq.t(Time.Date_time.t), error);

  let matching_unix_seconds:
    (~allow_search_param_override: bool, Search_param.t, time_pattern) =>
    result(Seq.t(int64), error);

  let matching_date_time_ranges:
    (~allow_search_param_override: bool, Search_param.t, time_pattern) =>
    result(Seq.t(Range.range(Time.Date_time.t)), error);

  let matching_time_slots:
    (~allow_search_param_override: bool, Search_param.t, time_pattern) =>
    result(Seq.t(Time_slot.t), error);

  let matching_time_slots_round_robin_non_decreasing:
    (
      ~allow_search_param_override: bool,
      Search_param.t,
      list(time_pattern)
    ) =>
    result(Seq.t(list(Time_slot.t)), error);

  let matching_time_slots_round_robin_non_decreasing_flat:
    (
      ~allow_search_param_override: bool,
      Search_param.t,
      list(time_pattern)
    ) =>
    result(Seq.t(Time_slot.t), error);

  let next_match_date_time:
    (~allow_search_param_override: bool, Search_param.t, time_pattern) =>
    result(option(Time.Date_time.t), error);

  let next_match_unix_second:
    (~allow_search_param_override: bool, Search_param.t, time_pattern) =>
    result(option(int64), error);

  let next_match_time_slot:
    (~allow_search_param_override: bool, Search_param.t, time_pattern) =>
    result(option(Time_slot.t), error);
};

module Range_pattern: {
  let matching_time_slots:
    (~allow_search_param_override: bool, Search_param.t, time_range_pattern) =>
    result(Seq.t(Time_slot.t), error);

  let next_match_time_slot:
    (~allow_search_param_override: bool, Search_param.t, time_range_pattern) =>
    result(option(Time_slot.t), error);

  let matching_time_slots_multi:
    (
      ~allow_search_param_override: bool,
      Search_param.t,
      list(time_range_pattern)
    ) =>
    result(Seq.t(Time_slot.t), error);

  let next_match_time_slot_multi:
    (
      ~allow_search_param_override: bool,
      Search_param.t,
      list(time_range_pattern)
    ) =>
    result(option((int64, int64)), error);

  let matching_time_slots_round_robin_non_decreasing:
    (
      ~allow_search_param_override: bool,
      Search_param.t,
      list(time_range_pattern)
    ) =>
    result(Seq.t(list(Time_slot.t)), error);

  let matching_time_slots_round_robin_non_decreasing_flat:
    (
      ~allow_search_param_override: bool,
      Search_param.t,
      list(time_range_pattern)
    ) =>
    result(Seq.t(Time_slot.t), error);
};

/* module Single_or_ranges : sig
 *   val matching_time_slots :
 *     Search_param.t -> single_or_ranges -> (Time_slot.t Seq.t, error) result
 *
 *   val next_match_time_slot :
 *     Search_param.t -> single_or_ranges -> (Time_slot.t option, error) result
 *
 *   val matching_time_slots_round_robin_non_decreasing :
 *     Search_param.t -> single_or_ranges -> (Time_slot.t list Seq.t, error) result
 *
 *   val matching_time_slots_round_robin_non_decreasing_flat :
 *     Search_param.t -> single_or_ranges -> (Time_slot.t Seq.t, error) result
 * end */

module Equal: {let equal: (time_pattern, time_pattern) => bool;};

module Parsers: {
  let cron_expr: MParser.t(time_pattern, unit);

  let time_pattern_core_expr: MParser.t(time_pattern, unit);

  let time_pattern_expr: MParser.t(time_pattern, unit);
};

module Of_string: {
  let time_pattern_of_cron_string: string => result(time_pattern, string);

  let time_pattern_of_string: string => result(time_pattern, string);
};

module To_string: {
  let string_of_error: error => string;

  let debug_string_of_weekdays: list(Time.weekday) => string;

  let debug_string_of_month_days: list(int) => string;

  let debug_string_of_time_pattern:
    (~indent_level: int=?, ~buffer: Buffer.t=?, time_pattern) => string;

  let debug_string_of_time_range_pattern:
    (~indent_level: int=?, ~buffer: Buffer.t=?, time_range_pattern) => string;
  /* val debug_string_of_single_or_ranges :
   *   ?indent_level:int -> ?buffer:Buffer.t -> single_or_ranges -> string */
};

module Print: {
  let debug_print_time_pattern: (~indent_level: int=?, time_pattern) => unit;

  let debug_print_time_range_pattern:
    (~indent_level: int=?, time_range_pattern) => unit;
  /* val debug_print_single_or_ranges :
   *   ?indent_level:int -> single_or_ranges -> unit */
};

module Serialize: {
  let pack_pattern: time_pattern => Time_pattern_t.time_pattern;
};

module Deserialize: {
  let unpack_pattern: Time_pattern_t.time_pattern => time_pattern;
};
