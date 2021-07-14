type start = [ | `Unix_second(int64) | `Date_time(Time.Date_time.t)];

type typ =
  | Time_slots(list(Time_slot.t))
  | Years_ahead({
      start,
      years_ahead: int,
    });

type t = {
  search_using_tz_offset_s: option(Time.tz_offset_s),
  typ,
};

type error =
  | Invalid_start
  | Invalid_time_slots
  | Invalid_search_years_ahead
  | Too_far_into_future;

let push_search_param_to_later_start: (~start: int64, t) => result(t, unit);

let start_date_time_and_search_years_ahead_of_search_param:
  t => option((Time.Date_time.t, int));

module Check: {let check_search_param: t => result(unit, error);};

let make_using_time_slots:
  (~search_using_tz_offset_s: int=?, list(Time_slot.t)) => result(t, error);

let make_using_years_ahead:
  (~search_using_tz_offset_s: int=?, ~start: start=?, int) => result(t, error);
