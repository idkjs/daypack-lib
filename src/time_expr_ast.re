type branch_unary_op =
  | Next_n_batches(int)
  | Every_batch;

type second_expr = int;

type minute_second_expr = {
  minute: int,
  second: int,
};

type hms_expr = {
  hour: int,
  minute: int,
  second: int,
};

type sign_expr =
  | Pos
  | Neg;

type second_range_expr = Range.range(second_expr);

type minute_second_range_expr = Range.range(minute_second_expr);

type hms_range_expr = Range.range(hms_expr);

type day_expr =
  | Weekday(Time.weekday)
  | Month_day(int);

type day_range_expr =
  | Weekday_range(Time.weekday_range)
  | Month_day_range(Time.month_day_range);

type month_expr = Time.month;

type year_expr = int;

type month_weekday_mode =
  | First_n(int)
  | Last_n(int);

type time_point_expr =
  | Tpe_name(string)
  | Tpe_unix_seconds(list(int64))
  | Second(second_expr)
  | Minute_second(minute_second_expr)
  | Hms(hms_expr)
  | Day_hms({
      day: day_expr,
      hms: hms_expr,
    })
  | Month_day_hms({
      month: month_expr,
      month_day: int,
      hms: hms_expr,
    })
  | Year_month_day_hms({
      year: year_expr,
      month: month_expr,
      month_day: int,
      hms: hms_expr,
    });

/* type branching_time_point_expr =
 *   | Btp_unary_op of branch_unary_op * branching_time_point_expr
 *   | Btp_month_days_and_hmss of {
 *       month_days : int Range.range list;
 *       hmss : hms_expr list;
 *     }
 *   | Btp_weekdays_and_hmss of {
 *       weekdays : Time.weekday Range.range list;
 *       hmss : hms_expr list;
 *     }
 *   | Btp_months_and_month_days_and_hmss of {
 *       months : month_expr Range.range list;
 *       month_days : int Range.range list;
 *       hmss : hms_expr list;
 *     }
 *   | Btp_months_and_weekdays_and_hmss of {
 *       months : month_expr Range.range list;
 *       weekdays : Time.weekday Range.range list;
 *       hmss : hms_expr list;
 *     }
 *   | Btp_months_and_weekday_and_hmss of {
 *       months : month_expr Range.range list;
 *       weekday : Time.weekday;
 *       hmss : hms_expr list;
 *       month_weekday_mode : month_weekday_mode option;
 *     }
 *   | Btp_years_and_months_and_month_days_and_hmss of {
 *       years : int Range.range list;
 *       months : month_expr Range.range list;
 *       month_days : int Range.range list;
 *       hmss : hms_expr list;
 *     } */

type time_slot_expr =
  | Tse_name(string)
  | Explicit_time_slot((time_point_expr, time_point_expr));

type branching_time_slot_expr =
  | Bts_unary_op(branch_unary_op, branching_time_slot_expr)
  | Bts_hms_ranges(list(hms_range_expr))
  | Bts_month_days_and_hms_ranges({
      month_days: list(Range.range(int)),
      hms_ranges: list(hms_range_expr),
    })
  | Bts_weekdays_and_hms_ranges({
      weekdays: list(Range.range(Time.weekday)),
      hms_ranges: list(hms_range_expr),
    })
  | Bts_months_and_month_days_and_hms_ranges({
      months: list(Range.range(month_expr)),
      month_days: list(Range.range(int)),
      hms_ranges: list(hms_range_expr),
    })
  | Bts_months_and_weekdays_and_hms_ranges({
      months: list(Range.range(month_expr)),
      weekdays: list(Range.range(Time.weekday)),
      hms_ranges: list(hms_range_expr),
    })
  | Bts_months_and_weekday_and_hms_ranges({
      months: list(Range.range(month_expr)),
      weekday: Time.weekday,
      hms_ranges: list(hms_range_expr),
      month_weekday_mode: option(month_weekday_mode),
    })
  | Bts_years_and_months_and_month_days_and_hms_ranges({
      years: list(Range.range(int)),
      months: list(Range.range(month_expr)),
      month_days: list(Range.range(int)),
      hms_ranges: list(hms_range_expr),
    });

type unary_op =
  | Not
  | Every
  | Next_n_points(int)
  | Next_n_slots(int)
  | Tz_offset(sign_expr, hms_expr);

type binary_op =
  | Union
  | Inter;

type t =
  | Time_point_expr(time_point_expr)
  | Time_slot_expr(time_slot_expr)
  /* | Branching_time_point_expr of branching_time_point_expr */
  | Branching_time_slot_expr(branching_time_slot_expr)
  | Time_pattern(Time_pattern.time_pattern)
  | Time_unary_op(unary_op, t)
  | Time_binary_op(binary_op, t, t)
  | Time_round_robin_select(list(t));
