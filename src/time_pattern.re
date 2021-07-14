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

type single_or_ranges =
  | Single_time_pattern(time_pattern)
  | Time_range_patterns(list(time_range_pattern));

module Check = {
  let check_time_pattern =
      (x: time_pattern): result(unit, time_pattern_error) => {
    let invalid_years = List.filter(x => x < 0 || 9999 < x, x.years);
    let invalid_month_days = List.filter(x => x < 1 || 31 < x, x.month_days);

    let invalid_hours = List.filter(x => x < 0 || 23 < x, x.hours);
    let invalid_minutes = List.filter(x => x < 0 || 59 < x, x.minutes);
    let invalid_seconds = List.filter(x => x < 0 || 59 < x, x.seconds);
    let invalid_unix_seconds =
      List.filter(
        x =>
          Result.is_error(
            Time.Date_time.of_unix_second(~tz_offset_s_of_date_time=None, x),
          ),
        x.unix_seconds,
      );

    switch (invalid_years) {
    | [] =>
      switch (invalid_month_days) {
      | [] =>
        switch (invalid_hours) {
        | [] =>
          switch (invalid_minutes) {
          | [] =>
            switch (invalid_seconds) {
            | [] =>
              switch (invalid_unix_seconds) {
              | [] => Ok()
              | l => Error(Invalid_unix_seconds(l))
              }
            | l => Error(Invalid_seconds(l))
            }
          | l => Error(Invalid_minutes(l))
          }
        | l => Error(Invalid_hours(l))
        }
      | l => Error(Invalid_month_days(l))
      }
    | l => Error(Invalid_years(l))
    };
  };

  let check_time_range_pattern =
      (x: time_range_pattern): result(unit, time_pattern_error) =>
    switch (x) {
    | `Range_inc(x, y)
    | `Range_exc(x, y) =>
      switch (check_time_pattern(x)) {
      | Error(e) => Error(e)
      | Ok () =>
        switch (check_time_pattern(y)) {
        | Error(e) => Error(e)
        | Ok () => Ok()
        }
      }
    };

  let check_search_param_and_time_pattern =
      (search_param: Search_param.t, x: time_pattern): result(unit, error) =>
    switch (Search_param.Check.check_search_param(search_param)) {
    | Error(e) => Error(Invalid_search_param(e))
    | Ok () =>
      switch (check_time_pattern(x)) {
      | Error(e) => Error(Invalid_time_pattern(e))
      | Ok () => Ok()
      }
    };

  let check_search_param_and_time_range_pattern =
      (search_param: Search_param.t, x: time_range_pattern)
      : result(unit, error) =>
    switch (Search_param.Check.check_search_param(search_param)) {
    | Error(e) => Error(Invalid_search_param(e))
    | Ok () =>
      switch (check_time_range_pattern(x)) {
      | Error(e) => Error(Invalid_time_pattern(e))
      | Ok () => Ok()
      }
    };
};

let empty = {
  years: [],
  months: [],
  weekdays: [],
  month_days: [],
  hours: [],
  minutes: [],
  seconds: [],
  unix_seconds: [],
};

let of_unix_second =
    (~tz_offset_s_of_time_pattern: option(Time.tz_offset_s), x: int64)
    : result(time_pattern, unit) =>
  Time.Date_time.of_unix_second(
    ~tz_offset_s_of_date_time=tz_offset_s_of_time_pattern,
    x,
  )
  |> Result.map(x =>
       Time.Date_time.{
         years: [x.year],
         months: [x.month],
         weekdays: [],
         month_days: [x.day],
         hours: [x.hour],
         minutes: [x.minute],
         seconds: [x.second],
         unix_seconds: [],
       }
     );

/* let search_in_time_zone_of_search_param (param : search_param) : Time.time_zone
      =
      match param with
      | Time_slots { search_in_time_zone; _ } -> search_in_time_zone
      | Years_ahead_start_unix_second { search_in_time_zone; _ } ->
       search_in_time_zone
      | Years_ahead_start_dt { search_in_time_zone; _ } -> search_in_time_zone
   */

module Matching_seconds = {
  let get_cur_branch_search_start =
      (~overall_search_start: Time.Date_time.t, cur_branch: Time.Date_time.t)
      : Time.Date_time.t =>
    if (cur_branch.year == overall_search_start.year
        && cur_branch.month == overall_search_start.month
        && cur_branch.day == overall_search_start.day
        && cur_branch.hour == overall_search_start.hour
        && cur_branch.minute == overall_search_start.minute) {
      overall_search_start;
    } else {
      Time.Date_time.set_to_first_sec(cur_branch);
    };

  let matching_seconds =
      (
        t: time_pattern,
        ~overall_search_start: Time.Date_time.t,
        cur_branch: Time.Date_time.t,
      )
      : Seq.t(Time.Date_time.t) => {
    let cur_branch_search_start =
      get_cur_branch_search_start(~overall_search_start, cur_branch);

    switch (t.seconds) {
    | [] =>
      Seq.map(
        second => {...cur_branch, second},
        OSeq.(cur_branch_search_start.second --^ 60),
      )
    | pat_sec_list =>
      pat_sec_list
      |> List.to_seq
      |> Seq.filter(second =>
           cur_branch_search_start.second <= second && second < 60
         )
      |> Seq.map(second => {...cur_branch, second})
    };
  };

  let matching_second_ranges =
      (
        t: time_pattern,
        ~overall_search_start: Time.Date_time.t,
        cur_branch: Time.Date_time.t,
      )
      : Seq.t(Range.range(Time.Date_time.t)) => {
    let range_map_start = (~cur_branch_search_start: Time.Date_time.t, x) =>
      if (x == cur_branch_search_start.second) {
        cur_branch_search_start;
      } else {
        {...cur_branch_search_start, second: x};
      };

    let range_map_inc = (~cur_branch_search_start: Time.Date_time.t, (x, y)) => (
      range_map_start(~cur_branch_search_start, x),
      {...cur_branch_search_start, second: y},
    );

    let range_map_exc = (~cur_branch_search_start: Time.Date_time.t, (x, y)) => (
      range_map_start(~cur_branch_search_start, x),
      {...cur_branch_search_start, second: y},
    );

    let cur_branch_search_start =
      get_cur_branch_search_start(~overall_search_start, cur_branch);

    switch (t.seconds) {
    | [] =>
      Seq.return(
        `Range_inc((
          cur_branch_search_start,
          Time.Date_time.set_to_last_sec(cur_branch),
        )),
      )
    | l =>
      List.sort_uniq(compare, l)
      |> Time.Second_ranges.Of_list.range_seq_of_list
      |> Seq.map(
           Range.map(
             ~f_inc=range_map_inc(~cur_branch_search_start),
             ~f_exc=range_map_exc(~cur_branch_search_start),
           ),
         )
    };
  };
};

module Matching_minutes = {
  let get_cur_branch_search_start =
      (~overall_search_start: Time.Date_time.t, cur_branch: Time.Date_time.t)
      : Time.Date_time.t =>
    if (cur_branch.year == overall_search_start.year
        && cur_branch.month == overall_search_start.month
        && cur_branch.day == overall_search_start.day
        && cur_branch.hour == overall_search_start.hour) {
      overall_search_start;
    } else {
      Time.Date_time.set_to_first_min_sec(cur_branch);
    };

  let matching_minutes =
      (
        t: time_pattern,
        ~overall_search_start: Time.Date_time.t,
        cur_branch: Time.Date_time.t,
      )
      : Seq.t(Time.Date_time.t) => {
    let cur_branch_search_start =
      get_cur_branch_search_start(~overall_search_start, cur_branch);

    switch (t.minutes) {
    | [] =>
      Seq.map(
        minute => {...cur_branch, minute},
        OSeq.(cur_branch_search_start.minute --^ 60),
      )
    | pat_min_list =>
      pat_min_list
      |> List.to_seq
      |> Seq.filter(minute =>
           cur_branch_search_start.minute <= minute && minute < 60
         )
      |> Seq.map(minute => {...cur_branch_search_start, minute})
    };
  };

  let matching_minute_ranges =
      (
        t: time_pattern,
        ~overall_search_start: Time.Date_time.t,
        cur_branch: Time.Date_time.t,
      )
      : Seq.t(Range.range(Time.Date_time.t)) => {
    let range_map_start = (~cur_branch_search_start: Time.Date_time.t, x) =>
      if (x == cur_branch_search_start.minute) {
        cur_branch_search_start;
      } else {
        Time.Date_time.set_to_first_sec({
          ...cur_branch_search_start,
          minute: x,
        });
      };

    let range_map_inc = (~cur_branch_search_start: Time.Date_time.t, (x, y)) => (
      range_map_start(~cur_branch_search_start, x),
      Time.Date_time.set_to_last_sec({...cur_branch_search_start, minute: y}),
    );

    let range_map_exc = (~cur_branch_search_start: Time.Date_time.t, (x, y)) => (
      range_map_start(~cur_branch_search_start, x),
      Time.Date_time.set_to_first_sec({
        ...cur_branch_search_start,
        minute: y,
      }),
    );

    let cur_branch_search_start =
      get_cur_branch_search_start(~overall_search_start, cur_branch);

    switch (t.minutes) {
    | [] =>
      Seq.return(
        `Range_inc((
          cur_branch_search_start,
          Time.Date_time.set_to_last_min_sec(cur_branch_search_start),
        )),
      )
    | l =>
      List.filter(min => cur_branch_search_start.minute <= min, l)
      |> List.sort_uniq(compare)
      |> Time.Minute_ranges.Of_list.range_seq_of_list
      |> Seq.map(
           Range.map(
             ~f_inc=range_map_inc(~cur_branch_search_start),
             ~f_exc=range_map_exc(~cur_branch_search_start),
           ),
         )
    };
  };
};

module Matching_hours = {
  let get_cur_branch_search_start =
      (~overall_search_start: Time.Date_time.t, cur_branch: Time.Date_time.t)
      : Time.Date_time.t =>
    if (cur_branch.year == overall_search_start.year
        && cur_branch.month == overall_search_start.month
        && cur_branch.day == overall_search_start.day) {
      overall_search_start;
    } else {
      Time.Date_time.set_to_first_hour_min_sec(cur_branch);
    };

  let matching_hours =
      (
        t: time_pattern,
        ~overall_search_start: Time.Date_time.t,
        cur_branch: Time.Date_time.t,
      )
      : Seq.t(Time.Date_time.t) => {
    let cur_branch_search_start =
      get_cur_branch_search_start(~overall_search_start, cur_branch);

    switch (t.hours) {
    | [] =>
      Seq.map(
        hour => {...cur_branch, hour},
        OSeq.(cur_branch_search_start.hour --^ 24),
      )
    | pat_hour_list =>
      pat_hour_list
      |> List.to_seq
      |> Seq.filter(hour => cur_branch_search_start.hour <= hour && hour < 24)
      |> Seq.map(hour => {...cur_branch, hour})
    };
  };

  let matching_hour_ranges =
      (
        t: time_pattern,
        ~overall_search_start: Time.Date_time.t,
        cur_branch: Time.Date_time.t,
      )
      : Seq.t(Range.range(Time.Date_time.t)) => {
    let range_map_start = (~cur_branch_search_start: Time.Date_time.t, x) =>
      if (x == cur_branch_search_start.hour) {
        cur_branch_search_start;
      } else {
        Time.Date_time.set_to_first_min_sec({
          ...cur_branch_search_start,
          hour: x,
        });
      };

    let range_map_inc = (~cur_branch_search_start: Time.Date_time.t, (x, y)) => (
      range_map_start(~cur_branch_search_start, x),
      Time.Date_time.set_to_last_min_sec({
        ...cur_branch_search_start,
        hour: y,
      }),
    );

    let range_map_exc = (~cur_branch_search_start: Time.Date_time.t, (x, y)) => (
      range_map_start(~cur_branch_search_start, x),
      Time.Date_time.set_to_first_min_sec({
        ...cur_branch_search_start,
        hour: y,
      }),
    );

    let cur_branch_search_start =
      get_cur_branch_search_start(~overall_search_start, cur_branch);

    switch (t.hours) {
    | [] =>
      Seq.return(
        `Range_inc((
          cur_branch_search_start,
          Time.Date_time.set_to_last_hour_min_sec(cur_branch),
        )),
      )
    | l =>
      List.filter(
        hour => cur_branch_search_start.hour <= hour && hour < 24,
        l,
      )
      |> List.sort_uniq(compare)
      |> Time.Hour_ranges.Of_list.range_seq_of_list
      |> Seq.map(
           Range.map(
             ~f_inc=range_map_inc(~cur_branch_search_start),
             ~f_exc=range_map_exc(~cur_branch_search_start),
           ),
         )
    };
  };
};

module Matching_days = {
  let get_cur_branch_search_start =
      (~overall_search_start: Time.Date_time.t, cur_branch: Time.Date_time.t)
      : Time.Date_time.t =>
    if (cur_branch.year == overall_search_start.year
        && cur_branch.month == overall_search_start.month) {
      overall_search_start;
    } else {
      Time.Date_time.set_to_first_day_hour_min_sec(cur_branch);
    };

  let int_month_days_of_matching_weekdays =
      (t: time_pattern, ~cur_branch_search_start: Time.Date_time.t)
      : Seq.t(int) => {
    let day_count =
      Time.day_count_of_month(
        ~year=cur_branch_search_start.year,
        ~month=cur_branch_search_start.month,
      );

    switch (t.weekdays) {
    | [] => OSeq.(cur_branch_search_start.day -- day_count)
    | l =>
      OSeq.(cur_branch_search_start.day -- day_count)
      |> Seq.filter(mday =>
           switch (
             Time.weekday_of_month_day(
               ~year=cur_branch_search_start.year,
               ~month=cur_branch_search_start.month,
               ~mday,
             )
           ) {
           | Ok(wday) => List.mem(wday, l)
           | Error () => false
           }
         )
    };
  };

  let direct_matching_int_month_days =
      (t: time_pattern, ~cur_branch_search_start: Time.Date_time.t)
      : Seq.t(int) => {
    let day_count =
      Time.day_count_of_month(
        ~year=cur_branch_search_start.year,
        ~month=cur_branch_search_start.month,
      );

    switch (t.month_days) {
    | [] => OSeq.(cur_branch_search_start.day -- day_count)
    | l =>
      List.filter(
        mday => cur_branch_search_start.day <= mday && mday <= day_count,
        l,
      )
      |> List.sort_uniq(compare)
      |> List.to_seq
    };
  };

  let matching_int_month_days =
      (t: time_pattern, ~cur_branch_search_start: Time.Date_time.t)
      : Seq.t(int) => {
    let matching_month_days =
      direct_matching_int_month_days(t, ~cur_branch_search_start)
      |> List.of_seq
      |> List.sort_uniq(compare);

    let month_days_of_matching_weekdays =
      int_month_days_of_matching_weekdays(t, ~cur_branch_search_start)
      |> List.of_seq
      |> List.sort_uniq(compare);

    OSeq.(1 -- 31)
    |> Seq.filter(mday =>
         List.mem(mday, matching_month_days)
         && List.mem(mday, month_days_of_matching_weekdays)
       );
  };

  let matching_days =
      (
        t: time_pattern,
        ~overall_search_start: Time.Date_time.t,
        cur_branch: Time.Date_time.t,
      )
      : Seq.t(Time.Date_time.t) => {
    let cur_branch_search_start =
      get_cur_branch_search_start(~overall_search_start, cur_branch);

    matching_int_month_days(t, ~cur_branch_search_start)
    |> Seq.map(day => {...cur_branch_search_start, day});
  };

  let matching_day_ranges =
      (
        t: time_pattern,
        ~overall_search_start: Time.Date_time.t,
        cur_branch: Time.Date_time.t,
      )
      : Seq.t(Range.range(Time.Date_time.t)) => {
    let range_map_start = (~cur_branch_search_start: Time.Date_time.t, x) =>
      if (x == cur_branch_search_start.day) {
        cur_branch_search_start;
      } else {
        Time.Date_time.set_to_first_hour_min_sec({
          ...cur_branch_search_start,
          day: x,
        });
      };

    let range_map_inc = (~cur_branch_search_start: Time.Date_time.t, (x, y)) => (
      range_map_start(~cur_branch_search_start, x),
      Time.Date_time.set_to_last_hour_min_sec({
        ...cur_branch_search_start,
        day: y,
      }),
    );

    let range_map_exc = (~cur_branch_search_start: Time.Date_time.t, (x, y)) => (
      range_map_start(~cur_branch_search_start, x),
      Time.Date_time.set_to_first_hour_min_sec({
        ...cur_branch_search_start,
        day: y,
      }),
    );

    let cur_branch_search_start =
      get_cur_branch_search_start(~overall_search_start, cur_branch);

    let f_inc = range_map_inc(~cur_branch_search_start);
    let f_exc = range_map_exc(~cur_branch_search_start);
    switch (t.month_days, t.weekdays) {
    | ([], []) =>
      Seq.return(
        `Range_inc((
          cur_branch_search_start,
          Time.Date_time.set_to_last_day_hour_min_sec(cur_branch),
        )),
      )
    | ([], _weekdays) =>
      int_month_days_of_matching_weekdays(t, ~cur_branch_search_start)
      |> Time.Month_day_ranges.Of_seq.range_seq_of_seq
      |> Seq.map(Range.map(~f_inc, ~f_exc))
    | (_month_days, []) =>
      direct_matching_int_month_days(t, ~cur_branch_search_start)
      |> Time.Month_day_ranges.Of_seq.range_seq_of_seq
      |> Seq.map(Range.map(~f_inc, ~f_exc))
    | (_, _) =>
      matching_int_month_days(t, ~cur_branch_search_start)
      |> Time.Month_day_ranges.Of_seq.range_seq_of_seq
      |> Seq.map(Range.map(~f_inc, ~f_exc))
    };
  };
};

module Matching_months = {
  let get_cur_branch_search_start =
      (~overall_search_start: Time.Date_time.t, cur_branch: Time.Date_time.t)
      : Time.Date_time.t =>
    if (cur_branch.year == overall_search_start.year) {
      overall_search_start;
    } else {
      Time.Date_time.set_to_first_month_day_hour_min_sec(cur_branch);
    };

  let matching_months =
      (
        t: time_pattern,
        ~overall_search_start: Time.Date_time.t,
        cur_branch: Time.Date_time.t,
      )
      : Seq.t(Time.Date_time.t) => {
    let cur_branch_search_start =
      get_cur_branch_search_start(~overall_search_start, cur_branch);

    let start_month_int =
      Time.human_int_of_month(cur_branch_search_start.month);

    switch (t.months) {
    | [] =>
      OSeq.(start_month_int -- 12)
      |> Seq.map(month => Time.month_of_human_int(month) |> Result.get_ok)
      |> Seq.map(month => {...cur_branch_search_start, month})
    | pat_mon_list =>
      pat_mon_list
      |> List.to_seq
      |> Seq.map(Time.human_int_of_month)
      |> Seq.filter(month => start_month_int <= month && month <= 12)
      |> Seq.map(month => Time.month_of_human_int(month) |> Result.get_ok)
      |> Seq.map(month => {...cur_branch_search_start, month})
    };
  };

  let matching_month_ranges =
      (
        t: time_pattern,
        ~overall_search_start: Time.Date_time.t,
        cur_branch: Time.Date_time.t,
      )
      : Seq.t(Range.range(Time.Date_time.t)) => {
    let range_map_start = (~cur_branch_search_start: Time.Date_time.t, x) =>
      if (x == cur_branch_search_start.month) {
        cur_branch_search_start;
      } else {
        Time.Date_time.set_to_first_day_hour_min_sec({
          ...cur_branch_search_start,
          month: x,
        });
      };

    let range_map_inc = (~cur_branch_search_start: Time.Date_time.t, (x, y)) => (
      range_map_start(~cur_branch_search_start, x),
      Time.Date_time.set_to_last_day_hour_min_sec({
        ...cur_branch_search_start,
        month: y,
      }),
    );

    let range_map_exc = (~cur_branch_search_start: Time.Date_time.t, (x, y)) => (
      range_map_start(~cur_branch_search_start, x),
      Time.Date_time.set_to_first_day_hour_min_sec({
        ...cur_branch_search_start,
        month: y,
      }),
    );

    let cur_branch_search_start =
      get_cur_branch_search_start(~overall_search_start, cur_branch);

    let start_month_int =
      Time.human_int_of_month(cur_branch_search_start.month);

    switch (t.months) {
    | [] =>
      Seq.return(
        `Range_inc((
          cur_branch_search_start,
          Time.Date_time.set_to_last_month_day_hour_min_sec(
            cur_branch_search_start,
          ),
        )),
      )
    | l =>
      l
      |> List.sort_uniq(Time.compare_month)
      |> List.to_seq
      |> Seq.map(Time.human_int_of_month)
      |> Seq.filter(month => start_month_int <= month && month <= 12)
      |> Seq.map(month => Time.month_of_human_int(month) |> Result.get_ok)
      |> Time.Month_ranges.Of_seq.range_seq_of_seq
      |> Seq.map(
           Range.map(
             ~f_inc=range_map_inc(~cur_branch_search_start),
             ~f_exc=range_map_exc(~cur_branch_search_start),
           ),
         )
    };
  };
};

module Matching_years = {
  let matching_years =
      (
        ~search_years_ahead,
        t: time_pattern,
        ~overall_search_start: Time.Date_time.t,
      )
      : Seq.t(Time.Date_time.t) =>
    switch (t.years) {
    | [] =>
      OSeq.(
        overall_search_start.year
        --^ (overall_search_start.year + search_years_ahead)
      )
      |> Seq.map(year => {...overall_search_start, year})
    | pat_year_list =>
      pat_year_list
      |> List.to_seq
      |> Seq.filter(year =>
           overall_search_start.year <= year
           && year < overall_search_start.year
           + search_years_ahead
         )
      |> Seq.map(year => {...overall_search_start, year})
    };

  let matching_year_ranges =
      (
        ~search_years_ahead,
        t: time_pattern,
        ~overall_search_start: Time.Date_time.t,
      )
      : Seq.t(Range.range(Time.Date_time.t)) => {
    let range_map_start = (~overall_search_start: Time.Date_time.t, x) =>
      if (x == overall_search_start.year) {
        overall_search_start;
      } else {
        Time.Date_time.set_to_first_month_day_hour_min_sec({
          ...overall_search_start,
          year: x,
        });
      };

    let range_map_inc = (~overall_search_start: Time.Date_time.t, (x, y)) => (
      range_map_start(~overall_search_start, x),
      Time.Date_time.set_to_last_month_day_hour_min_sec({
        ...overall_search_start,
        year: y,
      }),
    );

    let range_map_exc = (~overall_search_start: Time.Date_time.t, (x, y)) => (
      range_map_start(~overall_search_start, x),
      Time.Date_time.set_to_last_month_day_hour_min_sec({
        ...overall_search_start,
        year: y,
      }),
    );

    switch (t.years) {
    | [] =>
      Seq.return(
        `Range_inc((
          overall_search_start,
          Time.Date_time.set_to_last_month_day_hour_min_sec({
            ...overall_search_start,
            year: overall_search_start.year + search_years_ahead - 1,
          }),
        )),
      )
    | l =>
      List.sort_uniq(compare, l)
      |> List.to_seq
      |> Seq.filter(year =>
           overall_search_start.year <= year
           && year < overall_search_start.year
           + search_years_ahead
         )
      |> Time.Year_ranges.Of_seq.range_seq_of_seq
      |> Seq.map(
           Range.map(
             ~f_inc=range_map_inc(~overall_search_start),
             ~f_exc=range_map_exc(~overall_search_start),
           ),
         )
    };
  };
};

module Matching_unix_seconds = {
  let matching_unix_seconds =
      (
        ~search_using_tz_offset_s: option(Time.tz_offset_s),
        t: time_pattern,
        start: Time.Date_time.t,
      )
      : Time.Date_time_set.t =>
    switch (Time.Date_time.to_unix_second(start)) {
    | Error () => Time.Date_time_set.empty
    | Ok(start) =>
      t.unix_seconds
      |> List.sort_uniq(compare)
      |> List.to_seq
      |> OSeq.filter(x => x >= start)
      |> Seq.filter_map(x =>
           switch (
             Time.Date_time.of_unix_second(
               ~tz_offset_s_of_date_time=search_using_tz_offset_s,
               x,
             )
           ) {
           | Ok(x) => Some(x)
           | Error () => None
           }
         )
      |> Time.Date_time_set.of_seq
    };
};

let override_search_param_possibly =
    (
      ~allow_search_param_override,
      search_param: Search_param.t,
      t: time_pattern,
    )
    : Search_param.t =>
  if (allow_search_param_override) {
    switch (t.years) {
    | [] => search_param
    | l =>
      let l = List.sort_uniq(compare, l);
      let start_year = List.hd(l);
      let end_inc_year = List.hd(List.rev(l));
      let search_using_tz_offset_s =
        Option.value(~default=0, search_param.search_using_tz_offset_s);

      let start_date_time =
        Time.Date_time.(
          {...min, year: start_year, tz_offset_s: search_using_tz_offset_s}
          |> set_to_first_month_day_hour_min_sec
        );

      switch (
        Search_param.start_date_time_and_search_years_ahead_of_search_param(
          search_param,
        )
      ) {
      | None =>
        Search_param.{
          search_using_tz_offset_s: Some(search_using_tz_offset_s),
          typ:
            Years_ahead({
              start: `Date_time(start_date_time),
              years_ahead: end_inc_year - start_year + 1,
            }),
        }
      | Some((start_date_time', search_years_ahead')) =>
        let cmp_value =
          Time.Date_time.compare(start_date_time, start_date_time');

        let end_inc_year =
          max(start_date_time'.year + search_years_ahead', end_inc_year);

        let start_date_time =
          if (cmp_value <= 0) {
            start_date_time;
          } else {
            start_date_time';
          };

        Search_param.{
          search_using_tz_offset_s: Some(search_using_tz_offset_s),
          typ:
            Years_ahead({
              start: `Date_time(start_date_time),
              years_ahead: end_inc_year - start_date_time.year + 1,
            }),
        };
      };
    };
  } else {
    search_param;
  };

module Single_pattern = {
  let filter_using_matching_unix_seconds =
      (
        ~search_using_tz_offset_s,
        t: time_pattern,
        ~overall_search_start,
        s: Seq.t(Time.Date_time.t),
      )
      : Seq.t(Time.Date_time.t) => {
    let matching_unix_seconds =
      Matching_unix_seconds.matching_unix_seconds(
        ~search_using_tz_offset_s,
        t,
        overall_search_start,
      );

    if (Time.Date_time_set.is_empty(matching_unix_seconds)) {
      s;
    } else {
      Seq.filter(x => Time.Date_time_set.mem(x, matching_unix_seconds), s);
    };
  };

  let date_time_range_seq_of_unix_seconds =
      (~search_using_tz_offset_s, s: Seq.t(int64))
      : Seq.t(Range.range(Time.Date_time.t)) => {
    let f = ((x, y)) => (
      Time.Date_time.of_unix_second(
        ~tz_offset_s_of_date_time=search_using_tz_offset_s,
        x,
      ),
      Time.Date_time.of_unix_second(
        ~tz_offset_s_of_date_time=search_using_tz_offset_s,
        y,
      ),
    );

    s
    |> Ranges.Of_seq.range_seq_of_seq(
         ~modulo=None,
         ~to_int64=x => x,
         ~of_int64=x => x,
       )
    |> Seq.map(Range.map(~f_inc=f, ~f_exc=f))
    |> Seq.filter_map(Range_utils.result_range_get);
  };

  let matching_date_times =
      (
        ~allow_search_param_override: bool,
        search_param: Search_param.t,
        t: time_pattern,
      )
      : result(Seq.t(Time.Date_time.t), error) =>
    Check.check_search_param_and_time_pattern(search_param, t)
    |> Result.map(() =>{
         let search_param =
           override_search_param_possibly(
             ~allow_search_param_override,
             search_param,
             t,
           );

         switch (
           Search_param.start_date_time_and_search_years_ahead_of_search_param(
             search_param,
           )
         ) {
         | None => Seq.empty
         | Some((overall_search_start, search_years_ahead)) =>
           let search_using_tz_offset_s =
             search_param.search_using_tz_offset_s;

           Matching_years.matching_years(
             ~search_years_ahead,
             t,
             ~overall_search_start,
           )
           |> Seq.flat_map(
                Matching_months.matching_months(t, ~overall_search_start),
              )
           |> Seq.flat_map(
                Matching_days.matching_days(t, ~overall_search_start),
              )
           |> Seq.flat_map(
                Matching_hours.matching_hours(t, ~overall_search_start),
              )
           |> Seq.flat_map(
                Matching_minutes.matching_minutes(t, ~overall_search_start),
              )
           |> Seq.flat_map(
                Matching_seconds.matching_seconds(t, ~overall_search_start),
              )
           |> filter_using_matching_unix_seconds(
                ~search_using_tz_offset_s,
                t,
                ~overall_search_start,
              );
         };
      } );

  let matching_unix_seconds =
      (
        ~allow_search_param_override: bool,
        search_param: Search_param.t,
        t: time_pattern,
      )
      : result(Seq.t(int64), error) =>
    matching_date_times(~allow_search_param_override, search_param, t)
    |> Result.map(s =>
         Seq.filter_map(
           x =>
             switch (Time.Date_time.to_unix_second(x)) {
             | Ok(x) => Some(x)
             | Error () => None
             },
           s,
         )
       );

  let matching_date_time_ranges =
      (
        ~allow_search_param_override: bool,
        search_param: Search_param.t,
        t: time_pattern,
      )
      : result(Seq.t(Range.range(Time.Date_time.t)), error) =>
    switch (Check.check_search_param_and_time_pattern(search_param, t)) {
    | Error(msg) => Error(msg)
    | Ok () =>
      let search_param =
        override_search_param_possibly(
          ~allow_search_param_override,
          search_param,
          t,
        );

      switch (
        Search_param.start_date_time_and_search_years_ahead_of_search_param(
          search_param,
        )
      ) {
      | None => Ok(Seq.empty)
      | Some((overall_search_start, search_years_ahead)) =>
        let search_using_tz_offset_s = search_param.search_using_tz_offset_s;

        switch (
          t.years,
          t.months,
          t.month_days,
          t.weekdays,
          t.hours,
          t.minutes,
          t.seconds,
          t.unix_seconds,
        ) {
        | (_years, [], [], [], [], [], [], []) =>
          Matching_years.matching_year_ranges(
            ~search_years_ahead,
            t,
            ~overall_search_start,
          )
          |> Result.ok
        | (_years, _months, [], [], [], [], [], []) =>
          Matching_years.matching_years(
            ~search_years_ahead,
            t,
            ~overall_search_start,
          )
          |> Seq.flat_map(
               Matching_months.matching_month_ranges(
                 t,
                 ~overall_search_start,
               ),
             )
          |> Result.ok
        | (_years, _months, _month_days, _weekdays, [], [], [], []) =>
          Matching_years.matching_years(
            ~search_years_ahead,
            t,
            ~overall_search_start,
          )
          |> Seq.flat_map(
               Matching_months.matching_months(t, ~overall_search_start),
             )
          |> Seq.flat_map(
               Matching_days.matching_day_ranges(t, ~overall_search_start),
             )
          |> Result.ok
        | (_years, _months, _month_days, _weekdays, _hours, [], [], []) =>
          Matching_years.matching_years(
            ~search_years_ahead,
            t,
            ~overall_search_start,
          )
          |> Seq.flat_map(
               Matching_months.matching_months(t, ~overall_search_start),
             )
          |> Seq.flat_map(
               Matching_days.matching_days(t, ~overall_search_start),
             )
          |> Seq.flat_map(
               Matching_hours.matching_hour_ranges(t, ~overall_search_start),
             )
          |> Result.ok
        | (_years, _months, _month_days, _weekdays, _hours, _minutes, [], []) =>
          Matching_years.matching_years(
            ~search_years_ahead,
            t,
            ~overall_search_start,
          )
          |> Seq.flat_map(
               Matching_months.matching_months(t, ~overall_search_start),
             )
          |> Seq.flat_map(
               Matching_days.matching_days(t, ~overall_search_start),
             )
          |> Seq.flat_map(
               Matching_hours.matching_hours(t, ~overall_search_start),
             )
          |> Seq.flat_map(
               Matching_minutes.matching_minute_ranges(
                 t,
                 ~overall_search_start,
               ),
             )
          |> Result.ok
        | (
            _years,
            _months,
            _month_days,
            _weekdays,
            _hours,
            _minutes,
            _seconds,
            [],
          ) =>
          Matching_years.matching_years(
            ~search_years_ahead,
            t,
            ~overall_search_start,
          )
          |> Seq.flat_map(
               Matching_months.matching_months(t, ~overall_search_start),
             )
          |> Seq.flat_map(
               Matching_days.matching_days(t, ~overall_search_start),
             )
          |> Seq.flat_map(
               Matching_hours.matching_hours(t, ~overall_search_start),
             )
          |> Seq.flat_map(
               Matching_minutes.matching_minutes(t, ~overall_search_start),
             )
          |> Seq.flat_map(
               Matching_seconds.matching_second_ranges(
                 t,
                 ~overall_search_start,
               ),
             )
          |> Result.ok
        | ([], [], [], [], [], [], [], unix_seconds) =>
          unix_seconds
          |> List.to_seq
          |> date_time_range_seq_of_unix_seconds(~search_using_tz_offset_s)
          |> Result.ok
        | (
            _years,
            _months,
            _month_days,
            _weekdays,
            _hours,
            _minutes,
            _seconds,
            _unix_seconds,
          ) =>
          Matching_years.matching_years(
            ~search_years_ahead,
            t,
            ~overall_search_start,
          )
          |> Seq.flat_map(
               Matching_months.matching_months(t, ~overall_search_start),
             )
          |> Seq.flat_map(
               Matching_days.matching_days(t, ~overall_search_start),
             )
          |> Seq.flat_map(
               Matching_hours.matching_hours(t, ~overall_search_start),
             )
          |> Seq.flat_map(
               Matching_minutes.matching_minutes(t, ~overall_search_start),
             )
          |> Seq.flat_map(
               Matching_seconds.matching_seconds(t, ~overall_search_start),
             )
          |> filter_using_matching_unix_seconds(
               ~search_using_tz_offset_s,
               t,
               ~overall_search_start,
             )
          |> Seq.map(x => `Range_inc((x, x)))
          |> Result.ok
        };
      };
    };

  let matching_time_slots =
      (
        ~allow_search_param_override,
        search_param: Search_param.t,
        t: time_pattern,
      )
      : result(Seq.t(Time_slot.t), error) => {
    let f = ((x, y)) => (
      Time.Date_time.to_unix_second(x),
      Time.Date_time.to_unix_second(y),
    );

    matching_date_time_ranges(~allow_search_param_override, search_param, t)
    |> Result.map(s =>
         s
         |> Seq.map(Range.map(~f_inc=f, ~f_exc=f))
         |> Seq.filter_map(Range_utils.result_range_get)
         |> Seq.map(r =>
              switch (r) {
              | `Range_inc(x, y) => (x, Int64.succ(y))
              | `Range_exc(x, y) => (x, y)
              }
            )
         |> (
           l => {
             let time_slots =
               switch (search_param.typ) {
               | Time_slots(time_slots) =>
                 let time_slots =
                   time_slots |> Time_slots.Normalize.normalize_list_in_seq_out;

                 Some(time_slots);
               | _ => None
               };

             switch (time_slots) {
             | None => l
             | Some(time_slots) =>
               Time_slots.inter(time_slots, ~skip_check=true, l)
               |> Time_slots.Normalize.normalize(
                    ~skip_filter_invalid=true,
                    ~skip_sort=true,
                  )
             };
           }
         )
       );
  };

  let matching_time_slots_round_robin_non_decreasing =
      (
        ~allow_search_param_override,
        search_param: Search_param.t,
        l: list(time_pattern),
      )
      : result(Seq.t(list(Time_slot.t)), error) => {
    let l =
      List.map(
        matching_time_slots(~allow_search_param_override, search_param),
        l,
      );

    switch (List.find_opt(Result.is_error, l)) {
    | Some(e) => Error(Result.get_error(e))
    | None =>
      l
      |> List.map(Result.get_ok)
      |> Time_slots.Round_robin.collect_round_robin_non_decreasing(
           ~skip_check=true,
         )
      |> OSeq.take_while(List.for_all(Option.is_some))
      |> Seq.map(List.map(Option.get))
      |> Result.ok
    };
  };

  let matching_time_slots_round_robin_non_decreasing_flat =
      (
        ~allow_search_param_override,
        search_param: Search_param.t,
        l: list(time_pattern),
      )
      : result(Seq.t(Time_slot.t), error) =>
    matching_time_slots_round_robin_non_decreasing(
      ~allow_search_param_override,
      search_param,
      l,
    )
    |> Result.map(Seq.flat_map(List.to_seq));

  let next_match_date_time =
      (
        ~allow_search_param_override: bool,
        search_param: Search_param.t,
        t: time_pattern,
      )
      : result(option(Time.Date_time.t), error) =>
    matching_date_times(~allow_search_param_override, search_param, t)
    |> Result.map(s =>
         switch (s()) {
         | Seq.Nil => None
         | [@implicit_arity] Seq.Cons(x, _) => Some(x)
         }
       );

  let next_match_unix_second =
      (
        ~allow_search_param_override: bool,
        search_param: Search_param.t,
        t: time_pattern,
      )
      : result(option(int64), error) =>
    next_match_date_time(~allow_search_param_override, search_param, t)
    |> Result.map(x =>
         switch (x) {
         | None => None
         | Some(x) =>
           switch (Time.Date_time.to_unix_second(x)) {
           | Error () => None
           | Ok(x) => Some(x)
           }
         }
       );

  let next_match_time_slot =
      (
        ~allow_search_param_override: bool,
        search_param: Search_param.t,
        t: time_pattern,
      )
      : result(option(Time_slot.t), error) =>
    matching_time_slots(~allow_search_param_override, search_param, t)
    |> Result.map(s =>
         switch (s()) {
         | Seq.Nil => None
         | [@implicit_arity] Seq.Cons(x, _) => Some(x)
         }
       );
};

module Range_pattern = {
  let matching_time_slots =
      (
        ~allow_search_param_override,
        search_param: Search_param.t,
        range: time_range_pattern,
      )
      : result(Seq.t(Time_slot.t), error) => {
    let start_pat =
      switch (range) {
      | `Range_inc(t1, _)
      | `Range_exc(t1, _) => t1
      };

    let search_param =
      override_search_param_possibly(
        ~allow_search_param_override,
        search_param,
        start_pat,
      );

    let search_and_get_start =
        (
          search_param: Search_param.t,
          t: time_pattern,
          (start, _): Time_slot.t,
        )
        : option(Time_slot.t) => {
      let search_param =
        Search_param.push_search_param_to_later_start(~start, search_param)
        |> Result.get_ok;

      switch (
        Single_pattern.next_match_time_slot(
          ~allow_search_param_override=false,
          search_param,
          t,
        )
        |> Result.get_ok
      ) {
      | None => None
      | Some((start', _)) => Some((start, start'))
      };
    };

    let search_and_get_end_exc =
        (
          search_param: Search_param.t,
          t: time_pattern,
          (start, _): Time_slot.t,
        )
        : option(Time_slot.t) => {
      let search_param =
        Search_param.push_search_param_to_later_start(~start, search_param)
        |> Result.get_ok;

      switch (
        Single_pattern.next_match_time_slot(
          ~allow_search_param_override=false,
          search_param,
          t,
        )
        |> Result.get_ok
      ) {
      | None => None
      | Some((_, end_exc')) => Some((start, end_exc'))
      };
    };

    Check.check_search_param_and_time_range_pattern(search_param, range)
    |> Result.map(() =>{
         let s =
           Single_pattern.matching_time_slots(
             ~allow_search_param_override=false,
             search_param,
             start_pat,
           )
           |> Result.get_ok;

         switch (range) {
         | `Range_inc(_, t2) =>
           Seq.filter_map(search_and_get_end_exc(search_param, t2), s)
         | `Range_exc(_, t2) =>
           Seq.filter_map(search_and_get_start(search_param, t2), s)
         };
       });
  };

  let next_match_time_slot =
      (
        ~allow_search_param_override,
        search_param: Search_param.t,
        range: time_range_pattern,
      )
      : result(option((int64, int64)), error) =>
    matching_time_slots(~allow_search_param_override, search_param, range)
    |> Result.map(s =>
         switch (s()) {
         | Seq.Nil => None
         | [@implicit_arity] Seq.Cons((start, end_exc), _) =>
           Some((start, end_exc))
         }
       );

  let matching_time_slots_multi =
      (
        ~allow_search_param_override,
        search_param: Search_param.t,
        l: list(time_range_pattern),
      )
      : result(Seq.t(Time_slot.t), error) => {
    let l =
      List.map(
        matching_time_slots(~allow_search_param_override, search_param),
        l,
      );

    switch (List.find_opt(Result.is_error, l)) {
    | Some(e) => Error(Result.get_error(e))
    | None =>
      l
      |> List.map(Result.get_ok)
      |> Time_slots.Merge.merge_multi_list(~skip_check=true)
      |> Result.ok
    };
  };

  let next_match_time_slot_multi =
      (
        ~allow_search_param_override,
        search_param: Search_param.t,
        l: list(time_range_pattern),
      )
      : result(option((int64, int64)), error) =>
    matching_time_slots_multi(~allow_search_param_override, search_param, l)
    |> Result.map(s =>
         switch (s()) {
         | Seq.Nil => None
         | [@implicit_arity] Seq.Cons((start, end_exc), _) =>
           Some((start, end_exc))
         }
       );

  let matching_time_slots_round_robin_non_decreasing =
      (
        ~allow_search_param_override,
        search_param: Search_param.t,
        l: list(time_range_pattern),
      )
      : result(Seq.t(list(Time_slot.t)), error) => {
    let l =
      List.map(
        matching_time_slots(~allow_search_param_override, search_param),
        l,
      );

    switch (List.find_opt(Result.is_error, l)) {
    | Some(e) => Error(Result.get_error(e))
    | None =>
      l
      |> List.map(Result.get_ok)
      |> Time_slots.Round_robin.collect_round_robin_non_decreasing(
           ~skip_check=true,
         )
      |> OSeq.take_while(List.for_all(Option.is_some))
      |> Seq.map(List.map(Option.get))
      |> Result.ok
    };
  };

  let matching_time_slots_round_robin_non_decreasing_flat =
      (
        ~allow_search_param_override,
        search_param: Search_param.t,
        l: list(time_range_pattern),
      )
      : result(Seq.t(Time_slot.t), error) =>
    matching_time_slots_round_robin_non_decreasing(
      ~allow_search_param_override,
      search_param,
      l,
    )
    |> Result.map(Seq.flat_map(List.to_seq));
};

/* module Single_or_ranges = struct
 *   let matching_time_slots (search_param : Search_param.t) (x : single_or_ranges)
 *     : (Time_slot.t Seq.t, error) result =
 *     match x with
 *     | Single_time_pattern pat ->
 *       Single_pattern.matching_time_slots search_param pat
 *     | Time_range_patterns l ->
 *       Range_pattern.matching_time_slots_multi search_param l
 *
 *   let next_match_time_slot (search_param : Search_param.t)
 *       (x : single_or_ranges) : (Time_slot.t option, error) result =
 *     matching_time_slots search_param x
 *     |> Result.map (fun s ->
 *         match s () with
 *         | Seq.Nil -> None
 *         | Seq.Cons ((start, end_exc), _) -> Some (start, end_exc))
 *
 *   let matching_time_slots_round_robin_non_decreasing
 *       (search_param : Search_param.t) (t : single_or_ranges) :
 *     (Time_slot.t list Seq.t, error) result =
 *     match t with
 *     | Single_time_pattern pat ->
 *       Single_pattern.matching_time_slots_round_robin_non_decreasing
 *         search_param [ pat ]
 *     | Time_range_patterns l ->
 *       Range_pattern.matching_time_slots_round_robin_non_decreasing
 *         search_param l
 *
 *   let matching_time_slots_round_robin_non_decreasing_flat
 *       (search_param : Search_param.t) (t : single_or_ranges) :
 *     (Time_slot.t Seq.t, error) result =
 *     matching_time_slots_round_robin_non_decreasing search_param t
 *     |> Result.map (Seq.flat_map List.to_seq)
 * end */

module Serialize = {
  let pack_pattern = (t: time_pattern): Time_pattern_t.time_pattern => {
    years: t.years,
    months: t.months,
    weekdays: t.weekdays,
    month_days: t.month_days,
    hours: t.hours,
    minutes: t.minutes,
    seconds: t.seconds,
    unix_seconds: List.map(Misc_utils.int32_int32_of_int64, t.unix_seconds),
  };
};

module Deserialize = {
  let unpack_pattern = (t: Time_pattern_t.time_pattern): time_pattern => {
    years: t.years,
    months: t.months,
    weekdays: t.weekdays,
    month_days: t.month_days,
    hours: t.hours,
    minutes: t.minutes,
    seconds: t.seconds,
    unix_seconds: List.map(Misc_utils.int64_of_int32_int32, t.unix_seconds),
  };
};

module Equal = {
  let equal = (pat1: time_pattern, pat2: time_pattern): bool =>
    List.sort(compare, pat1.years) == List.sort(compare, pat2.years)
    && List.sort(compare, pat1.months) == List.sort(compare, pat2.months)
    && List.sort(compare, pat1.weekdays) == List.sort(compare, pat2.weekdays)
    && List.sort(compare, pat1.month_days)
    == List.sort(compare, pat2.month_days)
    && List.sort(compare, pat1.hours) == List.sort(compare, pat2.hours)
    && List.sort(compare, pat1.minutes) == List.sort(compare, pat2.minutes);
};

module Parsers = {
  open MParser;
  open Parser_components;

  let end_markers = " ]";

  let non_end_markers = many_satisfy(c => !String.contains(end_markers, c));

  let range_inc_expr = (p: t('a, unit)): t(Range.range('a), unit) =>
    attempt(p >>= (x => hyphen >> p >>= (y => return(`Range_inc((x, y))))))
    <|> (p >>= (x => return(`Range_inc((x, x)))));

  let ranges_expr =
      (
        ~allow_empty,
        ~f_flatten: list(Range.range('a)) => list('a),
        p: t('a, unit),
      )
      : t(list('a), unit) =>
    get_pos
    >>= (
      pos =>
        (
          if (allow_empty) {
            sep_by_comma(range_inc_expr(p));
          } else {
            sep_by_comma1(range_inc_expr(p));
          }
        )
        >>= (
          l =>
            try(return(f_flatten(l))) {
            | Range.Range_is_invalid =>
              fail(
                Printf.sprintf("Invalid range, pos: %s", string_of_pos(pos)),
              )
            }
        )
    );

  let time_pattern_ranges_expr = (p: t(list('a), unit)): t(list('a), unit) =>
    attempt(char('['))
    >> p
    >>= (
      l =>
        attempt(char(']') >> return(l))
        <|> (
          get_pos
          >>= (
            pos =>
              non_square_bracket_string
              >>= (
                s =>
                  if (s == "") {
                    fail(
                      Printf.sprintf(
                        "Missing ], pos: %s",
                        string_of_pos(pos),
                      ),
                    );
                  } else {
                    fail(
                      Printf.sprintf(
                        "Invalid ranges: %s, pos: %s",
                        s,
                        string_of_pos(pos),
                      ),
                    );
                  }
              )
          )
        )
    )
    <|> return([]);

  module Second = {
    let second_expr =
      nat_zero
      >>= (
        x =>
          if (x >= 60) {
            fail(Printf.sprintf("Invalid second: %d", x));
          } else {
            return(x);
          }
      )
      <|> (
        get_pos
        >>= (
          pos =>
            non_end_markers
            >>= (
              s =>
                fail(
                  Printf.sprintf(
                    "Invalid second term: %s, pos: %s",
                    s,
                    string_of_pos(pos),
                  ),
                )
            )
        )
      );

    let seconds_expr = (~allow_empty) =>
      ranges_expr(
        ~allow_empty,
        ~f_flatten=Time.Second_ranges.Flatten.flatten_list,
        second_expr,
      );

    let seconds_cron_expr =
      attempt(char('*') >> return([])) <|> seconds_expr(~allow_empty=false);

    let seconds_time_pattern_expr =
      time_pattern_ranges_expr(seconds_expr(~allow_empty=true));
  };

  module Minute = {
    let minute_expr =
      attempt(nat_zero)
      >>= (
        x =>
          if (x >= 60) {
            fail(Printf.sprintf("Invalid minute: %d", x));
          } else {
            return(x);
          }
      )
      <|> (
        get_pos
        >>= (
          pos =>
            non_end_markers
            >>= (
              s =>
                fail(
                  Printf.sprintf(
                    "Invalid minute term: %s, pos: %s",
                    s,
                    string_of_pos(pos),
                  ),
                )
            )
        )
      );

    let minutes_expr = (~allow_empty) =>
      ranges_expr(
        ~allow_empty,
        ~f_flatten=Time.Minute_ranges.Flatten.flatten_list,
        minute_expr,
      );

    let minutes_cron_expr =
      attempt(char('*') >> return([])) <|> minutes_expr(~allow_empty=false);

    let minutes_time_pattern_expr =
      time_pattern_ranges_expr(minutes_expr(~allow_empty=true));
  };

  module Hour = {
    let hour_expr =
      nat_zero
      >>= (
        x =>
          if (x >= 24) {
            fail(Printf.sprintf("Invalid hour: %d", x));
          } else {
            return(x);
          }
      )
      <|> (
        get_pos
        >>= (
          pos =>
            non_end_markers
            >>= (
              s =>
                fail(
                  Printf.sprintf(
                    "Invalid hour term: %s, pos: %s",
                    s,
                    string_of_pos(pos),
                  ),
                )
            )
        )
      );

    let hours_expr = (~allow_empty) =>
      ranges_expr(
        ~allow_empty,
        ~f_flatten=Time.Hour_ranges.Flatten.flatten_list,
        hour_expr,
      );

    let hours_cron_expr =
      attempt(char('*') >> return([])) <|> hours_expr(~allow_empty=false);

    let hours_time_pattern_expr =
      time_pattern_ranges_expr(hours_expr(~allow_empty=true));
  };

  module Month_day = {
    let month_day_expr =
      nat_zero
      >>= (
        x =>
          if (1 <= x && x <= 31) {
            return(x);
          } else {
            fail(Printf.sprintf("Invalid month day: %d", x));
          }
      );

    let month_days_expr = (~allow_empty) =>
      ranges_expr(
        ~allow_empty,
        ~f_flatten=Time.Month_day_ranges.Flatten.flatten_list,
        month_day_expr,
      );

    let month_days_cron_expr =
      attempt(char('*') >> return([]))
      <|> month_days_expr(~allow_empty=false);

    let month_days_time_pattern_expr =
      time_pattern_ranges_expr(month_days_expr(~allow_empty=true));
  };

  module Month = {
    let month_int_expr =
      nat_zero
      >>= (
        x =>
          switch (Time.month_of_human_int(x)) {
          | Ok(x) => return(x)
          | Error () => fail(Printf.sprintf("Invalid month int: %d", x))
          }
      );

    let month_word_expr = (~for_cron) =>
      alpha_string
      >>= (
        x =>
          if (for_cron && String.length(x) != 3) {
            fail(Printf.sprintf("Invalid length for month string: %s", x));
          } else {
            switch (Time.Of_string.month_of_string(x)) {
            | Ok(x) => return(x)
            | Error () => fail(Printf.sprintf("Invalid month string: %s", x))
            };
          }
      );

    let month_expr = (~for_cron) =>
      /* if for_cron then ((month_word_expr ~for_cron)) <|> month_int_expr
       * else month_word_expr ~for_cron */
      month_word_expr(~for_cron) <|> month_int_expr;

    let months_expr = (~allow_empty, ~for_cron) =>
      ranges_expr(
        ~allow_empty,
        ~f_flatten=Time.Month_ranges.Flatten.flatten_list,
        month_expr(~for_cron),
      );

    let months_cron_expr =
      attempt(char('*') >> return([]))
      <|> months_expr(~allow_empty=false, ~for_cron=true);

    let months_time_pattern_expr =
      time_pattern_ranges_expr(
        months_expr(~allow_empty=true, ~for_cron=false),
      );
  };

  module Year = {
    let year_expr =
      attempt(nat_zero)
      <|> (
        get_pos
        >>= (
          pos =>
            non_end_markers
            >>= (
              s =>
                fail(
                  Printf.sprintf(
                    "Invalid year term: %s, pos: %s",
                    s,
                    string_of_pos(pos),
                  ),
                )
            )
        )
      );

    let years_expr = (~allow_empty) =>
      ranges_expr(
        ~allow_empty,
        ~f_flatten=Time.Year_ranges.Flatten.flatten_list,
        year_expr,
      );

    let years_time_pattern_expr =
      time_pattern_ranges_expr(years_expr(~allow_empty=true));
  };

  module Weekday = {
    let weekday_int_expr =
      get_pos
      >>= (
        pos =>
          nat_zero
          >>= (
            x =>
              switch (Time.weekday_of_tm_int(x)) {
              | Ok(x) => return(x)
              | Error () =>
                fail(
                  Printf.sprintf(
                    "Invalid weekday int: %d, pos: %s",
                    x,
                    string_of_pos(pos),
                  ),
                )
              }
          )
      );

    let weekday_word_expr = (~for_cron) =>
      get_pos
      >>= (
        pos =>
          alpha_string
          >>= (
            x =>
              if (for_cron && String.length(x) != 3) {
                fail(
                  Printf.sprintf(
                    "Invalid length for weekday string: %s, pos: %s",
                    x,
                    string_of_pos(pos),
                  ),
                );
              } else {
                switch (Time.Of_string.weekday_of_string(x)) {
                | Ok(x) => return(x)
                | Error () =>
                  fail(
                    Printf.sprintf(
                      "Invalid weekday string: %s, pos: %s",
                      x,
                      string_of_pos(pos),
                    ),
                  )
                };
              }
          )
      );

    let weekday_expr = (~for_cron) =>
      if (for_cron) {
        weekday_int_expr <|> weekday_word_expr(~for_cron);
      } else {
        weekday_word_expr(~for_cron);
      };

    let weekdays_expr = (~allow_empty, ~for_cron) =>
      ranges_expr(
        ~allow_empty,
        ~f_flatten=Time.Weekday_ranges.Flatten.flatten_list,
        weekday_expr(~for_cron),
      );

    let weekdays_cron_expr =
      attempt(char('*') >> return([]))
      <|> weekdays_expr(~allow_empty=false, ~for_cron=true);

    let weekdays_time_pattern_expr =
      time_pattern_ranges_expr(
        weekdays_expr(~allow_empty=true, ~for_cron=false),
      );
  };

  let cron_expr =
    Minute.minutes_cron_expr
    >>= (
      minutes =>
        spaces1
        >> Hour.hours_cron_expr
        >>= (
          hours =>
            spaces1
            >> Month_day.month_days_cron_expr
            >>= (
              month_days =>
                spaces1
                >> Month.months_cron_expr
                >>= (
                  months =>
                    spaces1
                    >> Weekday.weekdays_cron_expr
                    >>= (
                      weekdays =>
                        eof
                        >> return({
                             years: [],
                             months,
                             month_days,
                             weekdays,
                             hours,
                             minutes,
                             seconds: [],
                             unix_seconds: [],
                           })
                    )
                )
            )
        )
    );

  let unit_char = (c): t(unit, unit) =>
    get_pos
    >>= (
      pos =>
        attempt(char(c))
        >> return()
        <|> (
          satisfy(_ => true)
          >>= (
            c =>
              fail(
                Printf.sprintf(
                  "Invalid unit char: %c, pos: %s",
                  c,
                  string_of_pos(pos),
                ),
              )
          )
        )
    );

  let optional_part = p => option([], attempt(p));

  let time_pattern_core_expr =
    optional_part(unit_char('y') >> spaces >> Year.years_time_pattern_expr)
    >>= (
      years =>
        spaces
        >> optional_part(
             unit_char('m') >> spaces >> Month.months_time_pattern_expr,
           )
        >>= (
          months =>
            spaces
            >> optional_part(
                 unit_char('d')
                 >> spaces
                 >> Month_day.month_days_time_pattern_expr,
               )
            >>= (
              month_days =>
                spaces
                >> optional_part(
                     unit_char('w')
                     >> spaces
                     >> Weekday.weekdays_time_pattern_expr,
                   )
                >>= (
                  weekdays =>
                    spaces
                    >> optional_part(
                         unit_char('h')
                         >> spaces
                         >> Hour.hours_time_pattern_expr,
                       )
                    >>= (
                      hours =>
                        spaces
                        >> unit_char('m')
                        >> spaces
                        >> Minute.minutes_time_pattern_expr
                        >>= (
                          minutes =>
                            spaces
                            >> optional_part(
                                 unit_char('s')
                                 >> spaces
                                 >> Second.seconds_time_pattern_expr,
                               )
                            >>= (
                              seconds =>
                                return({
                                  years,
                                  months,
                                  month_days,
                                  weekdays,
                                  hours,
                                  minutes,
                                  seconds,
                                  unix_seconds: [],
                                })
                            )
                        )
                    )
                )
            )
        )
    );

  let time_pattern_expr = attempt(time_pattern_core_expr) <|> cron_expr;
};

module Of_string = {
  let time_pattern_of_cron_string = (s: string): result(time_pattern, string) =>
    MParser.(
      switch (parse_string(Parsers.cron_expr << eof, s, ())) {
      | Success(x) => Ok(x)
      | [@implicit_arity] Failed(s, _) => Error(s)
      }
    );

  let time_pattern_of_string = (s: string): result(time_pattern, string) =>
    MParser.(
      switch (parse_string(Parsers.time_pattern_expr << spaces << eof, s, ())) {
      | Success(x) => Ok(x)
      | [@implicit_arity] Failed(s, _) => Error(s)
      }
    );
};

module To_string = {
  let string_of_error = (e: error): string =>
    switch (e) {
    | Invalid_search_param(_) => "Invalid search param"
    | Invalid_time_pattern(_) => "Invalid time pattern"
    };

  let debug_string_of_weekdays = (days: list(Time.weekday)): string => {
    let aux = l =>
      String.concat(
        ",",
        List.map(Time.To_string.abbreviated_string_of_weekday, l),
      );

    Printf.sprintf("weekday [%s]", aux(days));
  };

  let debug_string_of_month_days = (days: list(int)): string => {
    let aux = l => String.concat(",", List.map(string_of_int, l));
    Printf.sprintf("month day [%s]", aux(days));
  };

  let debug_string_of_time_pattern =
      (~indent_level=0, ~buffer=Buffer.create(4096), t: time_pattern): string => {
    let aux = l => String.concat(",", List.map(string_of_int, l));
    let aux_months = l =>
      String.concat(
        ",",
        List.map(Time.To_string.abbreviated_string_of_month, l),
      );

    Debug_print.bprintf(~indent_level, buffer, "time pattern :\n");
    Debug_print.bprintf(
      ~indent_level=indent_level + 1,
      buffer,
      "years : [%s]\n",
      aux(t.years),
    );
    Debug_print.bprintf(
      ~indent_level=indent_level + 1,
      buffer,
      "months : [%s]\n",
      aux_months(t.months),
    );
    Debug_print.bprintf(
      ~indent_level=indent_level + 1,
      buffer,
      "month days : %s\n",
      debug_string_of_month_days(t.month_days),
    );
    Debug_print.bprintf(
      ~indent_level=indent_level + 1,
      buffer,
      "weekdays : %s\n",
      debug_string_of_weekdays(t.weekdays),
    );
    Debug_print.bprintf(
      ~indent_level=indent_level + 1,
      buffer,
      "hours : [%s]\n",
      aux(t.hours),
    );
    Debug_print.bprintf(
      ~indent_level=indent_level + 1,
      buffer,
      "minutes : [%s]\n",
      aux(t.minutes),
    );
    Debug_print.bprintf(
      ~indent_level=indent_level + 1,
      buffer,
      "seconds : [%s]\n",
      aux(t.seconds),
    );
    Buffer.contents(buffer);
  };

  let debug_string_of_time_range_pattern =
      (~indent_level=0, ~buffer=Buffer.create(4096), t: time_range_pattern)
      : string => {
    switch (t) {
    | `Range_inc(t1, t2) =>
      Debug_print.bprintf(
        ~indent_level,
        buffer,
        "time range pattern inclusive:\n",
      );
      debug_string_of_time_pattern(
        ~indent_level=indent_level + 1,
        ~buffer,
        t1,
      )
      |> ignore;
      debug_string_of_time_pattern(
        ~indent_level=indent_level + 1,
        ~buffer,
        t2,
      )
      |> ignore;
    | `Range_exc(t1, t2) =>
      Debug_print.bprintf(
        ~indent_level,
        buffer,
        "time range pattern exclusive:\n",
      );
      debug_string_of_time_pattern(
        ~indent_level=indent_level + 1,
        ~buffer,
        t1,
      )
      |> ignore;
      debug_string_of_time_pattern(
        ~indent_level=indent_level + 1,
        ~buffer,
        t2,
      )
      |> ignore;
    };
    Buffer.contents(buffer);
  };

  let debug_string_of_single_or_ranges =
      (~indent_level=0, ~buffer=Buffer.create(4096), t: single_or_ranges)
      : string =>
    switch (t) {
    | Single_time_pattern(t) =>
      debug_string_of_time_pattern(~indent_level, ~buffer, t)
    | Time_range_patterns(l) =>
      List.iter(
        t =>
          debug_string_of_time_range_pattern(~indent_level, ~buffer, t)
          |> ignore,
        l,
      );
      Buffer.contents(buffer);
    };
};

module Print = {
  let debug_print_time_pattern = (~indent_level=0, t) =>
    print_string(To_string.debug_string_of_time_pattern(~indent_level, t));

  let debug_print_time_range_pattern = (~indent_level=0, t) =>
    print_string(
      To_string.debug_string_of_time_range_pattern(~indent_level, t),
    );

  let debug_print_single_or_ranges = (~indent_level=0, t) =>
    print_string(
      To_string.debug_string_of_single_or_ranges(~indent_level, t),
    );
};
