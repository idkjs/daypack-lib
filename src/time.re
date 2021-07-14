type tz_offset_s = int;

let tz_offset_s_utc = 0;

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

let first_mday = 1;

let tm_year_offset = 1900;

module Int64_multipliers = {
  let minute_to_seconds = 60L;

  let hour_to_seconds = Int64.mul(60L, minute_to_seconds);

  let day_to_seconds = Int64.mul(24L, hour_to_seconds);
};

module Float_multipliers = {
  let minute_to_seconds = Int64.to_float(Int64_multipliers.minute_to_seconds);

  let hour_to_seconds = Int64.to_float(Int64_multipliers.hour_to_seconds);

  let day_to_seconds = Int64.to_float(Int64_multipliers.day_to_seconds);
};

let resolve_current_tz_offset_s = (x: option(tz_offset_s)): tz_offset_s =>
  Option.value(~default=0, x);

let next_weekday = (wday: weekday): weekday =>
  switch (wday) {
  | `Sun => `Mon
  | `Mon => `Tue
  | `Tue => `Wed
  | `Wed => `Thu
  | `Thu => `Fri
  | `Fri => `Sat
  | `Sat => `Sun
  };

let tm_int_of_weekday = (wday: weekday): int =>
  switch (wday) {
  | `Sun => 0
  | `Mon => 1
  | `Tue => 2
  | `Wed => 3
  | `Thu => 4
  | `Fri => 5
  | `Sat => 6
  };

let weekday_of_tm_int = (x: int): result(weekday, unit) =>
  switch (x) {
  | 0 => Ok(`Sun)
  | 1 => Ok(`Mon)
  | 2 => Ok(`Tue)
  | 3 => Ok(`Wed)
  | 4 => Ok(`Thu)
  | 5 => Ok(`Fri)
  | 6 => Ok(`Sat)
  | _ => Error()
  };

let tm_int_of_month = (month: month): int =>
  switch (month) {
  | `Jan => 0
  | `Feb => 1
  | `Mar => 2
  | `Apr => 3
  | `May => 4
  | `Jun => 5
  | `Jul => 6
  | `Aug => 7
  | `Sep => 8
  | `Oct => 9
  | `Nov => 10
  | `Dec => 11
  };

let month_of_tm_int = (x: int): result(month, unit) =>
  switch (x) {
  | 0 => Ok(`Jan)
  | 1 => Ok(`Feb)
  | 2 => Ok(`Mar)
  | 3 => Ok(`Apr)
  | 4 => Ok(`May)
  | 5 => Ok(`Jun)
  | 6 => Ok(`Jul)
  | 7 => Ok(`Aug)
  | 8 => Ok(`Sep)
  | 9 => Ok(`Oct)
  | 10 => Ok(`Nov)
  | 11 => Ok(`Dec)
  | _ => Error()
  };

let human_int_of_month = (month: month): int => tm_int_of_month(month) + 1;

let month_of_human_int = (x: int): result(month, unit) =>
  month_of_tm_int(x - 1);

let compare_month = (m1: month, m2: month): int =>
  compare(tm_int_of_month(m1), tm_int_of_month(m2));

let month_lt = (m1, m2) => tm_int_of_month(m1) < tm_int_of_month(m2);

let month_le = (m1, m2) => tm_int_of_month(m1) <= tm_int_of_month(m2);

let month_gt = (m1, m2) => tm_int_of_month(m1) > tm_int_of_month(m2);

let month_ge = (m1, m2) => tm_int_of_month(m1) >= tm_int_of_month(m2);

let compare_weekday = (d1: weekday, d2: weekday): int =>
  compare(tm_int_of_weekday(d1), tm_int_of_weekday(d2));

let weekday_lt = (d1, d2) => tm_int_of_weekday(d1) < tm_int_of_weekday(d2);

let weekday_le = (d1, d2) => tm_int_of_weekday(d1) <= tm_int_of_weekday(d2);

let weekday_gt = (d1, d2) => tm_int_of_weekday(d1) > tm_int_of_weekday(d2);

let weekday_ge = (d1, d2) => tm_int_of_weekday(d1) >= tm_int_of_weekday(d2);

let zero_tm_sec = tm => Unix.{...tm, tm_sec: 0};

/* let tm_of_date_time (x : date_time) : Unix.tm =
   {
    tm_sec = x.second;
    tm_min = x.minute;
    tm_hour = x.hour;
    tm_mday = x.day;
    tm_mon = tm_int_of_month x.month;
    tm_year = x.year;
    tm_wday = 0;
    tm_yday = 0;
    tm_isdst = false;
   } */

/* let tm_of_unix_second ~(time_zone_of_tm : time_zone) (time : int64) : (Unix.tm, unit) result =
   let time = Int64.to_float time in
   match time_zone_of_tm with
   | `Local -> Ok (Unix.localtime time)
   | `UTC -> Ok (Unix.gmtime time)
   | `UTC_plus_sec tz_offset_s ->
      match Ptime.of_float_s time with
      | None -> Error ()
      | Ok x ->
          x
      |> Ptime.to_date_time ~tz_offset_s
      |> date_time_of_ptime_date_time

    let date_time = Ptime.of_float_s time in
    CalendarLib.Calendar.convert date_time CalendarLib.Time_Zone.UTC
      CalendarLib.Time_Zone.(UTC_Plus x)
    |> CalendarLib.Calendar.to_unixtm

   let unix_second_of_tm ~(time_zone_of_tm : time_zone) (tm : Unix.tm) : int64 =
   tm
   |> (fun x ->
      match time_zone_of_tm with
      | `Local ->
        let time, _ = Unix.mktime tm in
        time
      | `UTC ->
        x
        |> CalendarLib.Calendar.from_unixtm
        |> CalendarLib.Calendar.from_gmt
        |> CalendarLib.Calendar.to_unixfloat
      | `UTC_plus _ ->
        let date_time = CalendarLib.Calendar.from_unixtm tm in
        let tz = cal_time_zone_of_time_zone time_zone_of_tm in
        CalendarLib.Calendar.convert date_time tz CalendarLib.Time_Zone.UTC
        |> CalendarLib.Calendar.to_unixfloat)
   |> fun time -> time |> Int64.of_float */

/* let normalize_tm tm =
   tm
   |> zero_tm_sec
   |> CalendarLib.Calendar.from_unixtm
   |> CalendarLib.Calendar.to_unixtm

   let tm_change_time_zone ~(from_time_zone : time_zone)
    ~(to_time_zone : time_zone) (tm : Unix.tm) : Unix.tm =
   if from_time_zone = to_time_zone then tm
   else
    let time = unix_second_of_tm ~time_zone_of_tm:from_time_zone tm in
    tm_of_unix_second ~time_zone_of_tm:to_time_zone time */

let is_leap_year = (~year) => {
  assert(year >= 0);
  let divisible_by_4 = year mod 4 == 0;
  let divisible_by_100 = year mod 100 == 0;
  let divisible_by_400 = year mod 400 == 0;
  divisible_by_4 && (!divisible_by_100 || divisible_by_400);
};

let day_count_of_year = (~year) =>
  if (is_leap_year(~year)) {
    366;
  } else {
    365;
  };

let day_count_of_month = (~year, ~month: month) =>
  switch (month) {
  | `Jan => 31
  | `Feb =>
    if (is_leap_year(~year)) {
      29;
    } else {
      28;
    }
  | `Mar => 31
  | `Apr => 30
  | `May => 31
  | `Jun => 30
  | `Jul => 31
  | `Aug => 31
  | `Sep => 30
  | `Oct => 31
  | `Nov => 30
  | `Dec => 31
  };

let weekday_of_month_day =
    (~year: int, ~month: month, ~mday: int): result(weekday, unit) =>
  switch (Ptime.(of_date((year, human_int_of_month(month), mday)))) {
  | None => Error()
  | Some(wday) => Ok(Ptime.weekday(wday))
  };

/* let local_tm_to_utc_tm (tm : Unix.tm) : Unix.tm =
   let timestamp, _ = Unix.mktime tm in
   Unix.gmtime timestamp */

module Second_ranges =
  Ranges_small.Make({
    type t = int;

    let modulo = None;

    let to_int = x => x;

    let of_int = x => x;
  });

module Minute_ranges =
  Ranges_small.Make({
    type t = int;

    let modulo = None;

    let to_int = x => x;

    let of_int = x => x;
  });

module Hour_ranges =
  Ranges_small.Make({
    type t = int;

    let modulo = None;

    let to_int = x => x;

    let of_int = x => x;
  });

module Weekday_tm_int_ranges =
  Ranges_small.Make({
    type t = int;

    let modulo = Some(7);

    let to_int = x => x;

    let of_int = x => x;
  });

module Weekday_ranges =
  Ranges_small.Make({
    type t = weekday;

    let modulo = Some(7);

    let to_int = tm_int_of_weekday;

    let of_int = x => x |> weekday_of_tm_int |> Result.get_ok;
  });

module Month_day_ranges =
  Ranges_small.Make({
    type t = int;

    let modulo = None;

    let to_int = x => x;

    let of_int = x => x;
  });

module Month_tm_int_ranges =
  Ranges_small.Make({
    type t = int;

    let modulo = None;

    let to_int = x => x;

    let of_int = x => x;
  });

module Month_ranges =
  Ranges_small.Make({
    type t = month;

    let modulo = None;

    let to_int = human_int_of_month;

    let of_int = x => x |> month_of_human_int |> Result.get_ok;
  });

module Year_ranges =
  Ranges_small.Make({
    type t = int;

    let modulo = None;

    let to_int = x => x;

    let of_int = x => x;
  });

module Date_time = {
  type t = {
    year: int,
    month,
    day: int,
    hour: int,
    minute: int,
    second: int,
    tz_offset_s: int,
  };

  let to_ptime_date_time = (x: t): (Ptime.date, Ptime.time) => (
    (x.year, human_int_of_month(x.month), x.day),
    ((x.hour, x.minute, x.second), x.tz_offset_s),
  );

  let of_ptime_date_time =
      (
        ((year, month, day), ((hour, minute, second), tz_offset_s)): (
          Ptime.date,
          Ptime.time,
        ),
      )
      : result(t, unit) =>
    switch (month_of_human_int(month)) {
    | Ok(month) => Ok({year, month, day, hour, minute, second, tz_offset_s})
    | Error () => Error()
    };

  let to_unix_second = (x: t): result(int64, unit) =>
    switch (Ptime.of_date_time(to_ptime_date_time(x))) {
    | None => Error()
    | Some(x) => x |> Ptime.to_float_s |> Int64.of_float |> Result.ok
    };

  let of_unix_second =
      (~tz_offset_s_of_date_time: option(tz_offset_s), x: int64)
      : result(t, unit) =>
    switch (Ptime.of_float_s(Int64.to_float(x))) {
    | None => Error()
    | Some(x) =>
      let tz_offset_s = resolve_current_tz_offset_s(tz_offset_s_of_date_time);

      x |> Ptime.to_date_time(~tz_offset_s) |> of_ptime_date_time;
    };

  let min =
    Ptime.min |> Ptime.to_date_time |> of_ptime_date_time |> Result.get_ok;

  let max =
    Ptime.max |> Ptime.to_date_time |> of_ptime_date_time |> Result.get_ok;

  let compare = (x: t, y: t): int =>
    switch (compare(x.year, y.year)) {
    | 0 =>
      switch (
        compare(human_int_of_month(x.month), human_int_of_month(y.month))
      ) {
      | 0 =>
        switch (compare(x.day, y.day)) {
        | 0 =>
          switch (compare(x.hour, y.hour)) {
          | 0 =>
            switch (compare(x.minute, y.minute)) {
            | 0 => compare(x.second, y.second)
            | n => n
            }
          | n => n
          }
        | n => n
        }
      | n => n
      }
    | n => n
    };

  let set_to_first_sec = (x: t): t => {...x, second: 0};

  let set_to_last_sec = (x: t): t => {...x, second: 59};

  let set_to_first_min_sec = (x: t): t =>
    {...x, minute: 0} |> set_to_first_sec;

  let set_to_last_min_sec = (x: t): t =>
    {...x, minute: 59} |> set_to_last_sec;

  let set_to_first_hour_min_sec = (x: t): t =>
    {...x, hour: 0} |> set_to_first_min_sec;

  let set_to_last_hour_min_sec = (x: t): t =>
    {...x, hour: 23} |> set_to_last_min_sec;

  let set_to_first_day_hour_min_sec = (x: t): t =>
    {...x, day: 1} |> set_to_first_hour_min_sec;

  let set_to_last_day_hour_min_sec = (x: t): t =>
    {...x, day: day_count_of_month(~year=x.year, ~month=x.month)}
    |> set_to_last_hour_min_sec;

  let set_to_first_month_day_hour_min_sec = (x: t): t =>
    {...x, month: `Jan} |> set_to_first_day_hour_min_sec;

  let set_to_last_month_day_hour_min_sec = (x: t): t =>
    {...x, month: `Dec} |> set_to_last_day_hour_min_sec;
};

module Check = {
  let unix_second_is_valid = (x: int64): bool =>
    switch (Date_time.of_unix_second(~tz_offset_s_of_date_time=None, x)) {
    | Ok(_) => true
    | Error () => false
    };

  let second_is_valid = (~second: int): bool => 0 <= second && second < 60;

  let minute_second_is_valid = (~minute: int, ~second: int): bool =>
    0 <= minute && minute < 60 && second_is_valid(~second);

  let hour_minute_second_is_valid =
      (~hour: int, ~minute: int, ~second: int): bool =>
    (0 <= hour && hour < 24) && minute_second_is_valid(~minute, ~second);

  let date_time_is_valid = (x: Date_time.t): bool =>
    switch (Date_time.to_unix_second(x)) {
    | Ok(_) => true
    | Error () => false
    };
};

let next_hour_minute = (~hour: int, ~minute: int): result((int, int), unit) =>
  if (Check.hour_minute_second_is_valid(~hour, ~minute, ~second=0)) {
    if (minute < 59) {
      [@implicit_arity] Ok(hour, succ(minute));
    } else {
      [@implicit_arity] Ok(succ(hour) mod 24, 0);
    };
  } else {
    Error();
  };

module Current = {
  let cur_unix_second = (): int64 => Unix.time() |> Int64.of_float;

  let cur_date_time = (~tz_offset_s_of_date_time): result(Date_time.t, unit) =>
    cur_unix_second() |> Date_time.of_unix_second(~tz_offset_s_of_date_time);

  let cur_tm_local = (): Unix.tm => Unix.time() |> Unix.localtime;

  let cur_tm_utc = (): Unix.tm => Unix.time() |> Unix.gmtime;
};

module Of_string = {
  let weekdays: list((string, weekday)) = (
    [
      ("sunday", `Sun),
      ("monday", `Mon),
      ("tuesday", `Tue),
      ("wednesday", `Wed),
      ("thursday", `Thu),
      ("friday", `Fri),
      ("saturday", `Sat),
    ]:
      list((string, weekday))
  );

  let months: list((string, month)) = (
    [
      ("january", `Jan),
      ("february", `Feb),
      ("march", `Mar),
      ("april", `Apr),
      ("may", `May),
      ("june", `Jun),
      ("july", `Jul),
      ("august", `Aug),
      ("september", `Sep),
      ("october", `Oct),
      ("november", `Nov),
      ("december", `Dec),
    ]:
      list((string, month))
  );

  let weekday_of_string = (s: string): result(weekday, unit) =>
    switch (Misc_utils.prefix_string_match(weekdays, s)) {
    | [(_, x)] => Ok(x)
    | _ => Error()
    };

  let month_of_string = (s: string): result(month, unit) =>
    switch (Misc_utils.prefix_string_match(months, s)) {
    | [(_, x)] => Ok(x)
    | _ => Error()
    };
};

module Add = {
  let add_days_unix_second = (~days: int, x: int64): int64 =>
    Int64.add(
      Int64.mul(Int64.of_int(days), Int64_multipliers.day_to_seconds),
      x,
    );
};

module Serialize = {
  let pack_weekday = (x: weekday): Time_t.weekday => x;

  let pack_month = (x: month): Time_t.month => x;
};

module Deserialize = {
  let unpack_weekday = (x: Time_t.weekday): weekday => x;

  let unpack_month = (x: Time_t.month): month => x;
};

module To_string = {
  type case =
    | Upper
    | Lower;

  type size_and_casing =
    | Abbreviated(case, case, case)
    | Full(case, case);

  let map_char_to_case = (case: case, c: char) =>
    switch (case) {
    | Upper => Char.uppercase_ascii(c)
    | Lower => Char.lowercase_ascii(c)
    };

  let map_string_to_size_and_casing = (x: size_and_casing, s: string): string =>
    switch (x) {
    | [@implicit_arity] Abbreviated(case1, case2, case3) =>
      let c1 = map_char_to_case(case1, s.[0]);
      let c2 = map_char_to_case(case2, s.[1]);
      let c3 = map_char_to_case(case3, s.[2]);
      Printf.sprintf("%c%c%c", c1, c2, c3);
    | [@implicit_arity] Full(case1, case2) =>
      String.mapi(
        (i, c) =>
          if (i == 0) {
            map_char_to_case(case1, c);
          } else {
            map_char_to_case(case2, c);
          },
        s,
      )
    };

  let pad_int = (c: option(char), x: int): string =>
    switch (c) {
    | None => string_of_int(x)
    | Some(c) =>
      if (x < 10) {
        Printf.sprintf("%c%d", c, x);
      } else {
        string_of_int(x);
      }
    };

  let full_string_of_weekday = (wday: weekday): string =>
    switch (wday) {
    | `Sun => "Sunday"
    | `Mon => "Monday"
    | `Tue => "Tuesday"
    | `Wed => "Wednesday"
    | `Thu => "Thursday"
    | `Fri => "Friday"
    | `Sat => "Saturday"
    };

  let abbreviated_string_of_weekday = (wday: weekday): string =>
    String.sub(full_string_of_weekday(wday), 0, 3);

  let full_string_of_month = (month: month): string =>
    switch (month) {
    | `Jan => "January"
    | `Feb => "February"
    | `Mar => "March"
    | `Apr => "April"
    | `May => "May"
    | `Jun => "June"
    | `Jul => "July"
    | `Aug => "August"
    | `Sep => "September"
    | `Oct => "October"
    | `Nov => "November"
    | `Dec => "December"
    };

  let abbreviated_string_of_month = (month: month): string =>
    String.sub(full_string_of_month(month), 0, 3);

  let yyyymondd_hhmmss_string_of_tm = (tm: Unix.tm): result(string, unit) =>
    switch (month_of_tm_int(tm.tm_mon)) {
    | Ok(mon) =>
      let mon = abbreviated_string_of_month(mon);
      Ok(
        Printf.sprintf(
          "%04d %s %02d %02d:%02d:%02d",
          tm.tm_year + tm_year_offset,
          mon,
          tm.tm_mday,
          tm.tm_hour,
          tm.tm_min,
          tm.tm_sec,
        ),
      );
    | Error () => Error()
    };

  let yyyymondd_hhmmss_string_of_date_time = (x: Date_time.t): string => {
    let mon = abbreviated_string_of_month(x.month);
    Printf.sprintf(
      "%04d %s %02d %02d:%02d:%02d",
      x.year,
      mon,
      x.day,
      x.hour,
      x.minute,
      x.second,
    );
  };

  let yyyymondd_hhmmss_string_of_unix_second =
      (~display_using_tz_offset_s: option(tz_offset_s), time: int64)
      : result(string, unit) =>
    Date_time.of_unix_second(
      ~tz_offset_s_of_date_time=display_using_tz_offset_s,
      time,
    )
    |> Result.map(yyyymondd_hhmmss_string_of_date_time);

  /* let yyyymmdd_hhmmss_string_of_tm (tm : Unix.tm) : (string, unit) result =
     match month_of_tm_int tm.tm_mon with
     | Ok mon ->
       let mon = human_int_of_month mon in
       Ok
         (Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
            (tm.tm_year + tm_year_offset)
            mon tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec)
     | Error () -> Error () */

  let yyyymmdd_hhmmss_string_of_date_time = (x: Date_time.t): string => {
    let mon = human_int_of_month(x.month);
    Printf.sprintf(
      "%04d-%02d-%02d %02d:%02d:%02d",
      x.year,
      mon,
      x.day,
      x.hour,
      x.minute,
      x.second,
    );
  };

  let yyyymmdd_hhmmss_string_of_unix_second =
      (~display_using_tz_offset_s: option(tz_offset_s), time: int64)
      : result(string, unit) =>
    Date_time.of_unix_second(
      ~tz_offset_s_of_date_time=display_using_tz_offset_s,
      time,
    )
    |> Result.map(yyyymmdd_hhmmss_string_of_date_time);

  /*let yyyymondd_hhmm_string_of_tm (tm : Unix.tm) : (string, unit) result =
      match month_of_tm_int tm.tm_mon with
      | Ok mon ->
        let mon = string_of_month mon in
        Ok
          (Printf.sprintf "%04d %s %02d %02d:%02d"
             (tm.tm_year + tm_year_offset)
             mon tm.tm_mday tm.tm_hour tm.tm_min)
      | Error () -> Error ()
    */

  let yyyymondd_hhmm_string_of_date_time = (x: Date_time.t): string => {
    let mon = abbreviated_string_of_month(x.month);
    Printf.sprintf(
      "%04d %s %02d %02d:%02d",
      x.year,
      mon,
      x.day,
      x.hour,
      x.minute,
    );
  };

  let yyyymondd_hhmm_string_of_unix_second =
      (~display_using_tz_offset_s: option(tz_offset_s), time: int64)
      : result(string, unit) =>
    Date_time.of_unix_second(
      ~tz_offset_s_of_date_time=display_using_tz_offset_s,
      time,
    )
    |> Result.map(yyyymondd_hhmm_string_of_date_time);

  /* let yyyymmdd_hhmm_string_of_tm (tm : Unix.tm) : (string, unit) result =
     match month_of_tm_int tm.tm_mon with
     | Ok mon ->
       let mon = human_int_of_month mon in
       Ok
         (Printf.sprintf "%04d-%02d-%02d %02d:%02d"
            (tm.tm_year + tm_year_offset)
            mon tm.tm_mday tm.tm_hour tm.tm_min)
     | Error () -> Error () */

  let yyyymmdd_hhmm_string_of_date_time = (x: Date_time.t): string => {
    let mon = human_int_of_month(x.month);
    Printf.sprintf(
      "%04d-%02d-%02d %02d:%02d",
      x.year,
      mon,
      x.day,
      x.hour,
      x.minute,
    );
  };

  let yyyymmdd_hhmm_string_of_unix_second =
      (~display_using_tz_offset_s: option(tz_offset_s), time: int64)
      : result(string, unit) =>
    Date_time.of_unix_second(
      ~tz_offset_s_of_date_time=display_using_tz_offset_s,
      time,
    )
    |> Result.map(yyyymmdd_hhmm_string_of_date_time);

  module Format_string_parsers = {
    open MParser;

    let case: t(case, unit) = (
      attempt(char('x') >> return(Lower)) <|> (char('X') >> return(Upper)):
        t(case, unit)
    );

    let size_and_casing: t(size_and_casing, unit) = (
      case
      >>= (
        c1 =>
          case
          >>= (
            c2 =>
              attempt(char('*') >> return([@implicit_arity] Full(c1, c2)))
              <|> (
                case
                >>= (c3 => return([@implicit_arity] Abbreviated(c1, c2, c3)))
              )
          )
      ):
        t(size_and_casing, unit)
    );

    let padding: t(option(char), unit) = (
      attempt(
        satisfy(_ => true)
        >>= (padding => char('X') >> return(Some(padding))),
      )
      <|> (char('X') >> return(None)):
        t(option(char), unit)
    );

    let inner = (date_time: Date_time.t): t(string, unit) =>
      choice([
        attempt(string("year")) >> return(string_of_int(date_time.year)),
        attempt(string("mon:"))
        >> size_and_casing
        >>= (
          x =>
            return(
              map_string_to_size_and_casing(
                x,
                full_string_of_month(date_time.month),
              ),
            )
        ),
        attempt(string("mday:"))
        >> padding
        >>= (padding => return(pad_int(padding, date_time.day))),
        attempt(string("wday:"))
        >> size_and_casing
        >>= (
          x =>
            switch (
              weekday_of_month_day(
                ~year=date_time.year,
                ~month=date_time.month,
                ~mday=date_time.day,
              )
            ) {
            | Error () => fail("Invalid date time")
            | Ok(wday) =>
              return(
                map_string_to_size_and_casing(
                  x,
                  full_string_of_weekday(wday),
                ),
              )
            }
        ),
        attempt(
          string("hour:")
          >> padding
          >>= (padding => return(pad_int(padding, date_time.hour))),
        ),
        attempt(
          string("12hour:")
          >> padding
          >>= (
            padding => {
              let hour =
                if (date_time.hour == 0) {
                  12;
                } else {
                  date_time.hour mod 12;
                };

              return(pad_int(padding, hour));
            }
          ),
        ),
        attempt(
          string("min:")
          >> padding
          >>= (padding => return(pad_int(padding, date_time.minute))),
        ),
        attempt(
          string("sec:")
          >> padding
          >>= (padding => return(pad_int(padding, date_time.second))),
        ),
        string("unix")
        >> (
          switch (Date_time.to_unix_second(date_time)) {
          | Error () => fail("Invalid date time")
          | Ok(sec) => return(Int64.to_string(sec))
          }
        ),
      ]);
  };

  let string_of_date_time =
      (~format: string, x: Date_time.t): result(string, string) => {
    open MParser;
    open Parser_components;
    let single = (date_time: Date_time.t): t(string, unit) =>
      choice([
        attempt(string("{{") >> return("{")),
        attempt(char('{'))
        >> Format_string_parsers.inner(date_time)
        << char('}'),
        many1_satisfy(
          fun
          | '{' => false
          | _ => true,
        )
        |>> (s => s),
      ]);

    let p = (date_time: Date_time.t): t(list(string), unit) =>
      many(single(date_time));

    parse_string(p(x) << eof, format, ())
    |> result_of_mparser_result
    |> Result.map(l => String.concat("", l));
  };

  let string_of_unix_second =
      (~format, ~display_using_tz_offset_s: option(tz_offset_s), time: int64)
      : result(string, string) =>
    switch (
      Date_time.of_unix_second(
        ~tz_offset_s_of_date_time=display_using_tz_offset_s,
        time,
      )
    ) {
    | Error () => Error("Invalid unix second")
    | Ok(dt) => string_of_date_time(~format, dt)
    };

  let string_of_time_slot =
      (
        ~format: string,
        ~display_using_tz_offset_s: option(tz_offset_s),
        (s, e): Time_slot.t,
      )
      : result(string, string) => {
    open MParser;
    open Parser_components;
    let single =
        (start_date_time: Date_time.t, end_date_time: Date_time.t)
        : t(string, unit) =>
      choice([
        attempt(string("{{") >> return("{")),
        attempt(char('{'))
        >> (
          attempt(char('s') >> return(start_date_time))
          <|> (char('e') >> return(end_date_time))
        )
        >>= (
          date_time => Format_string_parsers.inner(date_time) << char('}')
        ),
        many1_satisfy(
          fun
          | '{' => false
          | _ => true,
        )
        >>= (s => return(s)),
      ]);

    let p =
        (start_date_time: Date_time.t, end_date_time: Date_time.t)
        : t(list(string), unit) =>
      many(single(start_date_time, end_date_time));

    switch (
      Date_time.of_unix_second(
        ~tz_offset_s_of_date_time=display_using_tz_offset_s,
        s,
      )
    ) {
    | Error () => Error("Invalid start unix time")
    | Ok(s) =>
      switch (
        Date_time.of_unix_second(
          ~tz_offset_s_of_date_time=display_using_tz_offset_s,
          e,
        )
      ) {
      | Error () => Error("Invalid end unix time")
      | Ok(e) =>
        parse_string(
          p(s, e)
          >>= (
            s =>
              get_pos
              >>= (
                pos =>
                  attempt(eof)
                  >> return(s)
                  <|> fail(
                        Printf.sprintf(
                          "Expected EOI, pos: %s",
                          string_of_pos(pos),
                        ),
                      )
              )
          ),
          format,
          (),
        )
        |> result_of_mparser_result
        |> Result.map(l => String.concat("", l))
      }
    };
  };

  let debug_string_of_time =
      (
        ~indent_level=0,
        ~buffer=Buffer.create(4096),
        ~display_using_tz_offset_s: option(tz_offset_s),
        time: int64,
      )
      : string => {
    switch (
      yyyymondd_hhmmss_string_of_unix_second(~display_using_tz_offset_s, time)
    ) {
    | Error () => Debug_print.bprintf(~indent_level, buffer, "Invalid time\n")
    | Ok(s) => Debug_print.bprintf(~indent_level, buffer, "%s\n", s)
    };
    Buffer.contents(buffer);
  };
};

module Print = {
  let debug_print_time =
      (
        ~indent_level=0,
        ~display_using_tz_offset_s: option(tz_offset_s),
        time: int64,
      )
      : unit =>
    print_string(
      To_string.debug_string_of_time(
        ~indent_level,
        ~display_using_tz_offset_s,
        time,
      ),
    );
};

module Date_time_set =
  Set.Make({
    type t = Date_time.t;

    let compare = Date_time.compare;
  });
