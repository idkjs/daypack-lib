type error =
  | Invalid_time_point_expr
  | Invalid_time_slot_expr;

type f_resolve_tse_name = string => option(Time_expr_ast.time_slot_expr);

type f_resolve_tpe_name = string => option(Time_expr_ast.time_point_expr);

type lang_fragment = [
  | `Time_point_expr
  | `Time_slot_expr
  | `Branching_time_slot_expr
  | `Time_pattern
];

let all_lang_fragments = [
  `Time_point_expr,
  `Time_slot_expr,
  `Branching_time_slot_expr,
  `Time_pattern,
];

let default_f_resolve_tse_name =
    (_: string): option(Time_expr_ast.time_slot_expr) =>
  None;

let default_f_resolve_tpe_name =
    (_: string): option(Time_expr_ast.time_point_expr) =>
  None;

module To_string = {
  let debug_string_of_hms_ranges =
      ({hour, minute, second}: Time_expr_ast.hms_expr): string =>
    Printf.sprintf("%02d:%02d:%02d", hour, minute, second);
};

exception Invalid_time_expr(string);

let max_resolve_depth = 1000;

module Resolve = {
  let resolve_time_point_expr =
      (
        ~f_resolve_tpe_name: f_resolve_tpe_name,
        e: Time_expr_ast.time_point_expr,
      )
      : result(Time_expr_ast.time_point_expr, string) => {
    let rec aux = (f_resolve_tpe_name, remaining_resolve_depth, name, e) =>
      if (remaining_resolve_depth <= 0) {
        Error("Maximum resolve depth reached");
      } else {
        switch (e) {
        | Time_expr_ast.Tpe_name(s) =>
          if (name == Some(s)) {
            Error(
              Printf.sprintf("Name resolution loop detected for name: %s", s),
            );
          } else {
            let name =
              switch (name) {
              | None => Some(s)
              | Some(x) => Some(x)
              };
            switch (f_resolve_tpe_name(s)) {
            | None =>
              Error(Printf.sprintf("Name resolution failed for name: %s", s))
            | Some(e) =>
              aux(f_resolve_tpe_name, pred(remaining_resolve_depth), name, e)
            };
          }
        | e => Ok(e)
        };
      };

    aux(f_resolve_tpe_name, max_resolve_depth, None, e);
  };

  let resolve_time_slot_expr =
      (
        ~f_resolve_tse_name: f_resolve_tse_name,
        ~f_resolve_tpe_name: f_resolve_tpe_name,
        e: Time_expr_ast.time_slot_expr,
      )
      : result(Time_expr_ast.time_slot_expr, string) => {
    let rec aux =
            (
              f_resolve_tse_name,
              f_resolve_tpe_name,
              remaining_resolve_depth,
              name,
              e,
            ) =>
      if (remaining_resolve_depth <= 0) {
        Error("Maximum resolve depth reached");
      } else {
        switch (e) {
        | Time_expr_ast.Tse_name(s) =>
          if (name == Some(s)) {
            Error(
              Printf.sprintf("Name resolution loop detected for name: %s", s),
            );
          } else {
            let name =
              switch (name) {
              | None => Some(s)
              | Some(x) => Some(x)
              };
            switch (f_resolve_tse_name(s)) {
            | None =>
              Error(Printf.sprintf("Name resolution failed for name: %s", s))
            | Some(e) =>
              aux(
                f_resolve_tse_name,
                f_resolve_tpe_name,
                pred(remaining_resolve_depth),
                name,
                e,
              )
            };
          }
        | [@implicit_arity] Explicit_time_slot(start, end_exc) =>
          try(
            Ok(
              Time_expr_ast.Explicit_time_slot(
                switch (resolve_time_point_expr(~f_resolve_tpe_name, start)) {
                | Error(msg) => raise(Invalid_time_expr(msg))
                | Ok(start) =>
                  switch (
                    resolve_time_point_expr(~f_resolve_tpe_name, end_exc)
                  ) {
                  | Error(msg) => raise(Invalid_time_expr(msg))
                  | Ok(end_exc) => (start, end_exc)
                  }
                },
              ),
            )
          ) {
          | Invalid_time_expr(msg) => Error(msg)
          }
        };
      };

    aux(f_resolve_tse_name, f_resolve_tpe_name, max_resolve_depth, None, e);
  };
};

module Check = {
  let day_expr_is_valid = (e: Time_expr_ast.day_expr): bool =>
    switch (e) {
    | Time_expr_ast.Weekday(_) => true
    | Month_day(x) => 1 <= x && x <= 31
    };

  let hms_ranges_are_valid = (l: list(Time_expr_ast.hms_range_expr)) =>
    Time_expr_ast.(
      List.for_all(
        x =>
          switch (x) {
          | `Range_inc(x, y)
          | `Range_exc(x, y) =>
            Time.Check.hour_minute_second_is_valid(
              ~hour=x.hour,
              ~minute=x.minute,
              ~second=x.second,
            )
            && Time.Check.hour_minute_second_is_valid(
                 ~hour=y.hour,
                 ~minute=y.minute,
                 ~second=y.second,
               )
          },
        l,
      )
    );

  let check_time_point_expr =
      (e: Time_expr_ast.time_point_expr): result(unit, unit) =>
    Time_expr_ast.(
      switch (e) {
      | Tpe_name(_) => Ok()
      | Tpe_unix_seconds(l) =>
        let invalid_unix_seconds =
          List.filter(
            x =>
              Result.is_error(
                Time.Date_time.of_unix_second(
                  ~tz_offset_s_of_date_time=None,
                  x,
                ),
              ),
            l,
          );

        switch (invalid_unix_seconds) {
        | [] => Ok()
        | _ => Error()
        };
      | Second(second) =>
        if (Time.Check.second_is_valid(~second)) {
          Ok();
        } else {
          Error();
        }
      | Minute_second({minute, second}) =>
        if (Time.Check.minute_second_is_valid(~minute, ~second)) {
          Ok();
        } else {
          Error();
        }
      | Hms({hour, minute, second}) =>
        if (Time.Check.hour_minute_second_is_valid(~hour, ~minute, ~second)) {
          Ok();
        } else {
          Error();
        }
      | Day_hms({day, hms: {hour, minute, second}}) =>
        if (day_expr_is_valid(day)
            && Time.Check.hour_minute_second_is_valid(~hour, ~minute, ~second)) {
          Ok();
        } else {
          Error();
        }
      | Month_day_hms({month: _, month_day, hms: {hour, minute, second}}) =>
        if (1 <= month_day
            && month_day <= 31
            && Time.Check.hour_minute_second_is_valid(~hour, ~minute, ~second)) {
          Ok();
        } else {
          Error();
        }
      | Year_month_day_hms({
          year,
          month: _,
          month_day,
          hms: {hour, minute, second},
        }) =>
        if (0 <= year
            && year <= 9999
            && 1 <= month_day
            && month_day <= 31
            && Time.Check.hour_minute_second_is_valid(~hour, ~minute, ~second)) {
          Ok();
        } else {
          Error();
        }
      }
    );

  let check_time_slot_expr =
      (e: Time_expr_ast.time_slot_expr): result(unit, unit) => {
    open Time_expr_ast;
    let aux = e =>
      switch (e) {
      | Tse_name(_) => Ok()
      | [@implicit_arity] Explicit_time_slot(x, y) =>
        if (Result.is_ok(check_time_point_expr(x))
            && Result.is_ok(check_time_point_expr(y))) {
          Ok();
        } else {
          Error();
        }
      };

    aux(e);
  };

  let check_branching_time_slot_expr =
      (e: Time_expr_ast.branching_time_slot_expr): result(unit, unit) => {
    open Time_expr_ast;
    let rec aux = e =>
      switch (e) {
      | [@implicit_arity] Bts_unary_op(op, e) => aux(e)
      | Bts_hms_ranges(hms_ranges) =>
        if (hms_ranges_are_valid(hms_ranges)) {
          Ok();
        } else {
          Error();
        }
      | Bts_month_days_and_hms_ranges({month_days, hms_ranges}) =>
        if (Time.Month_day_ranges.Check.list_is_valid(month_days)
            && hms_ranges_are_valid(hms_ranges)) {
          Ok();
        } else {
          Error();
        }
      | Bts_weekdays_and_hms_ranges({weekdays: _, hms_ranges}) =>
        if (hms_ranges_are_valid(hms_ranges)) {
          Ok();
        } else {
          Error();
        }
      | Bts_months_and_month_days_and_hms_ranges({
          months: _,
          month_days,
          hms_ranges,
        }) =>
        if (Time.Month_day_ranges.Check.list_is_valid(month_days)
            && hms_ranges_are_valid(hms_ranges)) {
          Ok();
        } else {
          Error();
        }
      | Bts_months_and_weekdays_and_hms_ranges({
          months,
          weekdays: _,
          hms_ranges,
        }) =>
        if (Time.Month_ranges.Check.list_is_valid(months)
            && hms_ranges_are_valid(hms_ranges)) {
          Ok();
        } else {
          Error();
        }
      | Bts_months_and_weekday_and_hms_ranges({
          months,
          weekday: _,
          hms_ranges,
          month_weekday_mode: _,
        }) =>
        if (Time.Month_ranges.Check.list_is_valid(months)
            && hms_ranges_are_valid(hms_ranges)) {
          Ok();
        } else {
          Error();
        }
      | Bts_years_and_months_and_month_days_and_hms_ranges({
          years: _,
          months,
          month_days,
          hms_ranges,
        }) =>
        if (Time.Month_ranges.Check.list_is_valid(months)
            && Time.Month_day_ranges.Check.list_is_valid(month_days)
            && hms_ranges_are_valid(hms_ranges)) {
          Ok();
        } else {
          Error();
        }
      };

    aux(e);
  };
};

let check_time_expr = (e: Time_expr_ast.t): result(unit, unit) => {
  open Time_expr_ast;
  let rec aux = e =>
    switch (e) {
    | Time_point_expr(e') => Check.check_time_point_expr(e')
    | Time_slot_expr(e') => Check.check_time_slot_expr(e')
    | Branching_time_slot_expr(e') => failwith("Unimplemented") /* Check.check_branching_time_slot_expr e' */
    | Time_pattern(p) =>
      Time_pattern.Check.check_time_pattern(p) |> Result.map_error(_ => ())
    | [@implicit_arity] Time_unary_op(_op, e') => aux(e')
    | [@implicit_arity] Time_binary_op(_op, e1, e2) =>
      switch (aux(e1)) {
      | Error () => Error()
      | Ok () => aux(e2)
      }
    | Time_round_robin_select(l) =>
      l
      |> List.map(aux)
      |> Misc_utils.get_ok_error_list
      |> Result.map(_ => ())
    };

  aux(e);
};

module Of_string = {
  open MParser;
  open Parser_components;

  let not_str = string("not");

  let next_slot_str = string("next-slot");

  let next_point_str = attempt(string("next-point")) <|> string("next-pt");

  let next_batch_str = string("next-batch");

  let next_str = string("next");

  let point_str = string("point");

  let slot_str = string("slot");

  let points_str = string("points");

  let slots_str = string("slots");

  let batch_str = string("batch");

  let batches_str = string("batches");

  let of_str = string("of");

  let to_str = string("to");

  let first_str = string("first");

  let last_str = string("last");

  let sign_expr =
    attempt(char('+') >> return(Time_expr_ast.Pos))
    <|> (char('-') >> return(Time_expr_ast.Neg));

  let branch_unary_op =
    Time_expr_ast.(
      attempt(next_batch_str >> return(Next_n_batches(1)))
      <|> attempt(string("every-batch") >> return(Every_batch))
      <|> attempt(
            next_str
            >> hyphen
            >> nat_zero
            << hyphen
            << (attempt(batches_str) <|> batch_str)
            |>> (n => Next_n_batches(n)),
          )
    );

  let ident_string =
    ident_string(~reserved_words=["to", "first", "last", "next", "every"]);

  let range_inc_expr = (p: t('a, unit)): t(Range.range('a), unit) =>
    attempt(
      p
      >>= (x => spaces >> to_str >> spaces >> p |>> (y => `Range_inc((x, y)))),
    )
    <|> (p |>> (x => `Range_inc((x, x))));

  let range_exc_expr = (p: t('a, unit)): t(Range.range('a), unit) =>
    attempt(
      p
      >>= (x => spaces >> to_str >> spaces >> p |>> (y => `Range_exc((x, y)))),
    )
    <|> (p |>> (x => `Range_inc((x, x))));

  let symbols = "()[]&|>";

  let ranges_expr =
      (p: t(Range.range('a), unit)): t(list(Range.range('a)), unit) =>
    sep_by_comma1(p);

  module Second = {
    let second_expr: t(Time_expr_ast.second_expr, unit) = (
      attempt(string("::"))
      >> get_pos
      >>= (
        pos =>
          attempt(nat_zero)
          >>= (
            second =>
              if (second >= 60) {
                fail(
                  Printf.sprintf(
                    "Invalid second: %d, pos: %s",
                    second,
                    string_of_pos(pos),
                  ),
                );
              } else {
                return(second);
              }
          )
          <|> (
            non_space_string
            >>= (
              s =>
                if (s == "") {
                  fail(
                    Printf.sprintf(
                      "Missing second after ::, pos: %s",
                      string_of_pos(pos),
                    ),
                  );
                } else {
                  fail(
                    Printf.sprintf(
                      "Invalid second: %s, pos: %s",
                      s,
                      string_of_pos(pos),
                    ),
                  );
                }
            )
          )
      ):
        t(Time_expr_ast.second_expr, unit)
    );
  };

  module Minute_second = {
    let minute_second_expr: t(Time_expr_ast.minute_second_expr, unit) = (
      attempt(char(':'))
      >> get_pos
      >>= (
        pos =>
          nat_zero
          >>= (
            minute =>
              if (minute >= 60) {
                fail(
                  Printf.sprintf(
                    "Invalid minute: %d, pos: %s",
                    minute,
                    string_of_pos(pos),
                  ),
                );
              } else {
                get_pos
                >>= (
                  pos =>
                    option(0, char(':') >> nat_zero)
                    >>= (
                      second =>
                        if (second >= 60) {
                          fail(
                            Printf.sprintf(
                              "Invalid second: %d, pos: %s",
                              second,
                              string_of_pos(pos),
                            ),
                          );
                        } else {
                          return(Time_expr_ast.{minute, second});
                        }
                    )
                );
              }
          )
      ):
        t(Time_expr_ast.minute_second_expr, unit)
    );
  };

  module Hms = {
    let hms_mode =
      option(
        `Hour_in_24_hours,
        attempt(string("am") >> return(`Hour_in_AM))
        <|> (string("pm") >> return(`Hour_in_PM)),
      );

    let handle_time_with_mode =
        (~hour_pos, ~hour: int, ~minute: int, ~second: int, mode) => {
      let pos = string_of_pos(hour_pos);
      switch (mode) {
      | `Hour_in_24_hours =>
        if (hour >= 24) {
          fail(Printf.sprintf("Invalid hour: %d, pos: %s", hour, pos));
        } else {
          return(Time_expr_ast.{hour, minute, second});
        }
      | `Hour_in_AM =>
        if (1 <= hour && hour <= 12) {
          let hour =
            if (hour == 12) {
              0;
            } else {
              hour;
            };
          return(Time_expr_ast.{hour, minute, second});
        } else {
          fail(Printf.sprintf("Invalid hour: %d, pos: %s", hour, pos));
        }
      | `Hour_in_PM =>
        if (1 <= hour && hour <= 12) {
          let hour =
            if (hour == 12) {
              0;
            } else {
              hour;
            };
          return(Time_expr_ast.{hour: hour + 12, minute, second});
        } else {
          fail(Printf.sprintf("Invalid hour: %d, pos %s", hour, pos));
        }
      };
    };

    let hms: t(Time_expr_ast.hms_expr, unit) = (
      get_pos
      >>= (
        hour_pos =>
          attempt(nat_zero << char(':'))
          >>= (
            hour =>
              get_pos
              >>= (
                minute_pos =>
                  nat_zero
                  >>= (
                    minute =>
                      if (minute >= 60) {
                        fail(
                          Printf.sprintf(
                            "Invalid minute: %d, pos: %s",
                            minute,
                            string_of_pos(minute_pos),
                          ),
                        );
                      } else {
                        get_pos
                        >>= (
                          second_pos =>
                            option(0, char(':') >> nat_zero)
                            >>= (
                              second =>
                                if (second >= 60) {
                                  fail(
                                    Printf.sprintf(
                                      "Invalid second: %d, pos: %s",
                                      second,
                                      string_of_pos(second_pos),
                                    ),
                                  );
                                } else {
                                  spaces
                                  >> hms_mode
                                  >>= (
                                    mode =>
                                      handle_time_with_mode(
                                        ~hour_pos,
                                        ~hour,
                                        ~minute,
                                        ~second,
                                        mode,
                                      )
                                  );
                                }
                            )
                        );
                      }
                  )
              )
          )
          <|> (
            nat_zero
            >>= (
              hour =>
                spaces
                >> hms_mode
                >>= (
                  mode =>
                    handle_time_with_mode(
                      ~hour_pos,
                      ~hour,
                      ~minute=0,
                      ~second=0,
                      mode,
                    )
                )
            )
          )
      ):
        t(Time_expr_ast.hms_expr, unit)
    );

    let hms_range: t(Time_expr_ast.hms_range_expr, unit) = (
      range_exc_expr(hms): t(Time_expr_ast.hms_range_expr, unit)
    );

    let hms_ranges: t(list(Time_expr_ast.hms_range_expr), unit) = (
      sep_by_comma1(hms_range): t(list(Time_expr_ast.hms_range_expr), unit)
    );

    let non_singular_hms_ranges: t(list(Time_expr_ast.hms_range_expr), unit) = (
      hms_range
      >>= (
        hd =>
          spaces
          >> comma
          >> spaces
          >> sep_by_comma1(hms_range)
          >>= (tl => return([hd, ...tl]))
      ):
        t(list(Time_expr_ast.hms_range_expr), unit)
    );

    let hmss: t(list(Time_expr_ast.hms_expr), unit) = (
      sep_by_comma1(hms): t(list(Time_expr_ast.hms_expr), unit)
    );
  };

  module Month_day = {
    let month_day_expr: t(int, unit) = (
      get_pos
      >>= (
        pos =>
          nat_zero
          >>= (
            x =>
              if (1 <= x && x <= 31) {
                return(x);
              } else {
                fail(
                  Printf.sprintf(
                    "Invalid month day: %d, pos: %s",
                    x,
                    string_of_pos(pos),
                  ),
                );
              }
          )
      ):
        t(int, unit)
    );

    let month_day_range_expr: t(Range.range(int), unit) = (
      range_inc_expr(month_day_expr): t(Range.range(int), unit)
    );

    let month_day_ranges_expr: t(list(Range.range(int)), unit) = (
      ranges_expr(month_day_range_expr): t(list(Range.range(int)), unit)
    );
  };

  module Weekday = {
    let weekday_expr: t(Time.weekday, unit) = (
      get_pos
      >>= (
        pos =>
          alpha_string
          >>= (
            x =>
              switch (Time.Of_string.weekday_of_string(x)) {
              | Ok(x) => return(x)
              | Error(_) =>
                fail(
                  Printf.sprintf(
                    "Failed to interpret weekday string, pos: %s",
                    string_of_pos(pos),
                  ),
                )
              }
          )
      ):
        t(Time.weekday, unit)
    );

    let weekday_range_expr: t(Range.range(Time.weekday), unit) = (
      range_inc_expr(weekday_expr): t(Range.range(Time.weekday), unit)
    );

    let weekday_ranges_expr: t(list(Range.range(Time.weekday)), unit) = (
      ranges_expr(weekday_range_expr):
        t(list(Range.range(Time.weekday)), unit)
    );
  };

  module Day = {
    let day_expr: t(Time_expr_ast.day_expr, unit) = (
      attempt(Month_day.month_day_expr |>> (x => Time_expr_ast.Month_day(x)))
      <|> (Weekday.weekday_expr |>> (x => Time_expr_ast.Weekday(x))):
        t(Time_expr_ast.day_expr, unit)
    );
  };

  module Month = {
    let human_int_month_expr: t(Time_expr_ast.month_expr, unit) = (
      nat_zero
      >>= (
        x =>
          switch (Time.month_of_human_int(x)) {
          | Ok(m) => return(m)
          | Error () => fail(Printf.sprintf("Invalid month: %d", x))
          }
      ):
        t(Time_expr_ast.month_expr, unit)
    );

    let human_int_month_range_expr = range_inc_expr(human_int_month_expr);

    let human_int_month_ranges_expr =
      sep_by_comma1(human_int_month_range_expr);

    let direct_pick_month_expr: t(Time_expr_ast.month_expr, unit) = (
      alpha_string
      >>= (
        x =>
          switch (Time.Of_string.month_of_string(x)) {
          | Ok(x) => return(x)
          | Error(_) =>
            fail(Printf.sprintf("Failed to interpret month string: %s", x))
          }
      ):
        t(Time_expr_ast.month_expr, unit)
    );

    let direct_pick_month_range_expr = range_inc_expr(direct_pick_month_expr);

    let direct_pick_month_ranges_expr =
      sep_by_comma1(direct_pick_month_range_expr);

    let month_expr = attempt(human_int_month_expr) <|> direct_pick_month_expr;

    let month_range_expr = range_inc_expr(month_expr);

    let month_ranges_expr = sep_by_comma1(month_range_expr);
  };

  module Year = {
    let year_expr: t(int, unit) = (nat_zero: t(int, unit));

    let year_range_expr = range_inc_expr(year_expr);

    let year_ranges_expr = sep_by_comma1(year_range_expr);
  };

  module Time_point_expr = {
    let tp_name =
      attempt(string("at:"))
      >> ident_string
      >>= (s => return(Time_expr_ast.Tpe_name(s)));

    let tp_ymd_hms =
      attempt(
        nat_zero
        >>= (
          year =>
            hyphen
            >> Month.human_int_month_expr
            >>= (
              month =>
                hyphen
                >> Month_day.month_day_expr
                >>= (month_day => return((year, month, month_day)))
            )
        ),
      )
      >>= (
        ((year, month, month_day)) =>
          spaces
          >> Hms.hms
          >>= (
            hms =>
              return(
                Time_expr_ast.Year_month_day_hms({
                  year,
                  month,
                  month_day,
                  hms,
                }),
              )
          )
      );

    let tp_ymond_hms =
      attempt(
        nat_zero
        >>= (
          year =>
            spaces
            >> Month.direct_pick_month_expr
            >>= (month => return((year, month)))
        ),
      )
      >>= (
        ((year, month)) =>
          spaces
          >> Month_day.month_day_expr
          >>= (month_day => return((year, month, month_day)))
      )
      >>= (
        ((year, month, month_day)) =>
          spaces
          >> Hms.hms
          >>= (
            hms =>
              return(
                Time_expr_ast.Year_month_day_hms({
                  year,
                  month,
                  month_day,
                  hms,
                }),
              )
          )
      );

    let tp_md_hms =
      attempt(
        Month.month_expr
        >>= (
          month =>
            hyphen
            >> Month_day.month_day_expr
            >>= (month_day => return((month, month_day)))
        ),
      )
      >>= (
        ((month, month_day)) =>
          spaces
          >> Hms.hms
          >>= (
            hms =>
              return(Time_expr_ast.Month_day_hms({month, month_day, hms}))
          )
      );

    let tp_mond_hms =
      attempt(
        Month.direct_pick_month_expr
        >>= (
          month =>
            spaces
            >> Month_day.month_day_expr
            >>= (month_day => return((month, month_day)))
        ),
      )
      >>= (
        ((month, month_day)) =>
          spaces
          >> Hms.hms
          >>= (
            hms =>
              return(Time_expr_ast.Month_day_hms({month, month_day, hms}))
          )
      );

    let tp_d_hms =
      attempt(
        Day.day_expr
        >>= (day => spaces >> Hms.hms >>= (hms => return((day, hms)))),
      )
      >>= (((day, hms)) => return(Time_expr_ast.Day_hms({day, hms})));

    let tp_hms = Hms.hms >>= (hms => return(Time_expr_ast.Hms(hms)));

    let tp_minute_second =
      Minute_second.minute_second_expr
      >>= (
        minute_second => return(Time_expr_ast.Minute_second(minute_second))
      );

    let tp_second =
      Second.second_expr >>= (second => return(Time_expr_ast.Second(second)));

    let time_point_expr: t(Time_expr_ast.time_point_expr, unit) = (
      choice([
        tp_name,
        tp_ymd_hms,
        tp_ymond_hms,
        tp_md_hms,
        tp_mond_hms,
        tp_d_hms,
        tp_second,
        tp_minute_second,
        tp_hms,
      ]):
        t(Time_expr_ast.time_point_expr, unit)
    );
  };

  module Time_slot_expr = {
    let ts_name =
      attempt(string("during:"))
      >> ident_string
      >>= (s => return(Time_expr_ast.Tse_name(s)));

    let ts_explicit_time_slot =
      attempt(
        Time_point_expr.time_point_expr
        >>= (start => spaces >> to_str >> return(start)),
      )
      >>= (
        start =>
          spaces
          >> get_pos
          >>= (
            pos =>
              attempt(Time_point_expr.time_point_expr)
              >>= (
                end_exc =>
                  return(
                    [@implicit_arity]
                    Time_expr_ast.Explicit_time_slot(start, end_exc),
                  )
              )
              <|> fail(
                    Printf.sprintf(
                      "Expected time point expression at %s",
                      string_of_pos(pos),
                    ),
                  )
          )
      );

    let time_slot_expr: t(Time_expr_ast.time_slot_expr, unit) = (
      ts_name <|> ts_explicit_time_slot: t(Time_expr_ast.time_slot_expr, unit)
    );
  };

  module Branching_time_slot_expr = {
    let bts_hms_ranges =
      Hms.non_singular_hms_ranges
      >>= (hms_ranges => return(Time_expr_ast.Bts_hms_ranges(hms_ranges)));

    let bts_days_hms_ranges =
      attempt(
        Month_day.month_day_ranges_expr
        >>= (
          month_days =>
            spaces
            >> dot
            >> spaces
            >> Hms.hms_ranges
            |>> (
              hms_ranges =>
                Time_expr_ast.Bts_month_days_and_hms_ranges({
                  month_days,
                  hms_ranges,
                })
            )
        ),
      )
      <|> (
        Weekday.weekday_ranges_expr
        >>= (
          weekdays =>
            spaces
            >> dot
            >> spaces
            >> Hms.hms_ranges
            |>> (
              hms_ranges =>
                Time_expr_ast.Bts_weekdays_and_hms_ranges({
                  weekdays,
                  hms_ranges,
                })
            )
        )
      );

    let bts_hms_ranges_days =
      attempt(
        Hms.hms_ranges
        >>= (
          hms_ranges =>
            spaces
            >> of_str
            >> spaces
            >> Month_day.month_day_ranges_expr
            |>> (
              month_days =>
                Time_expr_ast.Bts_month_days_and_hms_ranges({
                  month_days,
                  hms_ranges,
                })
            )
        ),
      )
      <|> (
        Hms.hms_ranges
        >>= (
          hms_ranges =>
            spaces
            >> of_str
            >> spaces
            >> Weekday.weekday_ranges_expr
            |>> (
              weekdays =>
                Time_expr_ast.Bts_weekdays_and_hms_ranges({
                  weekdays,
                  hms_ranges,
                })
            )
        )
      );

    let bts_months_mdays_hms_ranges =
      Month.month_ranges_expr
      >>= (
        months =>
          spaces
          >> dot
          >> spaces
          >> Month_day.month_day_ranges_expr
          >>= (
            month_days =>
              spaces
              >> dot
              >> spaces
              >> Hms.hms_ranges
              |>> (
                hms_ranges =>
                  Time_expr_ast.Bts_months_and_month_days_and_hms_ranges({
                    months,
                    month_days,
                    hms_ranges,
                  })
              )
          )
      );

    let bts_hms_ranges_mdays_months =
      Hms.hms_ranges
      >>= (
        hms_ranges =>
          spaces
          >> of_str
          >> spaces
          >> Month_day.month_day_ranges_expr
          >>= (
            month_days =>
              spaces
              >> of_str
              >> spaces
              >> Month.month_ranges_expr
              |>> (
                months =>
                  Time_expr_ast.Bts_months_and_month_days_and_hms_ranges({
                    months,
                    month_days,
                    hms_ranges,
                  })
              )
          )
      );

    let bts_months_wdays_hms_ranges =
      Month.month_ranges_expr
      >>= (
        months =>
          spaces
          >> dot
          >> spaces
          >> Weekday.weekday_ranges_expr
          >>= (
            weekdays =>
              spaces
              >> dot
              >> spaces
              >> Hms.hms_ranges
              |>> (
                hms_ranges =>
                  Time_expr_ast.Bts_months_and_weekdays_and_hms_ranges({
                    months,
                    weekdays,
                    hms_ranges,
                  })
              )
          )
      );

    let month_weekday_mode_expr =
      attempt(
        first_str
        >> spaces
        >> nat_zero
        |>> (n => Some(Time_expr_ast.First_n(n))),
      )
      <|> (
        last_str
        >> spaces
        >> nat_zero
        |>> (n => Some(Time_expr_ast.Last_n(n)))
      );

    let bts_months_wday_hms_ranges =
      Month.month_ranges_expr
      >>= (
        months =>
          spaces
          >> dot
          >> spaces
          >> month_weekday_mode_expr
          >>= (
            month_weekday_mode =>
              spaces
              >> Weekday.weekday_expr
              >>= (
                weekday =>
                  spaces
                  >> dot
                  >> spaces
                  >> Hms.hms_ranges
                  |>> (
                    hms_ranges =>
                      Time_expr_ast.Bts_months_and_weekday_and_hms_ranges({
                        months,
                        weekday,
                        hms_ranges,
                        month_weekday_mode,
                      })
                  )
              )
          )
      );

    let bts_years_months_mdays_hms_ranges =
      Year.year_ranges_expr
      >>= (
        years =>
          spaces
          >> dot
          >> spaces
          >> Month.month_ranges_expr
          >>= (
            months =>
              spaces
              >> dot
              >> spaces
              >> Month_day.month_day_ranges_expr
              >>= (
                month_days =>
                  spaces
                  >> dot
                  >> spaces
                  >> Hms.hms_ranges
                  |>> (
                    hms_ranges =>
                      Time_expr_ast.Bts_years_and_months_and_month_days_and_hms_ranges({
                        years,
                        months,
                        month_days,
                        hms_ranges,
                      })
                  )
              )
          )
      );

    let bts_hms_ranges_mdays_months_years =
      Hms.hms_ranges
      >>= (
        hms_ranges =>
          spaces
          >> of_str
          >> spaces
          >> Month_day.month_day_ranges_expr
          >>= (
            month_days =>
              spaces
              >> of_str
              >> spaces
              >> Month.month_ranges_expr
              >>= (
                months =>
                  spaces
                  >> of_str
                  >> spaces
                  >> Year.year_ranges_expr
                  |>> (
                    years =>
                      Time_expr_ast.Bts_years_and_months_and_month_days_and_hms_ranges({
                        years,
                        months,
                        month_days,
                        hms_ranges,
                      })
                  )
              )
          )
      );

    let branching_time_slot_expr_atom:
      t(Time_expr_ast.branching_time_slot_expr, unit) = (
      choice([
        attempt(bts_hms_ranges),
        attempt(bts_days_hms_ranges),
        attempt(bts_months_mdays_hms_ranges),
        attempt(bts_months_wdays_hms_ranges),
        attempt(bts_months_wday_hms_ranges),
        attempt(bts_years_months_mdays_hms_ranges),
        attempt(bts_hms_ranges_mdays_months_years),
        attempt(bts_hms_ranges_mdays_months),
        attempt(bts_hms_ranges_days),
      ]):
        t(Time_expr_ast.branching_time_slot_expr, unit)
    );

    let branching_time_slot_expr:
      t(Time_expr_ast.branching_time_slot_expr, unit) = (
      attempt(
        branch_unary_op
        >>= (
          op =>
            spaces
            >> branching_time_slot_expr_atom
            |>> (e => [@implicit_arity] Time_expr_ast.Bts_unary_op(op, e))
        ),
      )
      <|> branching_time_slot_expr_atom:
        t(Time_expr_ast.branching_time_slot_expr, unit)
    );
  };

  let inter: t((Time_expr_ast.t, Time_expr_ast.t) => Time_expr_ast.t, unit) = (
    string("&&")
    >> return((a, b) =>
         [@implicit_arity] Time_expr_ast.Time_binary_op(Inter, a, b)
       ):
      t((Time_expr_ast.t, Time_expr_ast.t) => Time_expr_ast.t, unit)
  );

  let union: t((Time_expr_ast.t, Time_expr_ast.t) => Time_expr_ast.t, unit) = (
    string("||")
    >> return((a, b) =>
         [@implicit_arity] Time_expr_ast.Time_binary_op(Union, a, b)
       ):
      t((Time_expr_ast.t, Time_expr_ast.t) => Time_expr_ast.t, unit)
  );

  let round_robin_select:
    t((Time_expr_ast.t, Time_expr_ast.t) => Time_expr_ast.t, unit) = (
    string(">>")
    >> return((a, b) => Time_expr_ast.Time_round_robin_select([a, b])):
      t((Time_expr_ast.t, Time_expr_ast.t) => Time_expr_ast.t, unit)
  );

  let flatten_round_robin_select = (e: Time_expr_ast.t): Time_expr_ast.t => {
    open Time_expr_ast;
    let rec aux = e =>
      switch (e) {
      | Time_point_expr(_) => e
      | Time_slot_expr(_) => e
      | Branching_time_slot_expr(_) => e
      | Time_pattern(_) => e
      | [@implicit_arity] Time_unary_op(op, e) =>
        [@implicit_arity] Time_unary_op(op, aux(e))
      | [@implicit_arity] Time_binary_op(op, e1, e2) =>
        [@implicit_arity] Time_binary_op(op, aux(e1), aux(e2))
      | Time_round_robin_select(l) =>
        l
        |> List.to_seq
        |> Seq.map(aux)
        |> Seq.flat_map(e =>
             switch (e) {
             | Time_round_robin_select(l) => List.to_seq(l)
             | _ => Seq.return(e)
             }
           )
        |> List.of_seq
        |> (l => Time_round_robin_select(l))
      };

    aux(e);
  };

  let time_expr =
      (~enabled_fragments: list(lang_fragment)): t(Time_expr_ast.t, unit) => {
    open Time_expr_ast;
    let atom_parsers =
      [
        if (List.mem(`Time_pattern, enabled_fragments)) {
          Some(
            attempt(Time_pattern.Parsers.time_pattern_expr)
            >>= (e => return(Time_expr_ast.Time_pattern(e))),
          );
        } else {
          None;
        },
        if (List.mem(`Branching_time_slot_expr, enabled_fragments)) {
          Some(
            Branching_time_slot_expr.branching_time_slot_expr
            >>= (e => return(Time_expr_ast.Branching_time_slot_expr(e))),
          );
        } else {
          None;
        },
        if (List.mem(`Time_slot_expr, enabled_fragments)) {
          Some(
            Time_slot_expr.time_slot_expr
            >>= (e => return(Time_expr_ast.Time_slot_expr(e))),
          );
        } else {
          None;
        },
        if (List.mem(`Time_point_expr, enabled_fragments)) {
          Some(
            Time_point_expr.time_point_expr
            >>= (e => return(Time_expr_ast.Time_point_expr(e))),
          );
        } else {
          None;
        },
      ]
      |> List.filter_map(x => x);

    let rec make_atom = l =>
      /* match l with
       * | [] ->
       *   get_pos
       *   >>= fun pos ->
       *   fail
       *     (Printf.sprintf "Failed to parse expression, pos: %s"
       *        (string_of_pos pos))
       * | x :: xs -> x <|> make_atom xs */
      choice(l);

    let atom = spaces >> make_atom(atom_parsers) << spaces;
    let rec expr = mparser_state => {
      let group =
        attempt(char('('))
        >> (spaces >> expr << spaces << char(')'))
        <|> atom;

      let unary_op =
        choice([
          attempt(not_str) >> return(Not),
          attempt(next_slot_str) >> return(Next_n_slots(1)),
          attempt(next_point_str) >> return(Next_n_points(1)),
          attempt(
            next_str
            >> hyphen
            >> nat_zero
            << hyphen
            << (attempt(slots_str) <|> slot_str),
          )
          |>> (n => Next_n_slots(n)),
          attempt(
            next_str
            >> hyphen
            >> nat_zero
            << hyphen
            << (attempt(points_str) <|> point_str),
          )
          |>> (n => Next_n_points(n)),
          attempt(
            string("tzoffset=")
            >> sign_expr
            >>= (
              sign =>
                Hms.hms |>> (hms => [@implicit_arity] Tz_offset(sign, hms))
            ),
          ),
        ]);

      let inter_part =
        attempt(unary_op)
        >>= (
          op =>
            spaces >> expr |>> (e => [@implicit_arity] Time_unary_op(op, e))
        )
        <|> group;

      let ordered_select_part = chain_left1(inter_part, round_robin_select);
      let union_part = chain_left1(ordered_select_part, inter);
      chain_left1(union_part, union, mparser_state);
    };

    expr >>= (e => return(flatten_round_robin_select(e)));
  };

  let of_string =
      (~enabled_fragments=all_lang_fragments, s: string)
      : Result.t(Time_expr_ast.t, string) =>
    switch (enabled_fragments) {
    | [] => Error("No language fragments are enabled")
    | _ =>
      parse_string(
        time_expr(~enabled_fragments)
        << spaces
        >>= (
          e =>
            get_pos
            >>= (
              pos =>
                attempt(eof)
                >> return(e)
                <|> fail(
                      Printf.sprintf(
                        "Expected EOI, pos: %s",
                        string_of_pos(pos),
                      ),
                    )
            )
        ),
        s,
        (),
      )
      |> result_of_mparser_result
    };
};

let time_expr_parser = (~enabled_fragments=all_lang_fragments) =>
  Of_string.time_expr(~enabled_fragments);

let of_string = Of_string.of_string;

module To_time_pattern_lossy = {
  module Second = {
    let update_time_pattern_using_second_expr =
        (e: Time_expr_ast.second_expr, base: Time_pattern.time_pattern)
        : Time_pattern.time_pattern =>
      if (Time.Check.second_is_valid(~second=e)) {
        {...base, seconds: [e]};
      } else {
        raise(Invalid_time_expr(Printf.sprintf("Invalid second: ::%d", e)));
      };

    let time_range_pattern_of_second_range_expr_and_base_time_pattern =
        (e: Time_expr_ast.second_range_expr, base: Time_pattern.time_pattern)
        : Time_pattern.time_range_pattern =>
      switch (e) {
      | `Range_inc(x, y) =>
        `Range_inc((
          update_time_pattern_using_second_expr(x, base),
          update_time_pattern_using_second_expr(y, base),
        ))
      | `Range_exc(x, y) =>
        `Range_exc((
          update_time_pattern_using_second_expr(x, base),
          update_time_pattern_using_second_expr(y, base),
        ))
      };

    let time_range_patterns_of_second_ranges_and_base_time_pattern =
        (
          l: list(Time_expr_ast.second_range_expr),
          base: Time_pattern.time_pattern,
        )
        : Seq.t(Time_pattern.time_range_pattern) =>
      List.to_seq(l)
      |> Seq.map(e =>
           time_range_pattern_of_second_range_expr_and_base_time_pattern(
             e,
             base,
           )
         );
  };

  module Minute_second = {
    let update_time_pattern_using_minute_second_expr =
        (e: Time_expr_ast.minute_second_expr, base: Time_pattern.time_pattern)
        : Time_pattern.time_pattern =>
      if (Time.Check.minute_second_is_valid(
            ~minute=e.minute,
            ~second=e.second,
          )) {
        {...base, minutes: [e.minute]};
      } else {
        raise(
          Invalid_time_expr(
            Printf.sprintf(
              "Invalid minute second: :%d:%d",
              e.minute,
              e.second,
            ),
          ),
        );
      };

    let time_range_pattern_of_minute_second_range_expr_and_base_time_pattern =
        (
          e: Time_expr_ast.minute_second_range_expr,
          base: Time_pattern.time_pattern,
        )
        : Time_pattern.time_range_pattern =>
      switch (e) {
      | `Range_inc(x, y) =>
        `Range_inc((
          update_time_pattern_using_minute_second_expr(x, base),
          update_time_pattern_using_minute_second_expr(y, base),
        ))
      | `Range_exc(x, y) =>
        `Range_exc((
          update_time_pattern_using_minute_second_expr(x, base),
          update_time_pattern_using_minute_second_expr(y, base),
        ))
      };

    let time_range_patterns_of_hour_minute_second_ranges_and_base_time_pattern =
        (
          l: list(Time_expr_ast.minute_second_range_expr),
          base: Time_pattern.time_pattern,
        )
        : Seq.t(Time_pattern.time_range_pattern) =>
      List.to_seq(l)
      |> Seq.map(e =>
           time_range_pattern_of_minute_second_range_expr_and_base_time_pattern(
             e,
             base,
           )
         );
  };

  module Hms = {
    let update_time_pattern_using_hms_expr =
        (e: Time_expr_ast.hms_expr, base: Time_pattern.time_pattern)
        : Time_pattern.time_pattern =>
      if (Time.Check.hour_minute_second_is_valid(
            ~hour=e.hour,
            ~minute=e.minute,
            ~second=e.second,
          )) {
        {...base, hours: [e.hour], minutes: [e.minute]};
      } else {
        raise(
          Invalid_time_expr(
            Printf.sprintf("Invalid hour minute: %d:%d", e.hour, e.minute),
          ),
        );
      };

    let time_range_pattern_of_hms_range_expr_and_base_time_pattern =
        (e: Time_expr_ast.hms_range_expr, base: Time_pattern.time_pattern)
        : Time_pattern.time_range_pattern =>
      switch (e) {
      | `Range_inc(x, y) =>
        `Range_inc((
          update_time_pattern_using_hms_expr(x, base),
          update_time_pattern_using_hms_expr(y, base),
        ))
      | `Range_exc(x, y) =>
        `Range_exc((
          update_time_pattern_using_hms_expr(x, base),
          update_time_pattern_using_hms_expr(y, base),
        ))
      };

    let time_range_patterns_of_hms_ranges_and_base_time_pattern =
        (
          l: list(Time_expr_ast.hms_range_expr),
          base: Time_pattern.time_pattern,
        )
        : Seq.t(Time_pattern.time_range_pattern) =>
      List.to_seq(l)
      |> Seq.map(e =>
           time_range_pattern_of_hms_range_expr_and_base_time_pattern(e, base)
         );

    let time_patterns_of_hmss_and_base_time_pattern =
        (l: list(Time_expr_ast.hms_expr), base: Time_pattern.time_pattern)
        : Seq.t(Time_pattern.time_pattern) =>
      List.to_seq(l)
      |> Seq.map(e => update_time_pattern_using_hms_expr(e, base));
  };

  module Month_day = {
    let update_time_pattern_using_month_day_expr =
        (x: int, base: Time_pattern.time_pattern): Time_pattern.time_pattern =>
      if (1 <= x && x <= 31) {
        {...base, month_days: [x]};
      } else {
        raise(
          Invalid_time_expr(Printf.sprintf("Invalid day of month: %d", x)),
        );
      };

    let time_pattern_of_month_day_expr = x =>
      update_time_pattern_using_month_day_expr(x, Time_pattern.empty);

    let time_patterns_of_month_days_and_base_time_pattern =
        (l: list(int), base: Time_pattern.time_pattern)
        : Seq.t(Time_pattern.time_pattern) =>
      List.to_seq(l)
      |> Seq.map(e => update_time_pattern_using_month_day_expr(e, base));
  };

  module Weekday = {
    let update_time_pattern_using_weekday_expr =
        (x: Time.weekday, base: Time_pattern.time_pattern)
        : Time_pattern.time_pattern => {
      ...base,
      weekdays: [x],
    };

    let time_pattern_of_weekday_expr = x =>
      update_time_pattern_using_weekday_expr(x, Time_pattern.empty);

    let time_patterns_of_weekdays_and_base_time_pattern =
        (l: list(Time.weekday), base: Time_pattern.time_pattern)
        : Seq.t(Time_pattern.time_pattern) =>
      List.to_seq(l)
      |> Seq.map(e => update_time_pattern_using_weekday_expr(e, base));
  };

  module Day = {
    let update_time_pattern_using_day_expr =
        (e: Time_expr_ast.day_expr, base: Time_pattern.time_pattern)
        : Time_pattern.time_pattern =>
      switch (e) {
      | Month_day(e) =>
        Month_day.update_time_pattern_using_month_day_expr(e, base)
      | Weekday(e) => Weekday.update_time_pattern_using_weekday_expr(e, base)
      };
  };

  module Month = {
    let update_time_pattern_using_month_expr =
        (e: Time_expr_ast.month_expr, base: Time_pattern.time_pattern)
        : Time_pattern.time_pattern => {
      ...base,
      months: [e],
    };

    let time_pattern_of_month_expr = x =>
      update_time_pattern_using_month_expr(x, Time_pattern.empty);

    let time_patterns_of_months_and_base_time_pattern =
        (l: list(Time.month), base: Time_pattern.time_pattern)
        : Seq.t(Time_pattern.time_pattern) =>
      List.to_seq(l)
      |> Seq.map(e => update_time_pattern_using_month_expr(e, base));
  };

  module Year = {
    let update_time_pattern_using_year_expr =
        (e: Time_expr_ast.year_expr, base: Time_pattern.time_pattern)
        : Time_pattern.time_pattern => {
      ...base,
      years: [e],
    };

    let time_pattern_of_year_expr = x =>
      update_time_pattern_using_year_expr(x, Time_pattern.empty);
  };

  module Unix_times = {
    let update_time_pattern_using_unix_seconds =
        (unix_seconds: list(int64), base: Time_pattern.time_pattern)
        : Time_pattern.time_pattern => {
      ...base,
      unix_seconds,
    };

    let time_pattern_of_unix_seconds = l =>
      update_time_pattern_using_unix_seconds(l, Time_pattern.empty);
  };

  let time_pattern_of_time_point_expr =
      (
        ~f_resolve_tpe_name=default_f_resolve_tpe_name,
        e: Time_expr_ast.time_point_expr,
      )
      : result(Time_pattern.time_pattern, string) =>
    try(
      switch (Resolve.resolve_time_point_expr(~f_resolve_tpe_name, e)) {
      | Error(msg) => Error(msg)
      | Ok(e) =>
        Ok(
          switch (e) {
          | Tpe_name(_) => failwith("Unexpected case")
          | Tpe_unix_seconds(l) => Unix_times.time_pattern_of_unix_seconds(l)
          | Year_month_day_hms({year, month, month_day, hms}) =>
            Time_pattern.empty
            |> Year.update_time_pattern_using_year_expr(year)
            |> Month.update_time_pattern_using_month_expr(month)
            |> Month_day.update_time_pattern_using_month_day_expr(month_day)
            |> Hms.update_time_pattern_using_hms_expr(hms)
          | Month_day_hms({month, month_day, hms}) =>
            Time_pattern.empty
            |> Month.update_time_pattern_using_month_expr(month)
            |> Month_day.update_time_pattern_using_month_day_expr(month_day)
            |> Hms.update_time_pattern_using_hms_expr(hms)
          | Day_hms({day, hms}) =>
            Time_pattern.empty
            |> Day.update_time_pattern_using_day_expr(day)
            |> Hms.update_time_pattern_using_hms_expr(hms)
          | Hms(hms) =>
            Hms.update_time_pattern_using_hms_expr(hms, Time_pattern.empty)
          | Minute_second(second) =>
            Minute_second.update_time_pattern_using_minute_second_expr(
              second,
              Time_pattern.empty,
            )
          | Second(second) =>
            Second.update_time_pattern_using_second_expr(
              second,
              Time_pattern.empty,
            )
          },
        )
      }
    ) {
    | Invalid_time_expr(msg) => Error(msg)
    };

  let time_pattern_of_time_point_expr =
      (
        ~f_resolve_tpe_name=default_f_resolve_tpe_name,
        e: Time_expr_ast.time_point_expr,
      )
      : result(Time_pattern.time_pattern, string) =>
    time_pattern_of_time_point_expr(~f_resolve_tpe_name, e);

  let time_range_patterns_of_time_slot_expr =
      (
        ~f_resolve_tse_name=default_f_resolve_tse_name,
        ~f_resolve_tpe_name=default_f_resolve_tpe_name,
        e: Time_expr_ast.time_slot_expr,
      )
      : result(list(Time_pattern.time_range_pattern), string) => {
    open Time_expr_ast;
    let aux = e =>
      switch (e) {
      | Tse_name(_) => failwith("Unexpected case")
      | [@implicit_arity] Explicit_time_slot(start, end_exc) =>
        switch (time_pattern_of_time_point_expr(~f_resolve_tpe_name, start)) {
        | Error(msg) => raise(Invalid_time_expr(msg))
        | Ok(start) =>
          switch (
            time_pattern_of_time_point_expr(~f_resolve_tpe_name, end_exc)
          ) {
          | Error(msg) => raise(Invalid_time_expr(msg))
          | Ok(end_exc) => [`Range_exc((start, end_exc))]
          }
        }
      };

    try(
      switch (
        Resolve.resolve_time_slot_expr(
          ~f_resolve_tse_name,
          ~f_resolve_tpe_name,
          e,
        )
      ) {
      | Error(msg) => Error(msg)
      | Ok(e) => Ok(aux(e))
      }
    ) {
    | Invalid_time_expr(msg) => Error(msg)
    };
  };

  /* let time_patterns_of_branching_time_point_expr
   *     (e : Time_expr_ast.branching_time_point_expr) :
   *   (Time_pattern.time_pattern list, string) result =
   *   let open Time_expr_ast in
   *   let rec aux e =
   *     match e with
   *     | Btp_unary_op (_, e) -> aux e
   *     | Btp_month_days_and_hmss { month_days; hmss } ->
   *       (\* check_hmss hmss; *\)
   *       month_days
   *       |> List.to_seq
   *       |> Time.Month_day_ranges.Flatten.flatten
   *       |> Seq.map Month_day.time_pattern_of_month_day_expr
   *       |> Seq.flat_map (Hms.time_patterns_of_hmss_and_base_time_pattern hmss)
   *       |> List.of_seq
   *     | Btp_weekdays_and_hmss { weekdays; hmss } ->
   *       weekdays
   *       |> List.to_seq
   *       |> Time.Weekday_ranges.Flatten.flatten
   *       |> Seq.map Weekday.time_pattern_of_weekday_expr
   *       |> Seq.flat_map (Hms.time_patterns_of_hmss_and_base_time_pattern hmss)
   *       |> List.of_seq
   *     | Btp_months_and_month_days_and_hmss { months; month_days; hmss } ->
   *       let month_days =
   *         Time.Month_tm_int_ranges.Flatten.flatten_list month_days
   *       in
   *       months
   *       |> List.to_seq
   *       |> Time.Month_ranges.Flatten.flatten
   *       |> Seq.map Month.time_pattern_of_month_expr
   *       |> Seq.flat_map
   *         (Month_day.time_patterns_of_month_days_and_base_time_pattern
   *            month_days)
   *       |> Seq.flat_map (Hms.time_patterns_of_hmss_and_base_time_pattern hmss)
   *       |> List.of_seq
   *     | Btp_months_and_weekdays_and_hmss { months; weekdays; hmss } ->
   *       let weekdays = Time.Weekday_ranges.Flatten.flatten_list weekdays in
   *       months
   *       |> List.to_seq
   *       |> Time.Month_ranges.Flatten.flatten
   *       |> Seq.map Month.time_pattern_of_month_expr
   *       |> Seq.flat_map
   *         (Weekday.time_patterns_of_weekdays_and_base_time_pattern
   *            weekdays)
   *       |> Seq.flat_map (Hms.time_patterns_of_hmss_and_base_time_pattern hmss)
   *       |> List.of_seq
   *     | Btp_months_and_weekday_and_hmss
   *         { months; weekday; hmss; month_weekday_mode = _ } ->
   *       months
   *       |> List.to_seq
   *       |> Time.Month_ranges.Flatten.flatten
   *       |> Seq.map Month.time_pattern_of_month_expr
   *       |> Seq.map (Weekday.update_time_pattern_using_weekday_expr weekday)
   *       |> Seq.flat_map (Hms.time_patterns_of_hmss_and_base_time_pattern hmss)
   *       |> List.of_seq
   *     | Btp_years_and_months_and_month_days_and_hmss
   *         { years; months; month_days; hmss } ->
   *       let months = Time.Month_ranges.Flatten.flatten_list months in
   *       let month_days =
   *         Time.Month_day_ranges.Flatten.flatten_list month_days
   *       in
   *       years
   *       |> List.to_seq
   *       |> Time.Year_ranges.Flatten.flatten
   *       |> Seq.map Year.time_pattern_of_year_expr
   *       |> Seq.flat_map
   *         (Month.time_patterns_of_months_and_base_time_pattern months)
   *       |> Seq.flat_map
   *         (Month_day.time_patterns_of_month_days_and_base_time_pattern
   *            month_days)
   *       |> Seq.flat_map (Hms.time_patterns_of_hmss_and_base_time_pattern hmss)
   *       |> List.of_seq
   *   in
   *   try Ok (aux e) with Invalid_time_expr msg -> Error msg */

  let time_range_patterns_of_branching_time_slot_expr =
      (e: Time_expr_ast.branching_time_slot_expr)
      : result(list(Time_pattern.time_range_pattern), string) => {
    open Time_expr_ast;
    let rec aux = e =>
      switch (e) {
      | [@implicit_arity] Bts_unary_op(_, e) => aux(e)
      | Bts_hms_ranges(hms_ranges) =>
        Hms.time_range_patterns_of_hms_ranges_and_base_time_pattern(
          hms_ranges,
          Time_pattern.empty,
        )
        |> List.of_seq
      | Bts_month_days_and_hms_ranges({month_days, hms_ranges}) =>
        /* check_hms_ranges hms_ranges; */
        month_days
        |> List.to_seq
        |> Time.Month_day_ranges.Flatten.flatten
        |> Seq.map(Month_day.time_pattern_of_month_day_expr)
        |> Seq.flat_map(
             Hms.time_range_patterns_of_hms_ranges_and_base_time_pattern(
               hms_ranges,
             ),
           )
        |> List.of_seq
      | Bts_weekdays_and_hms_ranges({weekdays, hms_ranges}) =>
        weekdays
        |> List.to_seq
        |> Time.Weekday_ranges.Flatten.flatten
        |> Seq.map(Weekday.time_pattern_of_weekday_expr)
        |> Seq.flat_map(
             Hms.time_range_patterns_of_hms_ranges_and_base_time_pattern(
               hms_ranges,
             ),
           )
        |> List.of_seq
      | Bts_months_and_month_days_and_hms_ranges({
          months,
          month_days,
          hms_ranges,
        }) =>
        let month_days =
          Time.Month_tm_int_ranges.Flatten.flatten_list(month_days);

        months
        |> List.to_seq
        |> Time.Month_ranges.Flatten.flatten
        |> Seq.map(Month.time_pattern_of_month_expr)
        |> Seq.flat_map(
             Month_day.time_patterns_of_month_days_and_base_time_pattern(
               month_days,
             ),
           )
        |> Seq.flat_map(
             Hms.time_range_patterns_of_hms_ranges_and_base_time_pattern(
               hms_ranges,
             ),
           )
        |> List.of_seq;
      | Bts_months_and_weekdays_and_hms_ranges({months, weekdays, hms_ranges}) =>
        let weekdays = Time.Weekday_ranges.Flatten.flatten_list(weekdays);
        months
        |> List.to_seq
        |> Time.Month_ranges.Flatten.flatten
        |> Seq.map(Month.time_pattern_of_month_expr)
        |> Seq.flat_map(
             Weekday.time_patterns_of_weekdays_and_base_time_pattern(
               weekdays,
             ),
           )
        |> Seq.flat_map(
             Hms.time_range_patterns_of_hms_ranges_and_base_time_pattern(
               hms_ranges,
             ),
           )
        |> List.of_seq;
      | Bts_months_and_weekday_and_hms_ranges({
          months,
          weekday,
          hms_ranges,
          month_weekday_mode: _,
        }) =>
        months
        |> List.to_seq
        |> Time.Month_ranges.Flatten.flatten
        |> Seq.map(Month.time_pattern_of_month_expr)
        |> Seq.map(Weekday.update_time_pattern_using_weekday_expr(weekday))
        |> Seq.flat_map(
             Hms.time_range_patterns_of_hms_ranges_and_base_time_pattern(
               hms_ranges,
             ),
           )
        |> List.of_seq
      | Bts_years_and_months_and_month_days_and_hms_ranges({
          years,
          months,
          month_days,
          hms_ranges,
        }) =>
        let months = Time.Month_ranges.Flatten.flatten_list(months);
        let month_days =
          Time.Month_day_ranges.Flatten.flatten_list(month_days);

        years
        |> List.to_seq
        |> Time.Year_ranges.Flatten.flatten
        |> Seq.map(Year.time_pattern_of_year_expr)
        |> Seq.flat_map(
             Month.time_patterns_of_months_and_base_time_pattern(months),
           )
        |> Seq.flat_map(
             Month_day.time_patterns_of_month_days_and_base_time_pattern(
               month_days,
             ),
           )
        |> Seq.flat_map(
             Hms.time_range_patterns_of_hms_ranges_and_base_time_pattern(
               hms_ranges,
             ),
           )
        |> List.of_seq;
      };

    try(Ok(aux(e))) {
    | Invalid_time_expr(msg) => Error(msg)
    };
  };
};

module Time_point_expr = {
  let matching_unix_seconds =
      (
        ~f_resolve_tpe_name,
        search_param: Search_param.t,
        e: Time_expr_ast.time_point_expr,
      )
      : result(Seq.t(int64), string) =>
    switch (
      To_time_pattern_lossy.time_pattern_of_time_point_expr(
        ~f_resolve_tpe_name,
        e,
      )
    ) {
    | Error(msg) => Error(msg)
    | Ok(pat) =>
      switch (
        Time_pattern.Single_pattern.matching_time_slots(
          ~allow_search_param_override=true,
          search_param,
          pat,
        )
      ) {
      | Error(e) => Error(Time_pattern.To_string.string_of_error(e))
      | Ok(s) => s |> Seq.map(fst) |> Result.ok
      }
    };
};

module Time_slot_expr = {
  let get_first_or_last_n_matches_of_same_month_date_time_pair_seq =
      (
        ~first_or_last: [ | `First | `Last],
        ~n: int,
        s: Seq.t((Time.Date_time.t, Time.Date_time.t)),
      )
      : Seq.t((Time.Date_time.t, Time.Date_time.t)) => {
    let flush_acc =
        (
          first_or_last,
          n: int,
          acc: list((Time.Date_time.t, Time.Date_time.t)),
        )
        : Seq.t((Time.Date_time.t, Time.Date_time.t)) =>
      (
        switch (first_or_last) {
        | `First => acc |> List.rev |> Misc_utils.take_first_n_list(n)
        | `Last => acc |> List.rev |> Misc_utils.take_last_n_list(n)
        }
      )
      |> List.to_seq;

    let rec aux =
            (
              first_or_last,
              n: int,
              acc: list((Time.Date_time.t, Time.Date_time.t)),
              s: Seq.t((Time.Date_time.t, Time.Date_time.t)),
            )
            : Seq.t((Time.Date_time.t, Time.Date_time.t)) =>
      switch (s()) {
      | Seq.Nil => flush_acc(first_or_last, n, acc)
      | [@implicit_arity] Seq.Cons((start, end_exc), rest) =>
        switch (acc) {
        | [] => aux(first_or_last, n, [(start, end_exc)], rest)
        | [(x, _), ..._] =>
          if (x.month == start.month) {
            aux(first_or_last, n, [(start, end_exc), ...acc], rest);
          } else {
            OSeq.append(
              flush_acc(first_or_last, n, acc),
              aux(first_or_last, n, [(start, end_exc)], rest),
            );
          }
        }
      };

    aux(first_or_last, n, [], s);
  };

  let get_first_or_last_n_matches_of_same_month =
      (
        ~first_or_last: [ | `First | `Last],
        ~n: int,
        search_param: Search_param.t,
        s: Seq.t(Time_slot.t),
      )
      : Seq.t(Time_slot.t) => {
    let tz_offset_s_of_date_time = search_param.search_using_tz_offset_s;
    s
    |> Seq.map(((x, y)) =>
         (
           Time.Date_time.of_unix_second(~tz_offset_s_of_date_time, x)
           |> Result.get_ok,
           Time.Date_time.of_unix_second(~tz_offset_s_of_date_time, y)
           |> Result.get_ok,
         )
       )
    |> get_first_or_last_n_matches_of_same_month_date_time_pair_seq(
         ~first_or_last,
         ~n,
       )
    |> Seq.map(((x, y)) =>
         (
           Time.Date_time.to_unix_second(x) |> Result.get_ok,
           Time.Date_time.to_unix_second(y) |> Result.get_ok,
         )
       );
  };

  let matching_time_slots =
      (
        ~f_resolve_tpe_name,
        ~f_resolve_tse_name,
        search_param: Search_param.t,
        e: Time_expr_ast.time_slot_expr,
      )
      : result(Seq.t(Time_slot.t), string) =>
    switch (
      Resolve.resolve_time_slot_expr(
        ~f_resolve_tse_name,
        ~f_resolve_tpe_name,
        e,
      )
    ) {
    | Error(msg) => Error(msg)
    | Ok(e) =>
      switch (
        To_time_pattern_lossy.time_range_patterns_of_time_slot_expr(
          ~f_resolve_tse_name,
          ~f_resolve_tpe_name,
          e,
        )
      ) {
      | Error(msg) => Error(msg)
      | Ok(l) =>
        switch (
          Time_pattern.Range_pattern.matching_time_slots_round_robin_non_decreasing(
            ~allow_search_param_override=true,
            search_param,
            l,
          )
        ) {
        | Error(e) => Error(Time_pattern.To_string.string_of_error(e))
        | Ok(s) => s |> Seq.flat_map(List.to_seq) |> Result.ok
        }
      }
    };
};

module Branching_time_slot_expr = {
  let matching_time_slots =
      (
        search_param: Search_param.t,
        e: Time_expr_ast.branching_time_slot_expr,
      )
      : result(Seq.t(Time_slot.t), string) => {
    let rec aux =
            (e: Time_expr_ast.branching_time_slot_expr)
            : result(Seq.t(list(Time_slot.t)), string) =>
      Time_expr_ast.(
        switch (e) {
        | [@implicit_arity] Bts_unary_op(op, e) =>
          aux(e)
          |> Result.map(s =>
               switch (op) {
               | Next_n_batches(n) => OSeq.take(n, s)
               | Every_batch => s
               }
             )
        | _ =>
          switch (
            To_time_pattern_lossy.time_range_patterns_of_branching_time_slot_expr(
              e,
            )
          ) {
          | Error(msg) => Error(msg)
          | Ok(l) =>
            Time_pattern.Range_pattern.matching_time_slots_round_robin_non_decreasing(
              ~allow_search_param_override=true,
              search_param,
              l,
            )
            |> Result.map_error(Time_pattern.To_string.string_of_error)
          }
        }
      );

    aux(e) |> Result.map(s => s |> Seq.flat_map(List.to_seq));
  };
};

let matching_time_slots =
    (
      ~f_resolve_tpe_name=default_f_resolve_tpe_name,
      ~f_resolve_tse_name=default_f_resolve_tse_name,
      search_param: Search_param.t,
      e: Time_expr_ast.t,
    )
    : result(Seq.t(Time_slot.t), string) => {
  let rec aux = (search_param, e) =>
    Time_expr_ast.(
      switch (e) {
      | Time_point_expr(e) =>
        Time_point_expr.matching_unix_seconds(
          ~f_resolve_tpe_name,
          search_param,
          e,
        )
        |> Result.map(Seq.map(x => (x, Int64.succ(x))))
      | Time_slot_expr(e) =>
        Time_slot_expr.matching_time_slots(
          ~f_resolve_tpe_name,
          ~f_resolve_tse_name,
          search_param,
          e,
        )
      | Branching_time_slot_expr(e) =>
        Branching_time_slot_expr.matching_time_slots(search_param, e)
      | Time_pattern(pat) =>
        Time_pattern.Single_pattern.matching_time_slots(
          ~allow_search_param_override=true,
          search_param,
          pat,
        )
        |> Result.map_error(Time_pattern.To_string.string_of_error)
      | [@implicit_arity] Time_unary_op(op, e) =>
        switch (op) {
        | Not =>
          switch (
            Time_pattern.Single_pattern.matching_time_slots(
              ~allow_search_param_override=true,
              search_param,
              Time_pattern.empty,
            )
          ) {
          | Error(x) => Error(Time_pattern.To_string.string_of_error(x))
          | Ok(whole_range) =>
            aux(search_param, e)
            |> Result.map(s =>
                 Time_slots.relative_complement(~not_mem_of=s, whole_range)
               )
          }
        | Every => aux(search_param, e)
        | Next_n_slots(n) =>
          aux(search_param, e) |> Result.map(OSeq.take(n))
        | Next_n_points(n) =>
          aux(search_param, e)
          |> Result.map(s =>
               s
               |> Time_slots.chunk(~skip_check=true, ~chunk_size=1L)
               |> OSeq.take(n)
               |> Time_slots.Normalize.normalize(
                    ~skip_filter_invalid=true,
                    ~skip_filter_empty=true,
                    ~skip_sort=true,
                  )
             )
        | [@implicit_arity] Tz_offset(sign, {hour, minute, second}) =>
          let multiplier =
            switch (sign) {
            | Pos => 1
            | Neg => (-1)
            };
          let offset_s =
            Duration.(
              {...zero, hours: hour, minutes: minute, seconds: second}
              |> to_seconds
              |> Int64.to_int
              |> Int.mul(multiplier)
            );

          let search_param = {
            ...search_param,
            search_using_tz_offset_s: Some(offset_s),
          };

          aux(search_param, e);
        }
      | [@implicit_arity] Time_binary_op(op, e1, e2) =>
        switch (aux(search_param, e1)) {
        | Error(x) => Error(x)
        | Ok(s1) =>
          switch (aux(search_param, e2)) {
          | Error(e) => Error(e)
          | Ok(s2) =>
            Ok(
              switch (op) {
              | Union => Time_slots.Union.union(~skip_check=true, s1, s2)
              | Inter => Time_slots.inter(~skip_check=true, s1, s2)
              },
            )
          }
        }
      | Time_round_robin_select(l) =>
        l
        |> List.map(aux(search_param))
        |> Misc_utils.get_ok_error_list
        |> (
          x =>
            switch (x) {
            | Ok(l) =>
              l
              |> List.to_seq
              |> Time_slots.Round_robin.merge_multi_seq_round_robin_non_decreasing
              |> Result.ok
            | Error(x) => Error(x)
            }
        )
      }
    );

  aux(search_param, e)
  |> Result.map(
       Time_slots.Normalize.normalize(
         ~skip_filter_invalid=true,
         ~skip_filter_empty=true,
         ~skip_sort=true,
       ),
     );
};

let next_match_time_slot =
    (
      ~f_resolve_tpe_name=default_f_resolve_tpe_name,
      ~f_resolve_tse_name=default_f_resolve_tse_name,
      search_param: Search_param.t,
      e: Time_expr_ast.t,
    )
    : result(option(Time_slot.t), string) =>
  switch (
    matching_time_slots(
      ~f_resolve_tse_name,
      ~f_resolve_tpe_name,
      search_param,
      e,
    )
  ) {
  | Error(msg) => Error(msg)
  | Ok(seq) =>
    switch (seq()) {
    | Seq.Nil => Ok(None)
    | [@implicit_arity] Seq.Cons(x, _) => Ok(Some(x))
    }
  };
