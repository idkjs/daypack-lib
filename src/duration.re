open Int64_utils;

type raw = {
  days: float,
  hours: float,
  minutes: float,
  seconds: int,
};

type t = {
  days: int,
  hours: int,
  minutes: int,
  seconds: int,
};

let zero: t = ({days: 0, hours: 0, minutes: 0, seconds: 0}: t);

let of_seconds = (x: int64): result(t, unit) =>
  if (x < 0L) {
    Error();
  } else {
    let seconds = Int64.rem(x, 60L);
    let minutes = Int64.div(x, 60L);
    let hours = Int64.div(minutes, 60L);
    let days = Int64.div(hours, 24L);
    let hours = Int64.rem(hours, 24L);
    let minutes = Int64.rem(minutes, 60L);
    Ok({
      days: Int64.to_int(days),
      hours: Int64.to_int(hours),
      minutes: Int64.to_int(minutes),
      seconds: Int64.to_int(seconds),
    });
  };

let to_seconds = (t: t): int64 => {
  let days = Int64.of_int(t.days);
  let hours = Int64.of_int(t.hours);
  let minutes = Int64.of_int(t.minutes);
  let seconds = Int64.of_int(t.seconds);
  days
  *^ Time.Int64_multipliers.day_to_seconds
  +^ hours
  *^ Time.Int64_multipliers.hour_to_seconds
  +^ minutes
  *^ Time.Int64_multipliers.minute_to_seconds
  +^ seconds;
};

let seconds_of_raw = (r: raw): int64 =>
  r.days
  *. Time.Float_multipliers.day_to_seconds
  +. r.hours
  *. Time.Float_multipliers.hour_to_seconds
  +. r.minutes
  *. Time.Float_multipliers.minute_to_seconds
  |> Int64.of_float
  |> Int64.add(Int64.of_int(r.seconds));

let normalize = (t: t): t => t |> to_seconds |> of_seconds |> Result.get_ok;

module Of_string = {
  type duration = t;

  open MParser;
  open Parser_components;

  let seconds_string: t(string, unit) = (
    alpha_string
    >>= (
      x =>
        switch (
          Misc_utils.prefix_string_match(
            [("seconds", ()), ("secs", ())],
            x,
          )
        ) {
        | [] => fail("String doesn't match keyword representing seconds")
        | _ => return(x)
        }
    ):
      t(string, unit)
  );

  let minutes_string: t(string, unit) = (
    alpha_string
    >>= (
      x =>
        switch (
          Misc_utils.prefix_string_match(
            [("minutes", ()), ("mins", ())],
            x,
          )
        ) {
        | [] => fail("String doesn't match keyword representing minutes")
        | _ => return(x)
        }
    ):
      t(string, unit)
  );

  let hours_string: t(string, unit) = (
    alpha_string
    >>= (
      x =>
        switch (
          Misc_utils.prefix_string_match([("hours", ()), ("hrs", ())], x)
        ) {
        | [] => fail("String doesn't match keyword representing hours")
        | _ => return(x)
        }
    ):
      t(string, unit)
  );

  let days_string: t(string, unit) = (
    alpha_string
    >>= (
      x =>
        switch (Misc_utils.prefix_string_match([("days", ())], x)) {
        | [] => fail("String doesn't match keyword representing days")
        | _ => return(x)
        }
    ):
      t(string, unit)
  );

  let check_for_unused_term = (days, hours, minutes, seconds): t(unit, unit) => {
    let fail' = (units, prev, n, spaces, s, pos) =>
      switch (prev) {
      | None =>
        fail(
          Printf.sprintf(
            "Incorrect position for %s term: %f%s%s, pos: %s",
            units,
            n,
            spaces,
            s,
            string_of_pos(pos),
          ),
        )
      | Some(_) =>
        fail(
          Printf.sprintf(
            "Duplicate use of %s term: %f%s%s, pos: %s",
            units,
            n,
            spaces,
            s,
            string_of_pos(pos),
          ),
        )
      };

    get_pos
    >>= (
      pos =>
        attempt(
          float_non_neg >>= (n => take_space >>= (s => return((pos, n, s)))),
        )
        >>= (
          ((pos, n, spaces)) =>
            get_pos
            >>= (
              unit_keyword_pos =>
                attempt(days_string)
                >>= (s => fail'("days", days, n, spaces, s, pos))
                <|> (
                  attempt(hours_string)
                  >>= (s => fail'("hours", hours, n, spaces, s, pos))
                )
                <|> (
                  attempt(minutes_string)
                  >>= (s => fail'("minutes", minutes, n, spaces, s, pos))
                )
                <|> (
                  attempt(seconds_string)
                  >>= (s => fail'("seconds", seconds, n, spaces, s, pos))
                )
                <|> non_space_string
                >>= (
                  s =>
                    eof
                    >> fail(
                         Printf.sprintf(
                           "Invalid unit keyword: %s, pos: %s",
                           s,
                           string_of_pos(unit_keyword_pos),
                         ),
                       )
                )
            )
        )
        <|> (
          any_string
          >>= (
            s =>
              eof
              >> (
                if (s == "") {
                  return();
                } else {
                  fail(
                    Printf.sprintf(
                      "Invalid syntax: %s, pos: %s",
                      s,
                      string_of_pos(pos),
                    ),
                  );
                }
              )
          )
        )
    );
  };

  let duration_expr: t(duration, unit) = (
    {
      let term' = (num, p) =>
        attempt(num << spaces << p)
        >>= (n => return(Some(n)))
        <|> (attempt(num << spaces << eof) >>= (n => return(Some(n))))
        <|> return(None);

      term'(float_non_neg, days_string)
      >>= (
        days =>
          spaces
          >> term'(float_non_neg, hours_string)
          >>= (
            hours =>
              spaces
              >> term'(float_non_neg, minutes_string)
              >>= (
                minutes =>
                  spaces
                  >> term'(nat_zero, seconds_string)
                  >>= (
                    seconds =>
                      spaces
                      >> check_for_unused_term(days, hours, minutes, seconds)
                      >> return(
                           (
                             {
                               days: Option.value(~default=0.0, days),
                               hours: Option.value(~default=0.0, hours),
                               minutes: Option.value(~default=0.0, minutes),
                               seconds: Option.value(~default=0, seconds),
                             }: raw
                           )
                           |> seconds_of_raw
                           |> of_seconds
                           |> Result.get_ok,
                         )
                  )
              )
          )
      );
    }:
      t(duration, unit)
  );

  let of_string = (s: string): Result.t(duration, string) =>
    parse_string(duration_expr, s, ()) |> result_of_mparser_result;
};

let duration_expr_parser = Of_string.duration_expr;

let of_string = Of_string.of_string;

module To_string = {
  let human_readable_string_of_duration =
      ({days, hours, minutes, seconds}: t): string =>
    if (days > 0) {
      Printf.sprintf(
        "%d days %d hours %d mins %d secs",
        days,
        hours,
        minutes,
        seconds,
      );
    } else if (hours > 0) {
      Printf.sprintf("%d hours %d mins %d secs", hours, minutes, seconds);
    } else if (minutes > 0) {
      Printf.sprintf("%d mins %d secs", minutes, seconds);
    } else {
      Printf.sprintf("%d secs", seconds);
    };
};
