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

let push_search_param_to_later_start =
    (~start: int64, search_param: t): result(t, unit) =>
  switch (search_param.typ) {
  | Time_slots(time_slots) =>
    switch (Time_slots.Bound.min_start_and_max_end_exc_list(time_slots)) {
    | None => Ok(search_param)
    | Some((start', end_exc')) =>
      let start = max(start', start);
      let time_slots =
        time_slots
        |> List.to_seq
        |> Time_slots.inter(Seq.return((start, end_exc')))
        |> List.of_seq;

      Ok({...search_param, typ: Time_slots(time_slots)});
    }
  | Years_ahead({years_ahead, start: start'}) =>
    switch (start') {
    | `Unix_second(start') =>
      let start = max(start', start);
      Ok({
        ...search_param,
        typ: Years_ahead({years_ahead, start: `Unix_second(start)}),
      });
    | `Date_time(start') =>
      switch (Time.Date_time.to_unix_second(start')) {
      | Error () => Error()
      | Ok(start') =>
        let start = max(start', start);
        Time.Date_time.of_unix_second(
          ~tz_offset_s_of_date_time=search_param.search_using_tz_offset_s,
          start,
        )
        |> Result.map(start =>
             {
               ...search_param,
               typ: Years_ahead({years_ahead, start: `Date_time(start)}),
             }
           );
      }
    }
  };

let start_date_time_and_search_years_ahead_of_search_param =
    (search_param: t): option((Time.Date_time.t, int)) =>
  switch (search_param.typ) {
  | Time_slots(time_slots) =>
    switch (Time_slots.Bound.min_start_and_max_end_exc_list(time_slots)) {
    | None => None
    | Some((start, end_exc)) =>
      let start =
        Time.Date_time.of_unix_second(
          ~tz_offset_s_of_date_time=search_param.search_using_tz_offset_s,
          start,
        )
        |> Result.get_ok;

      let end_exc =
        Time.Date_time.of_unix_second(
          ~tz_offset_s_of_date_time=search_param.search_using_tz_offset_s,
          end_exc,
        )
        |> Result.get_ok;

      let search_years_ahead = end_exc.year - start.year + 1;
      Some((start, search_years_ahead));
    }
  | Years_ahead({years_ahead, start}) =>
    switch (start) {
    | `Unix_second(start) =>
      let start =
        Time.Date_time.of_unix_second(
          ~tz_offset_s_of_date_time=search_param.search_using_tz_offset_s,
          start,
        )
        |> Result.get_ok;

      Some((start, years_ahead));
    | `Date_time(start) => Some((start, years_ahead))
    }
  };

module Check = {
  let check_search_param = (x: t): result(unit, error) =>
    switch (x.typ) {
    | Time_slots(time_slots) =>
      if (List.for_all(
            ((x, y)) =>
              Time_slot.Check.is_valid((x, y))
              && Time.Date_time.of_unix_second(
                   ~tz_offset_s_of_date_time=None,
                   x,
                 )
              |> Result.is_ok
              && Time.Date_time.of_unix_second(
                   ~tz_offset_s_of_date_time=None,
                   y,
                 )
              |> Result.is_ok,
            time_slots,
          )) {
        Ok();
      } else {
        Error(Invalid_time_slots);
      }
    | Years_ahead({years_ahead, start}) =>
      switch (start) {
      | `Unix_second(start) =>
        switch (
          Time.Date_time.of_unix_second(
            ~tz_offset_s_of_date_time=x.search_using_tz_offset_s,
            start,
          )
        ) {
        | Error () => Error(Invalid_start)
        | Ok(start) =>
          if (years_ahead <= 0) {
            Error(Invalid_search_years_ahead);
          } else if (start.year + years_ahead > Time.Date_time.max.year) {
            Error(Too_far_into_future);
          } else {
            Ok();
          }
        }
      | `Date_time(start) =>
        if (Time.Check.date_time_is_valid(start)) {
          if (years_ahead <= 0) {
            Error(Invalid_search_years_ahead);
          } else if (start.year + years_ahead > Time.Date_time.max.year) {
            Error(Too_far_into_future);
          } else {
            Ok();
          };
        } else {
          Error(Invalid_start);
        }
      }
    };
};

let make_using_time_slots =
    (~search_using_tz_offset_s=?, time_slots: list(Time_slot.t))
    : result(t, error) => {
  let t = {search_using_tz_offset_s, typ: Time_slots(time_slots)};
  switch (Check.check_search_param(t)) {
  | Ok () => Ok(t)
  | Error(e) => Error(e)
  };
};

let make_using_years_ahead =
    (~search_using_tz_offset_s=?, ~start: option(start)=?, years_ahead)
    : result(t, error) => {
  let t = {
    search_using_tz_offset_s,
    typ:
      Years_ahead({
        start:
          Option.value(
            ~default=`Unix_second(Time.Current.cur_unix_second()),
            start,
          ),
        years_ahead,
      }),
  };

  switch (Check.check_search_param(t)) {
  | Ok () => Ok(t)
  | Error(e) => Error(e)
  };
};
