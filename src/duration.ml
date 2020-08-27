open Int64_utils

type raw = {
  days : float;
  hours : float;
  minutes : float;
  seconds : int;
}

type t = {
  days : int;
  hours : int;
  minutes : int;
  seconds : int;
}

let zero : t = { days = 0; hours = 0; minutes = 0; seconds = 0 }

let of_seconds (x : int64) : (t, unit) result =
  if x < 0L then Error ()
  else
    let seconds = Int64.rem x 60L in
    let minutes = Int64.div x 60L in
    let hours = Int64.div minutes 60L in
    let days = Int64.div hours 24L in
    let hours = Int64.rem hours 24L in
    let minutes = Int64.rem minutes 60L in
    Ok
      {
        days = Int64.to_int days;
        hours = Int64.to_int hours;
        minutes = Int64.to_int minutes;
        seconds = Int64.to_int seconds;
      }

let to_seconds (t : t) : int64 =
  let days = Int64.of_int t.days in
  let hours = Int64.of_int t.hours in
  let minutes = Int64.of_int t.minutes in
  let seconds = Int64.of_int t.seconds in
  (days *^ Time.Int64_multipliers.day_to_seconds)
  +^ (hours *^ Time.Int64_multipliers.hour_to_seconds)
  +^ (minutes *^ Time.Int64_multipliers.minute_to_seconds)
  +^ seconds

let seconds_of_raw (r : raw) : int64 =
  (r.days *. Time.Float_multipliers.day_to_seconds)
  +. (r.hours *. Time.Float_multipliers.hour_to_seconds)
  +. (r.minutes *. Time.Float_multipliers.minute_to_seconds)
  |> Int64.of_float
  |> Int64.add (Int64.of_int r.seconds)

let normalize (t : t) : t = t |> to_seconds |> of_seconds |> Result.get_ok

module Of_string = struct
  type duration = t

  open CCParse
  open Parser_components

  let seconds_string : string t =
    alpha_string
    >>= fun x ->
    match
      Misc_utils.prefix_string_match [ ("seconds", ()); ("secs", ()) ] x
    with
    | [] -> fail "String doesn't match keyword representing seconds"
    | _ -> return x

  let minutes_string : string t =
    alpha_string
    >>= fun x ->
    match
      Misc_utils.prefix_string_match [ ("minutes", ()); ("mins", ()) ] x
    with
    | [] -> fail "String doesn't match keyword representing minutes"
    | _ -> return x

  let hours_string : string t =
    alpha_string
    >>= fun x ->
    match Misc_utils.prefix_string_match [ ("hours", ()); ("hrs", ()) ] x with
    | [] -> fail "String doesn't match keyword representing hours"
    | _ -> return x

  let days_string : string t =
    alpha_string
    >>= fun x ->
    match Misc_utils.prefix_string_match [ ("days", ()) ] x with
    | [] -> fail "String doesn't match keyword representing days"
    | _ -> return x

  let check_for_unused_term days hours minutes seconds : unit t =
    let fail' units prev n spaces s cnum =
      match prev with
      | None ->
        failf "Incorrect position for %s term: %f%s%s, pos: %d" units n spaces
          s cnum
      | Some _ ->
        failf "Duplicate use of %s term: %f%s%s, pos: %d" units n spaces s
          cnum
    in
    get_cnum
    >>= fun cnum ->
    try_ (float_non_neg >>= fun n -> take_space >>= fun s -> return (cnum, n, s))
    >>= (fun (cnum, n, spaces) ->
        get_cnum
        >>= fun unit_keyword_cnum ->
        try_ days_string
        >>= (fun s -> fail' "days" days n spaces s cnum)
            <|> ( try_ hours_string
                  >>= fun s -> fail' "hours" hours n spaces s cnum )
            <|> ( try_ minutes_string
                  >>= fun s -> fail' "minutes" minutes n spaces s cnum )
            <|> ( try_ seconds_string
                  >>= fun s -> fail' "seconds" seconds n spaces s cnum )
            <|> non_space_string
        >>= fun s ->
        eoi *> failf "Invalid unit keyword: %s, pos: %d" s unit_keyword_cnum)
        <|> ( any_string
              >>= fun s ->
              eoi
              *> if s = "" then nop else failf "Invalid syntax: %s, pos: %d" s cnum )

  let duration_expr : duration t =
    let term' num p =
      try_ (num <* skip_space <* p)
      >>= (fun n -> return (Some n))
          <|> (try_ (num <* skip_space <* eoi) >>= fun n -> return (Some n))
          <|> return None
    in
    term' float_non_neg days_string
    >>= fun days ->
    skip_space *> term' float_non_neg hours_string
    >>= fun hours ->
    skip_space *> term' float_non_neg minutes_string
    >>= fun minutes ->
    skip_space *> term' nat_zero seconds_string
    >>= fun seconds ->
    skip_space
    *> check_for_unused_term days hours minutes seconds
    *> return
      ( ( {
            days = Option.value ~default:0.0 days;
            hours = Option.value ~default:0.0 hours;
            minutes = Option.value ~default:0.0 minutes;
            seconds = Option.value ~default:0 seconds;
          }
            : raw )
        |> seconds_of_raw
        |> of_seconds
        |> Result.get_ok )

  let of_string (s : string) : (duration, string) result =
    parse_string duration_expr s
end

let of_string = Of_string.of_string

module To_string = struct
  let human_readable_string_of_duration ({ days; hours; minutes; seconds } : t)
    : string =
    if days > 0 then
      Printf.sprintf "%d days %d hours %d mins %d secs" days hours minutes
        seconds
    else if hours > 0 then
      Printf.sprintf "%d hours %d mins %d secs" hours minutes seconds
    else if minutes > 0 then Printf.sprintf "%d mins %d secs" minutes seconds
    else Printf.sprintf "%d secs" seconds
end
