open Int64_utils

type t = {
  days : int;
  hours : int;
  minutes : int;
  seconds : int;
}

let of_seconds (x : int64) : t =
  assert (x >= 0L);
  let seconds = Int64.rem x 60L in
  let minutes = Int64.div x 60L in
  let hours = Int64.div minutes 60L in
  let days = Int64.div hours 24L in
  let hours = Int64.rem hours 24L in
  let minutes = Int64.rem minutes 60L in
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
  (days *^ Time.day_to_second_multiplier)
  +^ (hours *^ Time.hour_to_second_multiplier)
  +^ (minutes *^ Time.minute_to_second_multiplier)
  +^ seconds

let normalize (t : t) : t = t |> to_seconds |> of_seconds

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

  let check_for_unused_term : unit t =
    let fail' units n spaces s cnum =
      failf "Incorrect position for %s term: %d%s%s, pos: %d" units n spaces s
        cnum
    in
    skip_space
    *> ( try_
           ( get_cnum
             >>= fun cnum ->
             nat_zero >>= fun n -> take_space >>= fun s -> return (cnum, n, s) )
         >>= (fun (cnum, n, spaces) ->
             try_ days_string
             >>= (fun s -> fail' "days" n spaces s cnum)
                 <|> (try_ hours_string >>= fun s -> fail' "hours" n spaces s cnum)
                 <|> ( try_ minutes_string
                       >>= fun s -> fail' "minutes" n spaces s cnum )
                 <|> ( try_ seconds_string
                       >>= fun s -> fail' "seconds" n spaces s cnum )
                 <|> alpha_string
             >>= fun s ->
             eoi *> failf "Invalid unit keyword: %s, pos: %d" s cnum)
             <|> ( get_cnum
                   >>= fun cnum ->
                   any_string
                   >>= fun s -> eoi *> failf "Invalid syntax: %s, pos: %d" s cnum ) )

  let duration_expr : duration t =
    option 0 (nat_zero <* skip_space <* days_string)
    >>= fun days ->
    skip_space *> option 0 (nat_zero <* skip_space <* hours_string)
    >>= fun hours ->
    skip_space *> option 0 (nat_zero <* skip_space <* minutes_string)
    >>= fun minutes ->
    skip_space *> option 0 (nat_zero <* skip_space <* seconds_string)
    >>= fun seconds ->
    check_for_unused_term
    *> return (normalize { days; hours; minutes; seconds })

  let of_string (s : string) : (duration, string) result =
    parse_string duration_expr s
end

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
