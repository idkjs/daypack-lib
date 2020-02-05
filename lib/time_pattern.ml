type day =
  | Weekday of int
  | Month_day of int

type t = {
  year : int option;
  mon : int option;
  day : day option;
  hour : int option;
  min : int option;
}

type normalize_dir =
  [ `Start
  | `End
  ]

let first_mday = 1

let normalize_pattern (dir : normalize_dir) t =
  let map_none upper x default_val =
    match x with
    | Some x -> Some x
    | None -> ( match upper with Some _ -> Some default_val | None -> None )
  in
  t
  |> (fun t ->
      {
        t with
        mon = map_none t.year t.mon (match dir with `Start -> 0 | `End -> 11);
      })
  |> (fun t ->
      match dir with
      | `Start -> { t with day = map_none t.mon t.day (Month_day first_mday) }
      | `End ->
        {
          t with
          mon = Option.map succ t.mon;
          day = map_none t.mon t.day (Month_day 0);
        })
  |> (fun t ->
      {
        t with
        hour = map_none t.day t.hour (match dir with `Start -> 0 | `End -> 23);
      })
  |> fun t ->
  {
    t with
    min = map_none t.hour t.min (match dir with `Start -> 0 | `End -> 59);
  }

let normalize_tm tm =
  let _, tm = Unix.mktime tm in
  tm

let next_match_tm ~normalize_dir (t : t) (tm : Unix.tm) : Unix.tm option =
  let bump cur pat ub_exc =
    match pat with
    | Some x -> if cur < x then (false, x) else (true, x)
    | None ->
      let next = succ cur in
      if next < ub_exc then (false, next) else (true, 0)
  in
  let next_is_in_past =
    match t.year with Some x -> x < tm.tm_year + 1900 | None -> false
  in
  if next_is_in_past then None
  else
    let t = normalize_pattern normalize_dir t in
    let tm_sec = 0 in
    let bump_hour, tm_min = bump tm.tm_min t.min 60 in
    let bump_mday, tm_hour =
      if bump_hour then bump tm.tm_hour t.hour 24 else (false, tm.tm_hour)
    in
    let definitely_bump_mon, tm_mday =
      if bump_mday then
        match t.day with
        | Some x -> (
            match x with
            | Weekday x ->
              ( false,
                if tm.tm_wday < x then tm.tm_mday + (tm.tm_wday - x)
                else tm.tm_mday + 7 + (x - tm.tm_wday) )
            | Month_day x -> if tm.tm_mday < x then (false, x) else (true, x) )
        | None -> (false, succ tm.tm_mday)
      else (false, tm.tm_mday)
    in
    (* normalize calculated item thus far *)
    let tm = normalize_tm { tm with tm_sec; tm_min; tm_hour; tm_mday } in
    (* if certain to bump month, then do so,
       otherwise check if tm_mon in normalized tm already matches pattern *)
    let bump_year, tm_mon =
      if definitely_bump_mon then bump tm.tm_mon t.mon 12
      else
        match t.mon with
        | Some x -> if tm.tm_mon < x then (false, x) else (true, x)
        | None -> (false, tm.tm_mon)
    in
    let tm_year = if bump_year then succ tm.tm_year else tm.tm_year in
    { tm with tm_mon; tm_year } |> normalize_tm |> Option.some

let next_match_int64 ?(time_slots : Time_slot.t list = []) ~normalize_dir (t : t) (time : int64) : int64 option =
  Time.time_to_tm time |> next_match_tm ~normalize_dir t
  |> Option.map Time.tm_to_time
  |> (fun time ->
      match time with
      | None -> None
      | Some time ->
        match time_slots with
        | [] -> Some time
        | l ->
          let s =
            l |> List.to_seq
            |> (fun s -> match normalize_dir with | `Start -> Time_slot.slice ~start:time s | `End -> Time_slot.slice ~end_exc:time s)
          in
          match s () with
          | Seq.Nil -> None
          | Seq.Cons ((start, end_exc), _) ->
            match normalize_dir with
            | `Start -> Some start
            | `End -> Some end_exc
    )

(* let matching_time_slots  (t : t) (time_slots : Time_slot.t Seq.t) : Time_slot.t Seq.t =
 *   (\* assume 1 time unit in time slot = 1 minute *\)
 *   let rec aux t time_slots =
 *     match time_slots () with
 *     | Seq.Nil -> Seq.empty
 *     | Seq.Cons ((start, end_exc), rest) ->
 * 
 *   in
 *   aux t time_slots *)

module Print = struct
  let debug_string_of_pattern ?(indent_level = 0) ?(buffer = Buffer.create 4096)
      (t : t) : string =
    let aux = Option.fold ~some:string_of_int ~none:"None" in
    Debug_print.bprintf ~indent_level buffer "time pattern :\n";
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "year : %s\n"
      (aux t.year);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "mon : %s\n"
      (aux t.mon);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "day : %s\n"
      ( match t.day with
        | Some (Month_day x) -> Printf.sprintf "month day %d" x
        | Some (Weekday x) -> Printf.sprintf "weekday %d" x
        | None -> "None" );
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "hour : %s\n"
      (aux t.hour);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "min : %s\n"
      (aux t.min);
    Buffer.contents buffer

  let debug_print_pattern ?(indent_level = 0) t =
    print_string (debug_string_of_pattern ~indent_level t)
end
