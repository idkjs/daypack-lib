open Int64_utils

module Filter = struct
  let filter_invalid (time_slots : Time_slot.t Seq.t) : Time_slot.t Seq.t =
    Seq.filter (fun (x, y) -> x <= y) time_slots

  let filter_invalid_list (time_slots : Time_slot.t list) : Time_slot.t list =
    List.filter (fun (x, y) -> x <= y) time_slots

  let filter_empty (time_slots : Time_slot.t Seq.t) : Time_slot.t Seq.t =
    Seq.filter (fun (x, y) -> x <> y) time_slots

  let filter_empty_list (time_slots : Time_slot.t list) : Time_slot.t list =
    List.filter (fun (x, y) -> x <> y) time_slots
end

module Sort = struct
  let sort_time_slots_list (time_slots : Time_slot.t list) : Time_slot.t list =
    List.sort Time_slot.compare time_slots

  let sort_uniq_time_slots_list (time_slots : Time_slot.t list) :
    Time_slot.t list =
    List.sort_uniq Time_slot.compare time_slots

  let sort_uniq_time_slots (time_slots : Time_slot.t Seq.t) : Time_slot.t Seq.t
    =
    time_slots |> List.of_seq |> sort_uniq_time_slots_list |> List.to_seq

  let sort_time_slots (time_slots : Time_slot.t Seq.t) : Time_slot.t Seq.t =
    time_slots |> List.of_seq |> sort_time_slots_list |> List.to_seq
end

module Normalize = struct
  let join (time_slots : Time_slot.t Seq.t) : Time_slot.t Seq.t =
    let rec aux last_start_and_last_end_exc time_slots =
      match time_slots () with
      | Seq.Nil -> (
          match last_start_and_last_end_exc with
          | None -> Seq.empty
          | Some (last_start, last_end_exc) ->
            Seq.return (last_start, last_end_exc) )
      | Seq.Cons ((start, end_exc), rest) -> (
          match last_start_and_last_end_exc with
          | None -> aux (Some (start, end_exc)) rest
          | Some (last_start, last_end_exc) -> (
              match
                Time_slot.join (start, end_exc) (last_start, last_end_exc)
              with
              | Some x -> aux (Some x) rest
              | None ->
                (* cannot be merged, add time slot being carried to the sequence *)
                fun () ->
                  Seq.Cons
                    ( (last_start, last_end_exc),
                      aux (Some (start, end_exc)) rest ) ) )
    in
    aux None time_slots

  let normalize ?(skip_filter_invalid = false) ?(skip_filter_empty = false)
      ?(skip_sort = false) time_slots =
    time_slots
    |> (fun s -> if skip_filter_invalid then s else Filter.filter_invalid s)
    |> (fun s -> if skip_filter_empty then s else Filter.filter_empty s)
    |> (fun s -> if skip_sort then s else Sort.sort_uniq_time_slots s)
    |> join

  let normalize_list_in_seq_out ?(skip_filter_invalid = false)
      ?(skip_filter_empty = false) ?(skip_sort = false) time_slots =
    time_slots
    |> List.to_seq
    |> normalize ~skip_filter_invalid ~skip_filter_empty ~skip_sort
end

module Slice_internal = struct
  let slice_start ~start (time_slots : Time_slot.t Seq.t) : Time_slot.t Seq.t =
    let rec aux start time_slots =
      match time_slots () with
      | Seq.Nil -> Seq.empty
      | Seq.Cons ((ts_start, ts_end_exc), rest) ->
        if start <= ts_start then
          (* entire time slot is after start, do nothing *)
          time_slots
        else if ts_start < start && start < ts_end_exc then
          (* time slot spans across the start mark, split time slot *)
          fun () -> Seq.Cons ((start, ts_end_exc), rest)
        else
          (* time slot is before start mark, move to next time slot *)
          aux start rest
    in
    aux start time_slots

  let slice_end_exc ~end_exc (time_slots : Time_slot.t Seq.t) :
    Time_slot.t Seq.t =
    let rec aux end_exc time_slots =
      match time_slots () with
      | Seq.Nil -> Seq.empty
      | Seq.Cons ((ts_start, ts_end_exc), rest) ->
        if end_exc <= ts_start then
          (* entire time slot is after end_exc mark, drop everything *)
          aux end_exc Seq.empty
        else if ts_start < end_exc && end_exc < ts_end_exc then
          (* time slot spans across the end_exc mark, split time slot,
             skip remaining slots *)
          fun () -> Seq.Cons ((ts_start, end_exc), aux end_exc Seq.empty)
        else
          (* time slot is before end_exc, add to sequence and move to next time slot *)
          fun () -> Seq.Cons ((ts_start, ts_end_exc), aux end_exc rest)
    in
    aux end_exc time_slots
end

module Slice_rev_internal = struct
  let slice_start ~start (time_slots : Time_slot.t Seq.t) : Time_slot.t Seq.t =
    let rec aux acc start time_slots =
      match time_slots () with
      | Seq.Nil -> List.rev acc |> List.to_seq
      | Seq.Cons ((ts_start, ts_end_exc), slots) ->
        if start <= ts_start then
          (* entire time slot is after start, add to acc *)
          aux ((ts_start, ts_end_exc) :: acc) start slots
        else if ts_start < start && start < ts_end_exc then
          (* time slot spans across the start mark, split time slot *)
          aux ((start, ts_end_exc) :: acc) start slots
        else
          (* time slot is before start mark, do nothing *)
          aux acc start Seq.empty
    in
    aux [] start time_slots

  let slice_end_exc ~end_exc (time_slots : Time_slot.t Seq.t) :
    Time_slot.t Seq.t =
    let rec aux end_exc time_slots =
      match time_slots () with
      | Seq.Nil -> Seq.empty
      | Seq.Cons ((ts_start, ts_end_exc), slots) ->
        if ts_end_exc <= end_exc then
          (* entire time slot is before end_exc mark, do nothing *)
          time_slots
        else if ts_start < end_exc && end_exc < ts_end_exc then
          (* time slot spans across the end_exc mark, split time slot *)
          OSeq.cons (ts_start, end_exc) slots
        else
          (* time slot is after end_exc mark, move to next time slot *)
          aux end_exc slots
    in
    aux end_exc time_slots
end

module Slice = struct
  let slice ?start ?end_exc time_slots =
    time_slots
    |> (fun l ->
        match start with
        | None -> l
        | Some start -> Slice_internal.slice_start ~start l)
    |> fun l ->
    match end_exc with
    | None -> l
    | Some end_exc -> Slice_internal.slice_end_exc ~end_exc l

  let slice_rev ?start ?end_exc time_slots =
    time_slots
    |> (fun l ->
        match start with
        | None -> l
        | Some start -> Slice_rev_internal.slice_start ~start l)
    |> fun l ->
    match end_exc with
    | None -> l
    | Some end_exc -> Slice_rev_internal.slice_end_exc ~end_exc l
end

let relative_complement ~(mem_of : Time_slot.t Seq.t)
    ~(not_mem_of : Time_slot.t Seq.t) : Time_slot.t Seq.t =
  let rec aux mem_of not_mem_of =
    match (mem_of (), not_mem_of ()) with
    | Seq.Nil, _ -> Seq.empty
    | _, Seq.Nil -> mem_of
    | ( Seq.Cons (mem_of_ts, mem_of_rest),
        Seq.Cons (not_mem_of_ts, not_mem_of_rest) ) -> (
        let mem_of () = Seq.Cons (mem_of_ts, mem_of_rest) in
        let not_mem_of () = Seq.Cons (not_mem_of_ts, not_mem_of_rest) in
        match Time_slot.overlap_of_a_over_b ~a:mem_of_ts ~b:not_mem_of_ts with
        | None, None, None ->
          (* mem_of_ts is empty, drop mem_of_ts *)
          aux mem_of_rest not_mem_of
        | Some _, None, None ->
          (* mem_of_ts is before not_mem_of_ts entirely, output mem_of *)
          fun () -> Seq.Cons (mem_of_ts, aux mem_of_rest not_mem_of)
        | None, None, Some _ ->
          (* not_mem_of_ts is before mem_of entirely, drop not_mem_of_ts *)
          aux mem_of not_mem_of_rest
        | Some (start, end_exc), Some _, None ->
          fun () -> Seq.Cons ((start, end_exc), aux mem_of_rest not_mem_of)
        | None, Some _, None -> aux mem_of_rest not_mem_of
        | None, Some _, Some (start, end_exc) ->
          let mem_of () = Seq.Cons ((start, end_exc), mem_of_rest) in
          aux mem_of not_mem_of_rest
        | Some (start1, end_exc1), _, Some (start2, end_exc2) ->
          let mem_of () = Seq.Cons ((start2, end_exc2), mem_of_rest) in
          fun () -> Seq.Cons ((start1, end_exc1), aux mem_of not_mem_of_rest)
      )
  in
  aux mem_of not_mem_of

let invert ~start ~end_exc (time_slots : Time_slot.t Seq.t) : Time_slot.t Seq.t
  =
  relative_complement
    ~mem_of:(Seq.return (start, end_exc))
    ~not_mem_of:time_slots

let intersect (time_slots1 : Time_slot.t Seq.t)
    (time_slots2 : Time_slot.t Seq.t) : Time_slot.t Seq.t =
  let rec aux time_slots1 time_slots2 : Time_slot.t Seq.t =
    match (time_slots1 (), time_slots2 ()) with
    | Seq.Nil, _ -> Seq.empty
    | _, Seq.Nil -> Seq.empty
    | Seq.Cons ((start1, end_exc1), rest1), Seq.Cons ((start2, end_exc2), rest2)
      ->
      if end_exc1 < start2 then
        (* 1 is before 2 entirely, drop 1, keep 2 *)
        aux rest1 time_slots2
      else if end_exc2 < start1 then
        (* 2 is before 1 entirely, keep 1, drop 2 *)
        aux time_slots1 rest2
      else
        (* there is an overlap or touching *)
        let overlap_start = max start1 start2 in
        let overlap_end_exc = min end_exc1 end_exc2 in
        let s1 = if end_exc1 <= overlap_end_exc then rest1 else time_slots1 in
        let s2 = if end_exc2 <= overlap_end_exc then rest2 else time_slots2 in
        if overlap_start < overlap_end_exc then
          (* there is an overlap *)
          fun () -> Seq.Cons ((overlap_start, overlap_end_exc), aux s1 s2)
        else aux s1 s2
  in
  aux time_slots1 time_slots2

module Merge = struct
  let merge (time_slots1 : Time_slot.t Seq.t) (time_slots2 : Time_slot.t Seq.t)
    : Time_slot.t Seq.t =
    let rec aux time_slots1 time_slots2 =
      match (time_slots1 (), time_slots2 ()) with
      | Seq.Nil, s | s, Seq.Nil -> fun () -> s
      | Seq.Cons (x1, rest1), Seq.Cons (x2, rest2) ->
        let ts1 () = Seq.Cons (x1, rest1) in
        let ts2 () = Seq.Cons (x2, rest2) in
        if Time_slot.le x1 x2 then fun () -> Seq.Cons (x1, aux rest1 ts2)
        else fun () -> Seq.Cons (x2, aux rest2 ts1)
    in
    aux time_slots1 time_slots2

  let merge_multi_seq (time_slot_batches : Time_slot.t Seq.t Seq.t) :
    Time_slot.t Seq.t =
    Seq.fold_left
      (fun acc time_slots -> merge acc time_slots)
      Seq.empty time_slot_batches

  let merge_multi_list (time_slot_batches : Time_slot.t Seq.t list) :
    Time_slot.t Seq.t =
    List.to_seq time_slot_batches |> merge_multi_seq
end

module Round_robin = struct
  let collect_round_robin_non_decreasing (batches : Time_slot.t Seq.t list) :
    Time_slot.t option list Seq.t =
    let rec get_usable_part (cur_start : int64) (seq : Time_slot.t Seq.t) :
      Time_slot.t Seq.t =
      match seq () with
      | Seq.Nil -> Seq.empty
      | Seq.Cons ((start, end_exc), rest) as s ->
        if cur_start <= start then fun () -> s
        else if end_exc <= cur_start then get_usable_part cur_start rest
        else fun () -> Seq.Cons ((cur_start, end_exc), rest)
    in
    let rec aux (cur_start : int64 option) (batches : Time_slot.t Seq.t list) :
      Time_slot.t option list Seq.t =
      let cur_start, acc, new_batches =
        List.fold_left
          (fun (cur_start, acc, new_batches) seq ->
             let usable =
               match cur_start with
               | None -> seq
               | Some cur_start -> get_usable_part cur_start seq
             in
             match usable () with
             | Seq.Nil -> (cur_start, None :: acc, new_batches)
             | Seq.Cons ((start, end_exc), rest) ->
               (Some start, Some (start, end_exc) :: acc, rest :: new_batches))
          (cur_start, [], []) batches
      in
      let acc = List.rev acc in
      let new_batches = List.rev new_batches in
      fun () -> Seq.Cons (acc, aux cur_start new_batches)
    in
    aux None batches

  let merge_multi_list_round_robin_non_decreasing
      (batches : Time_slot.t Seq.t list) : Time_slot.t Seq.t =
    collect_round_robin_non_decreasing batches
    |> Seq.flat_map (fun l -> List.to_seq l |> Seq.filter_map (fun x -> x))

  let merge_multi_seq_round_robin_non_decreasing
      (batches : Time_slot.t Seq.t Seq.t) : Time_slot.t Seq.t =
    batches |> List.of_seq |> merge_multi_list_round_robin_non_decreasing
end

module Union = struct
  let union time_slots1 time_slots2 =
    Merge.merge time_slots1 time_slots2
    |> Normalize.normalize ~skip_filter_invalid:true ~skip_filter_empty:true
      ~skip_sort:true

  let union_multi_seq (time_slot_batches : Time_slot.t Seq.t Seq.t) :
    Time_slot.t Seq.t =
    Seq.fold_left
      (fun acc time_slots -> union acc time_slots)
      Seq.empty time_slot_batches

  let union_multi_list (time_slot_batches : Time_slot.t Seq.t list) :
    Time_slot.t Seq.t =
    List.to_seq time_slot_batches |> union_multi_seq
end

let chunk ~chunk_size ?(drop_partial = false) (time_slots : Time_slot.t Seq.t) :
  Time_slot.t Seq.t =
  let rec aux time_slots =
    match time_slots () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons ((start, end_exc), rest) ->
      let chunk_end_exc = min end_exc (start +^ chunk_size) in
      let size = chunk_end_exc -^ start in
      if size = 0L || (size < chunk_size && drop_partial) then aux rest
      else
        let rest () = Seq.Cons ((chunk_end_exc, end_exc), rest) in
        fun () -> Seq.Cons ((start, chunk_end_exc), aux rest)
  in
  aux time_slots

module Sum = struct
  let sum_length (time_slots : Time_slot.t Seq.t) : int64 =
    Seq.fold_left
      (fun acc (start, end_exc) -> acc +^ (end_exc -^ start))
      0L time_slots

  let sum_length_list (time_slots : Time_slot.t list) : int64 =
    time_slots |> List.to_seq |> sum_length
end

module Bound = struct
  let min_start_and_max_end_exc (time_slots : Time_slot.t Seq.t) :
    (int64 * int64) option =
    Seq.fold_left
      (fun acc (start, end_exc) ->
         match acc with
         | None -> Some (start, end_exc)
         | Some (min_start, max_end_exc) ->
           Some (min min_start start, max max_end_exc end_exc))
      None time_slots

  let min_start_and_max_end_exc_list (time_slots : Time_slot.t list) :
    (int64 * int64) option =
    time_slots |> List.to_seq |> min_start_and_max_end_exc
end

let shift_list ~offset (time_slots : Time_slot.t list) : Time_slot.t list =
  List.map
    (fun (start, end_exc) -> (start +^ offset, end_exc +^ offset))
    time_slots

let equal (time_slots1 : Time_slot.t list) (time_slots2 : Time_slot.t list) :
  bool =
  let time_slots1 =
    time_slots1 |> List.to_seq |> Normalize.normalize |> List.of_seq
  in
  let time_slots2 =
    time_slots2 |> List.to_seq |> Normalize.normalize |> List.of_seq
  in
  time_slots1 = time_slots2

let a_is_subset_of_b ~(a : Time_slot.t Seq.t) ~(b : Time_slot.t Seq.t) : bool =
  let inter = intersect a b |> List.of_seq in
  let a = List.of_seq a in
  a = inter

let count_overlap (time_slots : Time_slot.t Seq.t) :
  (Time_slot.t * int) Seq.t =
  let flatten_buffer buffer =
    buffer
    |> List.sort (fun (x, _count) (y, _count) -> Time_slot.compare x y)
    |> List.to_seq
    |> Seq.flat_map (fun (x, count) ->
        OSeq.(0 --^ count) |> Seq.map (fun _ -> x))
  in
  let flush_buffer_to_input buffer time_slots =
    Merge.merge (flatten_buffer buffer) time_slots
  in
  let rec aux (cur : ((int64 * int64) * int) option)
      (buffer : ((int64 * int64) * int) list) (time_slots : Time_slot.t Seq.t) :
    (Time_slot.t * int) Seq.t =
    match time_slots () with
    | Seq.Nil -> (
        match buffer with
        | [] -> (
            match cur with None -> Seq.empty | Some cur -> Seq.return cur )
        | buffer -> aux cur [] (flatten_buffer buffer) )
    | Seq.Cons (x, rest) -> (
        let s () = Seq.Cons (x, rest) in
        match cur with
        | None -> (
            match buffer with
            | [] -> aux (Some (x, 1)) [] rest
            | buffer -> aux None [] (flush_buffer_to_input buffer s) )
        | Some ((cur_start, cur_end_exc), cur_count) -> (
            match
              Time_slot.overlap_of_a_over_b ~a:x ~b:(cur_start, cur_end_exc)
            with
            | None, None, None -> aux cur buffer rest
            | Some _, _, _ ->
              raise (Invalid_argument "Time slots are not sorted")
            | None, Some (start, end_exc), None
            | None, Some (start, _), Some (_, end_exc) ->
              if start = cur_start then
                if end_exc < cur_end_exc then
                  raise (Invalid_argument "Time slots are not sorted")
                else if end_exc = cur_end_exc then
                  aux
                    (Some ((cur_start, cur_end_exc), succ cur_count))
                    buffer rest
                else
                  aux
                    (Some ((cur_start, cur_end_exc), succ cur_count))
                    (((cur_end_exc, end_exc), 1) :: buffer)
                    rest
              else fun () ->
                Seq.Cons
                  ( ((cur_start, start), cur_count),
                    let buffer =
                      if end_exc < cur_end_exc then
                        ((start, end_exc), succ cur_count)
                        :: ((end_exc, cur_end_exc), cur_count)
                        :: buffer
                      else if end_exc = cur_end_exc then
                        ((start, cur_end_exc), succ cur_count) :: buffer
                      else
                        ((start, cur_end_exc), succ cur_count)
                        :: ((cur_end_exc, end_exc), 1)
                        :: buffer
                    in
                    aux None buffer rest )
            | None, None, Some _ ->
              fun () ->
                Seq.Cons
                  (((cur_start, cur_end_exc), cur_count), aux None buffer s) )
      )
  in
  time_slots
  |> aux None []

module Serialize = struct
  let pack_time_slots time_slots =
    List.map Time_slot.Serialize.pack_time_slot time_slots
end

module Deserialize = struct
  let unpack_time_slots time_slots =
    List.map Time_slot.Deserialize.unpack_time_slot time_slots
end
