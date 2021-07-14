open Int64_utils;

let single_task_seg_shift =
    (
      ~incre,
      ~cur_pos,
      ~task_seg: Task.task_seg,
      time_slots: Seq.t(Time_slot.t),
    )
    : Seq.t(Task.task_seg_place) => {
  let rec aux =
          (
            incre,
            cur_pos,
            (task_seg_id, task_seg_size) as task_seg,
            time_slots,
          ) => {
    let time_slots = Time_slots.Slice.slice(~start=cur_pos, time_slots);
    switch (time_slots()) {
    | Seq.Nil => Seq.empty
    | [@implicit_arity] Seq.Cons((start, end_exc), slots) =>
      if (end_exc -^ start >= task_seg_size) {
        /* if time slot is large enough */
        (
          () =>
            [@implicit_arity]
            Seq.Cons(
              (task_seg_id, start, start +^ task_seg_size),
              aux(incre, start +^ incre, task_seg, time_slots),
            )
        );
      } else {
        /* not big enough, move to next time slot */
        aux(incre, cur_pos, task_seg, slots);
      }
    };
  };

  assert(incre > 0L);
  aux(incre, cur_pos, task_seg, time_slots);
};

let single_task_seg_shift_rev =
    (
      ~incre,
      ~cur_end_pos_exc,
      ~task_seg: Task.task_seg,
      time_slots: Seq.t(Time_slot.t),
    )
    : Seq.t(('a, int64, int64)) => {
  let rec aux =
          (
            incre,
            cur_end_pos_exc,
            (task_seg_id, task_seg_size) as task_seg,
            time_slots,
          ) => {
    let time_slots =
      Time_slots.Slice.slice_rev(~end_exc=cur_end_pos_exc, time_slots);

    switch (time_slots()) {
    | Seq.Nil => Seq.Nil
    | [@implicit_arity] Seq.Cons((start, end_exc), slots) =>
      if (end_exc -^ start >= task_seg_size) {
        /* if time slot is large enough */
        [@implicit_arity]
        Seq.Cons(
          (task_seg_id, end_exc -^ task_seg_size, end_exc),
          () => aux(incre, end_exc -^ incre, task_seg, time_slots),
        );
      } else {
        /* not big enough, move to next time slot */
        aux(incre, cur_end_pos_exc, task_seg, slots);
      }
    };
  };

  assert(incre > 0L);
  let time_slots = time_slots |> List.of_seq |> List.rev |> List.to_seq;
  () => aux(incre, cur_end_pos_exc, task_seg, time_slots);
};

let multi_task_segs_shift =
    (
      ~incre,
      ~task_segs: list(Task.task_seg),
      time_slots: Seq.t(Time_slot.t),
    )
    : Seq.t(list(Task.task_seg_place)) => {
  assert(incre > 0L);
  switch (task_segs) {
  | [] => Seq.empty
  | _ =>
    List.fold_left(
      (places_seq, task_seg) =>
        Seq.flat_map(
          places =>
            switch (places) {
            | [] =>
              single_task_seg_shift(
                ~incre,
                ~cur_pos=0L,
                ~task_seg,
                time_slots,
              )
              |> Seq.map(x => [x])
            | [(last_id, last_start, last_end_exc), ...pos_s] =>
              let time_slots =
                Time_slots.Slice.slice(~start=last_end_exc, time_slots);

              /* costruct next shifter which begins at last end_exc position */
              single_task_seg_shift(
                ~incre,
                ~cur_pos=last_end_exc,
                ~task_seg,
                time_slots,
              )
              /* chain the list to the shifter */
              |> Seq.map(((id, start, end_exc)) =>
                   [
                     (id, start, end_exc),
                     (last_id, last_start, last_end_exc),
                     ...pos_s,
                   ]
                 );
            },
          places_seq,
        ),
      Seq.return([]),
      task_segs,
    )
    |> Seq.map(List.rev)
  };
};

let single_task_seg_single_split =
    (~min_seg_size, ~max_seg_size, ~cur_split_pos, ~task_seg: Task.task_seg)
    : Seq.t((Task.task_seg, Task.task_seg)) => {
  let rec aux =
          (
            min_seg_size,
            max_seg_size,
            cur_split_pos,
            (task_seg_id, task_seg_size) as task_seg,
          ) =>
    if (cur_split_pos >= task_seg_size) {
      Seq.empty;
    } else {
      let l_split_size = cur_split_pos -^ 0L;
      let r_split_size = task_seg_size -^ cur_split_pos;
      if (l_split_size < min_seg_size
          || r_split_size < min_seg_size
          || max_seg_size < l_split_size
          || max_seg_size < r_split_size) {
        aux(min_seg_size, max_seg_size, Int64.succ(cur_split_pos), task_seg);
      } else {
        () =>
          [@implicit_arity]
          Seq.Cons(
            (
              (task_seg_id, l_split_size),
              (Task.Id.succ_task_seg_sub_id(task_seg_id), r_split_size),
            ),
            aux(
              min_seg_size,
              max_seg_size,
              Int64.succ(cur_split_pos),
              task_seg,
            ),
          );
      };
    };

  let (_, task_seg_size) = task_seg;
  assert(min_seg_size > 0L);
  assert(max_seg_size > 0L);
  assert(cur_split_pos >= 0L);
  assert(task_seg_size > 0L);
  let task_seg = Task.Id.init_task_seg_sub_id(task_seg);
  aux(min_seg_size, max_seg_size, cur_split_pos, task_seg);
};

let single_task_seg_multi_splits_exact =
    (
      ~min_seg_size,
      ~max_seg_size,
      ~split_count: int64,
      ~task_seg: Task.task_seg,
    )
    : Seq.t(list(Task.task_seg)) => {
  let (_, task_seg_size) = task_seg;
  assert(min_seg_size > 0L);
  Option.iter(max_seg_size => assert(max_seg_size > 0L), max_seg_size);
  assert(task_seg_size > 0L);
  Seq.fold_left(
    (splits_seq, _) =>
      Seq.flat_map(
        splits =>
          switch (splits) {
          | [] =>
            if (min_seg_size <= task_seg_size) {
              Seq.return([task_seg]);
            } else {
              Seq.empty;
            }
          | [first, ...rest] =>
            let splits_with_first_sub_splits =
              single_task_seg_single_split(
                ~min_seg_size,
                ~max_seg_size=task_seg_size,
                ~cur_split_pos=0L,
                ~task_seg=first,
              )
              |> Seq.map(((s1, s2)) => [s2, s1, ...rest]);

            splits_with_first_sub_splits;
          },
        splits_seq,
      ),
    Seq.return([]),
    Seq_utils.zero_to_n_inc_int64(split_count),
  )
  |> (
    s =>
      switch (max_seg_size) {
      | None => s
      | Some(max_seg_size) =>
        Seq.filter(l => List.for_all(((_, s)) => s <= max_seg_size, l), s)
      }
  )
  |> Seq.map(List.rev);
};

let single_task_seg_multi_splits_max =
    (
      ~min_seg_size,
      ~max_seg_size,
      ~split_count: int64,
      ~task_seg: Task.task_seg,
    )
    : Seq.t(list(Task.task_seg)) =>
  Seq.flat_map(
    split_count =>
      single_task_seg_multi_splits_exact(
        ~min_seg_size,
        ~max_seg_size,
        ~split_count,
        ~task_seg,
      ),
    Seq_utils.zero_to_n_inc_int64(split_count),
  );

let single_task_seg_multi_splits_exact_shift =
    (
      ~min_seg_size,
      ~max_seg_size,
      ~split_count,
      ~incre: int64,
      ~task_seg: Task.task_seg,
      time_slots: Seq.t(Time_slot.t),
    )
    : Seq.t(list(Task.task_seg_place)) =>
  single_task_seg_multi_splits_exact(
    ~min_seg_size,
    ~max_seg_size,
    ~split_count,
    ~task_seg,
  )
  |> Seq.flat_map(task_segs =>
       multi_task_segs_shift(~incre, ~task_segs, time_slots)
     );

let single_task_seg_multi_splits_max_shift =
    (
      ~min_seg_size,
      ~max_seg_size,
      ~split_count,
      ~incre: int64,
      ~task_seg: Task.task_seg,
      time_slots: Seq.t(Time_slot.t),
    )
    : Seq.t(list(Task.task_seg_place)) =>
  single_task_seg_multi_splits_max(
    ~min_seg_size,
    ~max_seg_size,
    ~split_count,
    ~task_seg,
  )
  |> Seq.flat_map(task_segs =>
       multi_task_segs_shift(~incre, ~task_segs, time_slots)
     );

let multi_task_segs_interleave =
    (
      ~interval_size,
      ~task_segs: list(Task.task_seg),
      time_slots: Seq.t(Time_slot.t),
    )
    : Seq.t(Task.task_seg_place) => {
  assert(interval_size > 0L);
  let quota =
    List.fold_left(
      (m, ((id1, id2, id3, id4, _), len)) =>
        Task_seg_id_map.add((id1, id2, id3, id4, None), len, m),
      Task_seg_id_map.empty,
      task_segs,
    );

  switch (task_segs) {
  | [] => Seq.empty
  | _ =>
    let max_round_count = {
      let max_len =
        List.fold_left((acc, (_, len)) => max(acc, len), 0L, task_segs);

      max_len /^ interval_size;
    };

    let max_seg_count =
      Seq.fold_left(
        (acc, (start, end_exc)) =>
          acc +^ (end_exc -^ start) /^ interval_size,
        0L,
        time_slots,
      )
      |> Int64.to_int;

    let time_slots_chunked =
      Time_slots.chunk(
        ~chunk_size=interval_size,
        ~drop_partial=true,
        time_slots,
      );

    let task_segs =
      Seq_utils.zero_to_n_exc_int64(max_round_count)
      |> Seq.flat_map(round =>{
           quota
           |> Task_seg_id_map.to_seq
           /* get task segments usable in this round */
           |> Seq.filter(((_id, len)) =>{
                let quota_left = len -^ round *^ interval_size;
                quota_left >= interval_size;
             } )
           /* update id based on round, drop length */
           |> Seq.map((((id1, id2, id3, id4, _), _)) =>
                (id1, id2, id3, id4, Some(round))
              )
         })
      |> OSeq.take(max_seg_count);

    OSeq.map2(
      (id, (start, end_exc)) => (id, start, end_exc),
      task_segs,
      time_slots_chunked,
    );
  };
};

let single_task_seg_multi_even_splits =
    (
      ~incre,
      ~task_seg: Task.task_seg,
      ~buckets: list(Time_slot.t),
      ~usable_time_slots: Seq.t(Time_slot.t),
    )
    : Seq.t(list(Task.task_seg_place)) => {
  let rec aux = (task_seg_size, n, buckets) =>
    /* try to find maximum number of buckets to fit into */
    if (n == 0L) {
      (None, []);
    } else {
      let bucket_count = List.length(buckets);
      let seg_part_size = Int64_utils.div_round_up(task_seg_size, n);
      let usable_buckets =
        buckets
        |> List.filter(bucket_parts =>
             List.for_all(
               ((start, end_exc)) => end_exc -^ start >= seg_part_size,
               bucket_parts,
             )
           );

      let usable_bucket_count = List.length(usable_buckets);
      if (usable_bucket_count > 0 && usable_bucket_count == bucket_count) {
        (Some(seg_part_size), usable_buckets);
      } else {
        aux(task_seg_size, Int64.pred(n), usable_buckets);
      };
    };

  let ((id1, id2, id3, id4, _), task_seg_size) = task_seg;
  let possibly_usable_buckets =
    buckets
    |> List.map(bucket =>
         Time_slots.inter(Seq.return(bucket), usable_time_slots)
         |> List.of_seq
       );

  let possibly_usable_bucket_count =
    List.length(possibly_usable_buckets) |> Int64.of_int;

  switch (
    aux(task_seg_size, possibly_usable_bucket_count, possibly_usable_buckets)
  ) {
  | (None, _) => Seq.empty
  | (Some(task_seg_part_size), l) =>
    l
    |> List.to_seq
    |> OSeq.mapi((i, bucket) => (Int64.of_int(i), List.to_seq(bucket)))
    |> Seq.fold_left(
         (places_seq, (bucket_id, bucket)) => {
           let id = (id1, id2, id3, id4, Some(bucket_id));
           let task_seg = (id, task_seg_part_size);
           Seq.flat_map(
             places =>
               single_task_seg_shift(~incre, ~cur_pos=0L, ~task_seg, bucket)
               |> Seq.map(place => [place, ...places]),
             places_seq,
           );
         },
         Seq.return([]),
       )
    |> Seq.map(List.rev)
  };
};
