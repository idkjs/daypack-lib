open Int64_utils;

let brute_force_single =
    (
      ~start,
      ~end_exc,
      ~base: Sched.sched,
      (_sched_req_id, sched_req_record_data_list): Sched_req.sched_req_record,
    )
    : Seq.t(Sched.sched) => {
  let task_seg_is_parallelizable =
      ((task_seg_id, _data): Task.task_seg): bool => {
    let task_id = Task.Id.task_id_of_task_seg_id(task_seg_id);
    let task = Sched.Task.Find.find_task_any_opt(task_id, base) |> Option.get;
    task.parallelizable;
  };

  let get_usable_time_slots = (task_segs: list(Task.task_seg), time_slots) => {
    let is_parallelizable =
      List.for_all(task_seg_is_parallelizable, task_segs);
    let time_slot_candidates =
      if (is_parallelizable) {
        Seq.return((start, end_exc));
      } else {
        Sched.Agenda.Time_slot.get_free_time_slots(~start, ~end_exc, base);
      };

    time_slots
    |> Time_slots.Normalize.normalize_list_in_seq_out
    |> Time_slots.inter(time_slot_candidates);
  };

  Seq.flat_map(
    sched_req_record_data =>
      switch (sched_req_record_data) {
      | Sched_req_data_unit_skeleton.Fixed({
          task_seg_related_data: task_seg,
          start,
        }) =>
        let (_, size) = task_seg;
        let usable_time_slots =
          get_usable_time_slots([task_seg], [(start, start +^ size)]);

        Task_seg_place_gens.multi_task_segs_shift(
          ~incre=1L,
          ~task_segs=[task_seg],
          usable_time_slots,
        )
        |> OSeq.map(places =>
             base |> Sched.Agenda.Add.add_task_seg_place_list(places)
           );
      | Shift(x) =>
        let usable_time_slots =
          get_usable_time_slots(x.task_seg_related_data_list, x.time_slots);

        Task_seg_place_gens.multi_task_segs_shift(
          ~incre=x.incre,
          ~task_segs=x.task_seg_related_data_list,
          usable_time_slots,
        )
        |> OSeq.map(places =>
             base |> Sched.Agenda.Add.add_task_seg_place_list(places)
           );
      | Split_and_shift(x) =>
        let usable_time_slots =
          get_usable_time_slots([x.task_seg_related_data], x.time_slots);

        (
          switch (x.split_count) {
          | Max_split(split_count) =>
            Task_seg_place_gens.single_task_seg_multi_splits_max_shift(
              ~min_seg_size=x.min_seg_size,
              ~max_seg_size=x.max_seg_size,
              ~split_count,
              ~incre=x.incre,
              ~task_seg=x.task_seg_related_data,
              usable_time_slots,
            )
          | Exact_split(split_count) =>
            Task_seg_place_gens.single_task_seg_multi_splits_exact_shift(
              ~min_seg_size=x.min_seg_size,
              ~max_seg_size=x.max_seg_size,
              ~split_count,
              ~incre=x.incre,
              ~task_seg=x.task_seg_related_data,
              usable_time_slots,
            )
          }
        )
        |> OSeq.map(places =>
             base |> Sched.Agenda.Add.add_task_seg_place_list(places)
           );
      | Split_even(x) =>
        let usable_time_slots =
          get_usable_time_slots([x.task_seg_related_data], x.time_slots);

        Task_seg_place_gens.single_task_seg_multi_even_splits(
          ~incre=x.incre,
          ~task_seg=x.task_seg_related_data,
          ~buckets=x.buckets,
          ~usable_time_slots,
        )
        |> OSeq.map(places =>
             base |> Sched.Agenda.Add.add_task_seg_place_list(places)
           );
      | Time_share(x) =>
        let usable_time_slots =
          get_usable_time_slots(x.task_seg_related_data_list, x.time_slots);

        let s =
          Task_seg_place_gens.multi_task_segs_interleave(
            ~interval_size=x.interval_size,
            ~task_segs=x.task_seg_related_data_list,
            usable_time_slots,
          );

        Seq.return(base |> Sched.Agenda.Add.add_task_seg_place_seq(s));
      | Push_toward(x) =>
        let usable_time_slots =
          get_usable_time_slots([x.task_seg_related_data], x.time_slots);

        let s1 =
          Task_seg_place_gens.single_task_seg_shift(
            ~cur_pos=x.target,
            ~incre=x.incre,
            ~task_seg=x.task_seg_related_data,
            usable_time_slots,
          )
          |> OSeq.take(1);

        let s2 =
          Task_seg_place_gens.single_task_seg_shift_rev(
            ~cur_end_pos_exc=x.target,
            ~incre=x.incre,
            ~task_seg=x.task_seg_related_data,
            usable_time_slots,
          )
          |> OSeq.take(1);

        let s =
          OSeq.sorted_merge(
            ~cmp=
              ((_id1, start1, end_exc1), (_id2, start2, end_exc2)) => {
                let distance1 = {
                  let mid1 = (end_exc1 +^ start1) /^ 2L;
                  Int64.abs(mid1 -^ x.target);
                };

                let distance2 = {
                  let mid2 = (end_exc2 +^ start2) /^ 2L;
                  Int64.abs(mid2 -^ x.target);
                };

                compare(distance1, distance2);
              },
            s1,
            s2,
          )
          |> OSeq.take(1);

        Seq.return(base |> Sched.Agenda.Add.add_task_seg_place_seq(s));
      },
    sched_req_record_data_list |> List.to_seq,
  );
};

let backtracking_search_multi =
    (
      ~start,
      ~end_exc,
      ~base,
      sched_req_records: list(Sched_req.sched_req_record),
    )
    : Seq.t(Sched.sched) =>
  sched_req_records
  |> Sched_req.sort_sched_req_record_list_by_flexibility_score
  |> List.fold_left(
       (sched_seq, sched_req) =>
         Seq.flat_map(
           sched =>
             brute_force_single(~start, ~end_exc, ~base=sched, sched_req),
           sched_seq,
         ),
       Seq.return(base),
     );

let backtracking_search_pending =
    (
      ~start,
      ~end_exc,
      ~include_sched_reqs_starting_within_time_slot,
      ~include_sched_reqs_ending_within_time_slot,
      ~up_to_sched_req_id_inc,
      ~base,
    )
    : Seq.t(Sched.sched) => {
  let (sched_req_records, base) =
    Sched.Sched_req.Allocate_task_segs.allocate_task_segs_for_pending_sched_reqs(
      ~start,
      ~end_exc,
      ~include_sched_reqs_starting_within_time_slot,
      ~include_sched_reqs_ending_within_time_slot,
      ~up_to_sched_req_id_inc,
      base,
    );

  backtracking_search_multi(~start, ~end_exc, ~base, sched_req_records);
};
