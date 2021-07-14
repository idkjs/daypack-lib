open Int64_utils;

exception Time_slots_are_not_sorted;

exception Time_slots_are_not_disjoint;

module Check = {
  let check_if_valid = (time_slots: Seq.t(Time_slot.t)): Seq.t(Time_slot.t) =>
    Seq.map(Time_slot.Check.check_if_valid, time_slots);

  let check_if_not_empty =
      (time_slots: Seq.t(Time_slot.t)): Seq.t(Time_slot.t) =>
    Seq.map(Time_slot.Check.check_if_not_empty, time_slots);

  let check_if_sorted = (time_slots: Seq.t(Time_slot.t)): Seq.t(Time_slot.t) =>
    Seq_utils.check_if_f_holds_for_immediate_neighbors(
      ~f=Time_slot.le,
      ~f_exn=(_, _) => Time_slots_are_not_sorted,
      time_slots,
    );

  let check_if_sorted_rev =
      (time_slots: Seq.t(Time_slot.t)): Seq.t(Time_slot.t) =>
    Seq_utils.check_if_f_holds_for_immediate_neighbors(
      ~f=Time_slot.ge,
      ~f_exn=(_, _) => Time_slots_are_not_sorted,
      time_slots,
    );

  let check_if_disjoint =
      (time_slots: Seq.t(Time_slot.t)): Seq.t(Time_slot.t) =>
    Seq_utils.check_if_f_holds_for_immediate_neighbors(
      ~f=
        (x, y) =>
          switch (Time_slot.overlap_of_a_over_b(~a=y, ~b=x)) {
          | (None, None, None)
          | (Some(_), None, None)
          | (None, None, Some(_)) => true
          | _ => false
          },
      ~f_exn=(_, _) => Time_slots_are_not_disjoint,
      time_slots,
    );

  let check_if_normalized =
      (time_slots: Seq.t(Time_slot.t)): Seq.t(Time_slot.t) =>
    time_slots
    |> check_if_valid
    |> check_if_not_empty
    |> check_if_sorted
    |> check_if_disjoint;
};

module Filter = {
  let filter_invalid = (time_slots: Seq.t(Time_slot.t)): Seq.t(Time_slot.t) =>
    Seq.filter(Time_slot.Check.is_valid, time_slots);

  let filter_invalid_list =
      (time_slots: list(Time_slot.t)): list(Time_slot.t) =>
    List.filter(Time_slot.Check.is_valid, time_slots);

  let filter_empty = (time_slots: Seq.t(Time_slot.t)): Seq.t(Time_slot.t) =>
    Seq.filter(Time_slot.Check.is_not_empty, time_slots);

  let filter_empty_list = (time_slots: list(Time_slot.t)): list(Time_slot.t) =>
    List.filter(Time_slot.Check.is_not_empty, time_slots);
};

module Sort = {
  let sort_time_slots_list =
      (~skip_check=false, time_slots: list(Time_slot.t)): list(Time_slot.t) =>
    time_slots
    |> (
      l =>
        if (skip_check) {
          l;
        } else {
          l |> List.to_seq |> Check.check_if_valid |> List.of_seq;
        }
    )
    |> List.sort(Time_slot.compare);

  let sort_uniq_time_slots_list =
      (~skip_check=false, time_slots: list(Time_slot.t)): list(Time_slot.t) =>
    time_slots
    |> (
      l =>
        if (skip_check) {
          l;
        } else {
          l |> List.to_seq |> Check.check_if_valid |> List.of_seq;
        }
    )
    |> List.sort_uniq(Time_slot.compare);

  let sort_uniq_time_slots =
      (~skip_check=false, time_slots: Seq.t(Time_slot.t))
      : Seq.t(Time_slot.t) =>
    time_slots
    |> (
      s =>
        if (skip_check) {
          s;
        } else {
          Check.check_if_valid(s);
        }
    )
    |> List.of_seq
    |> List.sort_uniq(Time_slot.compare)
    |> List.to_seq;

  let sort_time_slots =
      (~skip_check=false, time_slots: Seq.t(Time_slot.t))
      : Seq.t(Time_slot.t) =>
    time_slots
    |> (
      s =>
        if (skip_check) {
          s;
        } else {
          Check.check_if_valid(s);
        }
    )
    |> List.of_seq
    |> List.sort(Time_slot.compare)
    |> List.to_seq;
};

module Join_internal = {
  let join = (time_slots: Seq.t(Time_slot.t)): Seq.t(Time_slot.t) => {
    let rec aux = (cur, time_slots) =>
      switch (time_slots()) {
      | Seq.Nil =>
        switch (cur) {
        | None => Seq.empty
        | Some(x) => Seq.return(x)
        }
      | [@implicit_arity] Seq.Cons((start, end_exc), rest) =>
        switch (cur) {
        | None => aux(Some((start, end_exc)), rest)
        | Some(cur) =>
          switch (Time_slot.join(cur, (start, end_exc))) {
          | Some(x) => aux(Some(x), rest)
          | None =>
            /* cannot be merged, add time slot being carried to the sequence */
            (
              () =>
                [@implicit_arity]
                Seq.Cons(cur, aux(Some((start, end_exc)), rest))
            )
          }
        }
      };

    aux(None, time_slots);
  };
};

let join = (~skip_check=false, time_slots) =>
  time_slots
  |> (
    s =>
      if (skip_check) {
        s;
      } else {
        s |> Check.check_if_valid |> Check.check_if_sorted;
      }
  )
  |> Join_internal.join;

module Normalize = {
  let normalize =
      (
        ~skip_filter_invalid=false,
        ~skip_filter_empty=false,
        ~skip_sort=false,
        time_slots,
      ) =>
    time_slots
    |> (
      s =>
        if (skip_filter_invalid) {
          s;
        } else {
          Filter.filter_invalid(s);
        }
    )
    |> (
      s =>
        if (skip_filter_empty) {
          s;
        } else {
          Filter.filter_empty(s);
        }
    )
    |> (
      s =>
        if (skip_sort) {
          s;
        } else {
          Sort.sort_uniq_time_slots(s);
        }
    )
    |> Join_internal.join;

  let normalize_list_in_seq_out =
      (
        ~skip_filter_invalid=false,
        ~skip_filter_empty=false,
        ~skip_sort=false,
        time_slots,
      ) =>
    time_slots
    |> List.to_seq
    |> normalize(~skip_filter_invalid, ~skip_filter_empty, ~skip_sort);
};

module Slice_internal = {
  let slice_start =
      (~start, time_slots: Seq.t(Time_slot.t)): Seq.t(Time_slot.t) => {
    let rec aux = (start, time_slots) =>
      switch (time_slots()) {
      | Seq.Nil => Seq.empty
      | [@implicit_arity] Seq.Cons((ts_start, ts_end_exc), rest) =>
        if (start <= ts_start) {
          /* entire time slot is after start, do nothing */
          time_slots;
        } else if (ts_start < start && start < ts_end_exc) {
          /* time slot spans across the start mark, split time slot */
          (() => [@implicit_arity] Seq.Cons((start, ts_end_exc), rest));
        } else {
          /* time slot is before start mark, move to next time slot */
          aux(start, rest);
        }
      };

    aux(start, time_slots);
  };

  let slice_end_exc =
      (~end_exc, time_slots: Seq.t(Time_slot.t)): Seq.t(Time_slot.t) => {
    let rec aux = (end_exc, time_slots) =>
      switch (time_slots()) {
      | Seq.Nil => Seq.empty
      | [@implicit_arity] Seq.Cons((ts_start, ts_end_exc), rest) =>
        if (end_exc <= ts_start) {
          /* entire time slot is after end_exc mark, drop everything */
          aux(end_exc, Seq.empty);
        } else if (ts_start < end_exc && end_exc < ts_end_exc) {
          /* time slot spans across the end_exc mark, split time slot,
             skip remaining slots */
          (
            () =>
              [@implicit_arity]
              Seq.Cons((ts_start, end_exc), aux(end_exc, Seq.empty))
          );
        } else {
          /* time slot is before end_exc, add to sequence and move to next time slot */
          (
            () =>
              [@implicit_arity]
              Seq.Cons((ts_start, ts_end_exc), aux(end_exc, rest))
          );
        }
      };

    aux(end_exc, time_slots);
  };
};

module Slice_rev_internal = {
  let slice_start =
      (~start, time_slots: Seq.t(Time_slot.t)): Seq.t(Time_slot.t) => {
    let rec aux = (acc, start, time_slots) =>
      switch (time_slots()) {
      | Seq.Nil => List.rev(acc) |> List.to_seq
      | [@implicit_arity] Seq.Cons((ts_start, ts_end_exc), slots) =>
        if (start <= ts_start) {
          /* entire time slot is after start, add to acc */
          aux([(ts_start, ts_end_exc), ...acc], start, slots);
        } else if (ts_start < start && start < ts_end_exc) {
          /* time slot spans across the start mark, split time slot */
          aux([(start, ts_end_exc), ...acc], start, slots);
        } else {
          /* time slot is before start mark, do nothing */
          aux(acc, start, Seq.empty);
        }
      };

    aux([], start, time_slots);
  };

  let slice_end_exc =
      (~end_exc, time_slots: Seq.t(Time_slot.t)): Seq.t(Time_slot.t) => {
    let rec aux = (end_exc, time_slots) =>
      switch (time_slots()) {
      | Seq.Nil => Seq.empty
      | [@implicit_arity] Seq.Cons((ts_start, ts_end_exc), slots) =>
        if (ts_end_exc <= end_exc) {
          /* entire time slot is before end_exc mark, do nothing */
          time_slots;
        } else if (ts_start < end_exc && end_exc < ts_end_exc) {
          /* time slot spans across the end_exc mark, split time slot */
          OSeq.cons((ts_start, end_exc), slots);
        } else {
          /* time slot is after end_exc mark, move to next time slot */
          aux(end_exc, slots);
        }
      };

    aux(end_exc, time_slots);
  };
};

module Slice = {
  let slice = (~skip_check=false, ~start=?, ~end_exc=?, time_slots) =>
    time_slots
    |> (
      s =>
        if (skip_check) {
          s;
        } else {
          s
          |> Check.check_if_valid
          |> Check.check_if_disjoint
          |> Check.check_if_sorted;
        }
    )
    |> (
      s =>
        switch (start) {
        | None => s
        | Some(start) => Slice_internal.slice_start(~start, s)
        }
    )
    |> (
      s =>
        switch (end_exc) {
        | None => s
        | Some(end_exc) => Slice_internal.slice_end_exc(~end_exc, s)
        }
    );

  let slice_rev = (~skip_check=false, ~start=?, ~end_exc=?, time_slots) =>
    time_slots
    |> (
      s =>
        if (skip_check) {
          s;
        } else {
          s
          |> Check.check_if_valid
          |> Check.check_if_disjoint
          |> Check.check_if_sorted_rev;
        }
    )
    |> (
      s =>
        switch (start) {
        | None => s
        | Some(start) => Slice_rev_internal.slice_start(~start, s)
        }
    )
    |> (
      s =>
        switch (end_exc) {
        | None => s
        | Some(end_exc) => Slice_rev_internal.slice_end_exc(~end_exc, s)
        }
    );
};

let relative_complement =
    (
      ~skip_check=false,
      ~not_mem_of: Seq.t(Time_slot.t),
      mem_of: Seq.t(Time_slot.t),
    )
    : Seq.t(Time_slot.t) => {
  let rec aux = (mem_of, not_mem_of) =>
    switch (mem_of(), not_mem_of()) {
    | (Seq.Nil, _) => Seq.empty
    | (_, Seq.Nil) => mem_of
    | (
        [@implicit_arity] Seq.Cons(mem_of_ts, mem_of_rest),
        [@implicit_arity] Seq.Cons(not_mem_of_ts, not_mem_of_rest),
      ) =>
      let mem_of = () => [@implicit_arity] Seq.Cons(mem_of_ts, mem_of_rest);
      let not_mem_of = () =>
        [@implicit_arity] Seq.Cons(not_mem_of_ts, not_mem_of_rest);
      switch (Time_slot.overlap_of_a_over_b(~a=mem_of_ts, ~b=not_mem_of_ts)) {
      | (None, None, None) =>
        /* mem_of_ts is empty, drop mem_of_ts */
        aux(mem_of_rest, not_mem_of)
      | (Some(_), None, None) =>
        /* mem_of_ts is before not_mem_of_ts entirely, output mem_of */
        (
          () =>
            [@implicit_arity]
            Seq.Cons(mem_of_ts, aux(mem_of_rest, not_mem_of))
        )
      | (None, None, Some(_)) =>
        /* not_mem_of_ts is before mem_of entirely, drop not_mem_of_ts */
        aux(mem_of, not_mem_of_rest)
      | (Some((start, end_exc)), Some(_), None) => (
          () =>
            [@implicit_arity]
            Seq.Cons((start, end_exc), aux(mem_of_rest, not_mem_of))
        )
      | (None, Some(_), None) => aux(mem_of_rest, not_mem_of)
      | (None, Some(_), Some((start, end_exc))) =>
        let mem_of = () =>
          [@implicit_arity] Seq.Cons((start, end_exc), mem_of_rest);
        aux(mem_of, not_mem_of_rest);
      | (Some((start1, end_exc1)), _, Some((start2, end_exc2))) =>
        let mem_of = () =>
          [@implicit_arity] Seq.Cons((start2, end_exc2), mem_of_rest);
        (
          () =>
            [@implicit_arity]
            Seq.Cons((start1, end_exc1), aux(mem_of, not_mem_of_rest))
        );
      };
    };

  let mem_of =
    if (skip_check) {
      mem_of;
    } else {
      mem_of
      |> Check.check_if_valid
      |> Check.check_if_disjoint
      |> Check.check_if_sorted;
    };

  let not_mem_of =
    if (skip_check) {
      not_mem_of;
    } else {
      not_mem_of
      |> Check.check_if_valid
      |> Check.check_if_disjoint
      |> Check.check_if_sorted;
    };

  aux(mem_of, not_mem_of);
};

let invert =
    (~skip_check=false, ~start, ~end_exc, time_slots: Seq.t(Time_slot.t))
    : Seq.t(Time_slot.t) =>
  relative_complement(
    ~skip_check,
    ~not_mem_of=time_slots,
    Seq.return((start, end_exc)),
  );

let inter =
    (
      ~skip_check=false,
      time_slots1: Seq.t(Time_slot.t),
      time_slots2: Seq.t(Time_slot.t),
    )
    : Seq.t(Time_slot.t) => {
  let rec aux = (time_slots1, time_slots2): Seq.t(Time_slot.t) =>
    switch (time_slots1(), time_slots2()) {
    | (Seq.Nil, _) => Seq.empty
    | (_, Seq.Nil) => Seq.empty
    | (
        [@implicit_arity] Seq.Cons((start1, end_exc1), rest1),
        [@implicit_arity] Seq.Cons((start2, end_exc2), rest2),
      ) =>
      if (end_exc1 < start2) {
        /* 1 is before 2 entirely, drop 1, keep 2 */
        aux(rest1, time_slots2);
      } else if (end_exc2 < start1) {
        /* 2 is before 1 entirely, keep 1, drop 2 */
        aux(time_slots1, rest2);
      } else {
        /* there is an overlap or touching */
        let overlap_start = max(start1, start2);
        let overlap_end_exc = min(end_exc1, end_exc2);
        let s1 =
          if (end_exc1 <= overlap_end_exc) {
            rest1;
          } else {
            time_slots1;
          };
        let s2 =
          if (end_exc2 <= overlap_end_exc) {
            rest2;
          } else {
            time_slots2;
          };
        if (overlap_start < overlap_end_exc) {
          /* there is an overlap */
          (
            () =>
              [@implicit_arity]
              Seq.Cons((overlap_start, overlap_end_exc), aux(s1, s2))
          );
        } else {
          aux(s1, s2);
        };
      }
    };

  let time_slots1 =
    if (skip_check) {
      time_slots1;
    } else {
      time_slots1
      |> Check.check_if_valid
      |> Check.check_if_disjoint
      |> Check.check_if_sorted;
    };

  let time_slots2 =
    if (skip_check) {
      time_slots2;
    } else {
      time_slots2
      |> Check.check_if_valid
      |> Check.check_if_disjoint
      |> Check.check_if_sorted;
    };

  aux(time_slots1, time_slots2);
};

module Merge = {
  let merge =
      (
        ~skip_check=false,
        time_slots1: Seq.t(Time_slot.t),
        time_slots2: Seq.t(Time_slot.t),
      )
      : Seq.t(Time_slot.t) => {
    let rec aux = (time_slots1, time_slots2) =>
      switch (time_slots1(), time_slots2()) {
      | (Seq.Nil, s)
      | (s, Seq.Nil) => (() => s)
      | (
          [@implicit_arity] Seq.Cons(x1, rest1),
          [@implicit_arity] Seq.Cons(x2, rest2),
        ) =>
        let ts1 = () => [@implicit_arity] Seq.Cons(x1, rest1);
        let ts2 = () => [@implicit_arity] Seq.Cons(x2, rest2);
        if (Time_slot.le(x1, x2)) {
          (() => [@implicit_arity] Seq.Cons(x1, aux(rest1, ts2)));
        } else {
          (() => [@implicit_arity] Seq.Cons(x2, aux(rest2, ts1)));
        };
      };

    let time_slots1 =
      if (skip_check) {
        time_slots1;
      } else {
        time_slots1 |> Check.check_if_valid |> Check.check_if_sorted;
      };

    let time_slots2 =
      if (skip_check) {
        time_slots2;
      } else {
        time_slots2 |> Check.check_if_valid |> Check.check_if_sorted;
      };

    aux(time_slots1, time_slots2);
  };

  let merge_multi_seq =
      (~skip_check=false, time_slot_batches: Seq.t(Seq.t(Time_slot.t)))
      : Seq.t(Time_slot.t) =>
    Seq.fold_left(
      (acc, time_slots) => merge(~skip_check, acc, time_slots),
      Seq.empty,
      time_slot_batches,
    );

  let merge_multi_list =
      (~skip_check=false, time_slot_batches: list(Seq.t(Time_slot.t)))
      : Seq.t(Time_slot.t) =>
    List.to_seq(time_slot_batches) |> merge_multi_seq(~skip_check);
};

module Round_robin = {
  let collect_round_robin_non_decreasing =
      (~skip_check=false, batches: list(Seq.t(Time_slot.t)))
      : Seq.t(list(option(Time_slot.t))) =>
    batches
    |> List.map(s =>
         if (skip_check) {
           s;
         } else {
           s |> Check.check_if_valid |> Check.check_if_sorted;
         }
       )
    |> Seq_utils.collect_round_robin(Time_slot.le);

  let merge_multi_list_round_robin_non_decreasing =
      (~skip_check=false, batches: list(Seq.t(Time_slot.t)))
      : Seq.t(Time_slot.t) =>
    collect_round_robin_non_decreasing(~skip_check, batches)
    |> Seq.flat_map(l => List.to_seq(l) |> Seq.filter_map(x => x));

  let merge_multi_seq_round_robin_non_decreasing =
      (~skip_check=false, batches: Seq.t(Seq.t(Time_slot.t)))
      : Seq.t(Time_slot.t) =>
    batches
    |> List.of_seq
    |> merge_multi_list_round_robin_non_decreasing(~skip_check);
};

module Union = {
  let union = (~skip_check=false, time_slots1, time_slots2) => {
    let time_slots1 =
      if (skip_check) {
        time_slots1;
      } else {
        time_slots1
        |> Check.check_if_valid
        |> Check.check_if_disjoint
        |> Check.check_if_sorted;
      };

    let time_slots2 =
      if (skip_check) {
        time_slots2;
      } else {
        time_slots2
        |> Check.check_if_valid
        |> Check.check_if_disjoint
        |> Check.check_if_sorted;
      };

    Merge.merge(time_slots1, time_slots2)
    |> Normalize.normalize(
         ~skip_filter_invalid=true,
         ~skip_filter_empty=true,
         ~skip_sort=true,
       );
  };

  let union_multi_seq =
      (~skip_check=false, time_slot_batches: Seq.t(Seq.t(Time_slot.t)))
      : Seq.t(Time_slot.t) =>
    Seq.fold_left(
      (acc, time_slots) => union(~skip_check, acc, time_slots),
      Seq.empty,
      time_slot_batches,
    );

  let union_multi_list =
      (~skip_check=false, time_slot_batches: list(Seq.t(Time_slot.t)))
      : Seq.t(Time_slot.t) =>
    List.to_seq(time_slot_batches) |> union_multi_seq(~skip_check);
};

let chunk =
    (
      ~skip_check=false,
      ~drop_partial=false,
      ~chunk_size,
      time_slots: Seq.t(Time_slot.t),
    )
    : Seq.t(Time_slot.t) => {
  let rec aux = time_slots =>
    switch (time_slots()) {
    | Seq.Nil => Seq.empty
    | [@implicit_arity] Seq.Cons((start, end_exc), rest) =>
      let chunk_end_exc = min(end_exc, start +^ chunk_size);
      let size = chunk_end_exc -^ start;
      if (size == 0L || size < chunk_size && drop_partial) {
        aux(rest);
      } else {
        let rest = () =>
          [@implicit_arity] Seq.Cons((chunk_end_exc, end_exc), rest);
        (
          () => [@implicit_arity] Seq.Cons((start, chunk_end_exc), aux(rest))
        );
      };
    };

  time_slots
  |> (
    s =>
      if (skip_check) {
        s;
      } else {
        s |> Check.check_if_valid;
      }
  )
  |> aux;
};

module Sum = {
  let sum_length = (~skip_check=false, time_slots: Seq.t(Time_slot.t)): int64 =>
    time_slots
    |> (
      s =>
        if (skip_check) {
          s;
        } else {
          Check.check_if_valid(s);
        }
    )
    |> Seq.fold_left(
         (acc, (start, end_exc)) => acc +^ (end_exc -^ start),
         0L,
       );

  let sum_length_list =
      (~skip_check=false, time_slots: list(Time_slot.t)): int64 =>
    time_slots |> List.to_seq |> sum_length(~skip_check);
};

module Bound = {
  let min_start_and_max_end_exc =
      (~skip_check=false, time_slots: Seq.t(Time_slot.t))
      : option((int64, int64)) =>
    time_slots
    |> (
      s =>
        if (skip_check) {
          s;
        } else {
          Check.check_if_valid(s);
        }
    )
    |> Seq.fold_left(
         (acc, (start, end_exc)) =>
           switch (acc) {
           | None => Some((start, end_exc))
           | Some((min_start, max_end_exc)) =>
             Some((min(min_start, start), max(max_end_exc, end_exc)))
           },
         None,
       );

  let min_start_and_max_end_exc_list =
      (~skip_check=false, time_slots: list(Time_slot.t))
      : option((int64, int64)) =>
    time_slots |> List.to_seq |> min_start_and_max_end_exc(~skip_check);
};

let shift_list = (~offset, time_slots: list(Time_slot.t)): list(Time_slot.t) =>
  List.map(
    ((start, end_exc)) => (start +^ offset, end_exc +^ offset),
    time_slots,
  );

let equal =
    (time_slots1: list(Time_slot.t), time_slots2: list(Time_slot.t)): bool => {
  let time_slots1 =
    time_slots1 |> List.to_seq |> Normalize.normalize |> List.of_seq;

  let time_slots2 =
    time_slots2 |> List.to_seq |> Normalize.normalize |> List.of_seq;

  time_slots1 == time_slots2;
};

let a_is_subset_of_b =
    (~a: Seq.t(Time_slot.t), ~b: Seq.t(Time_slot.t)): bool => {
  let inter = inter(a, b) |> List.of_seq;
  let a = List.of_seq(a);
  a == inter;
};

let count_overlap =
    (~skip_check=false, time_slots: Seq.t(Time_slot.t))
    : Seq.t((Time_slot.t, int)) => {
  let flatten_buffer = buffer =>
    buffer
    |> List.sort(((x, _count), (y, _count)) => Time_slot.compare(x, y))
    |> List.to_seq
    |> Seq.flat_map(((x, count)) => OSeq.(0 --^ count) |> Seq.map(_ => x));

  let flush_buffer_to_input = (buffer, time_slots) =>
    Merge.merge(flatten_buffer(buffer), time_slots);

  let rec aux =
          (
            cur: option(((int64, int64), int)),
            buffer: list(((int64, int64), int)),
            time_slots: Seq.t(Time_slot.t),
          )
          : Seq.t((Time_slot.t, int)) =>
    switch (time_slots()) {
    | Seq.Nil =>
      switch (buffer) {
      | [] =>
        switch (cur) {
        | None => Seq.empty
        | Some(cur) => Seq.return(cur)
        }
      | buffer => aux(cur, [], flatten_buffer(buffer))
      }
    | [@implicit_arity] Seq.Cons(x, rest) =>
      let s = () => [@implicit_arity] Seq.Cons(x, rest);
      switch (cur) {
      | None =>
        switch (buffer) {
        | [] => aux(Some((x, 1)), [], rest)
        | buffer => aux(None, [], flush_buffer_to_input(buffer, s))
        }
      | Some(((cur_start, cur_end_exc), cur_count)) =>
        switch (
          Time_slot.overlap_of_a_over_b(~a=x, ~b=(cur_start, cur_end_exc))
        ) {
        | (None, None, None) => aux(cur, buffer, rest)
        | (Some(_), _, _) =>
          failwith("Unexpected case, time slots are not sorted")
        /* raise (Invalid_argument "Time slots are not sorted") */
        | (None, Some((start, end_exc)), None)
        | (None, Some((start, _)), Some((_, end_exc))) =>
          if (start == cur_start) {
            if (end_exc < cur_end_exc) {
              failwith("Unexpected case, time slots are not sorted");
            } else if
              /* raise (Invalid_argument "Time slots are not sorted") */
              (end_exc == cur_end_exc) {
              aux(
                Some(((cur_start, cur_end_exc), succ(cur_count))),
                buffer,
                rest,
              );
            } else {
              aux(
                Some(((cur_start, cur_end_exc), succ(cur_count))),
                [((cur_end_exc, end_exc), 1), ...buffer],
                rest,
              );
            };
          } else {
            (
              () =>
                [@implicit_arity]
                Seq.Cons(
                  ((cur_start, start), cur_count),
                  {
                    let buffer =
                      if (end_exc < cur_end_exc) {
                        [
                          ((start, end_exc), succ(cur_count)),
                          ((end_exc, cur_end_exc), cur_count),
                          ...buffer,
                        ];
                      } else if (end_exc == cur_end_exc) {
                        [
                          ((start, cur_end_exc), succ(cur_count)),
                          ...buffer,
                        ];
                      } else {
                        [
                          ((start, cur_end_exc), succ(cur_count)),
                          ((cur_end_exc, end_exc), 1),
                          ...buffer,
                        ];
                      };

                    aux(None, buffer, rest);
                  },
                )
            );
          }
        | (None, None, Some(_)) => (
            () =>
              [@implicit_arity]
              Seq.Cons(
                ((cur_start, cur_end_exc), cur_count),
                aux(None, buffer, s),
              )
          )
        }
      };
    };

  time_slots
  |> (
    s =>
      if (skip_check) {
        s;
      } else {
        s |> Check.check_if_valid |> Check.check_if_sorted;
      }
  )
  |> aux(None, []);
};

module Serialize = {
  let pack_time_slots = time_slots =>
    List.map(Time_slot.Serialize.pack_time_slot, time_slots);
};

module Deserialize = {
  let unpack_time_slots = time_slots =>
    List.map(Time_slot.Deserialize.unpack_time_slot, time_slots);
};
