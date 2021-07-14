open Test_utils;

module Alco = {
  let count_overlap1 = () =>
    Alcotest.(check(list(pair(pair(int64, int64), int))))(
      "same list",
      [
        ((0L, 1L), 2),
        ((1L, 3L), 4),
        ((3L, 4L), 3),
        ((4L, 5L), 2),
        ((5L, 6L), 1),
      ],
      Daypack_lib.Time_slots.count_overlap(
        List.to_seq([(0L, 5L), (0L, 6L), (1L, 3L), (1L, 4L)]),
      )
      |> List.of_seq,
    );

  let count_overlap2 = () =>
    Alcotest.(check(list(pair(pair(int64, int64), int))))(
      "same list",
      [
        ((0L, 1L), 2),
        ((1L, 2L), 5),
        ((2L, 3L), 7),
        ((3L, 4L), 6),
        ((4L, 5L), 5),
        ((5L, 6L), 2),
        ((6L, 7L), 1),
      ],
      Daypack_lib.Time_slots.count_overlap(
        List.to_seq([
          (0L, 5L),
          (0L, 6L),
          (1L, 4L),
          (1L, 5L),
          (1L, 7L),
          (2L, 3L),
          (2L, 5L),
        ]),
      )
      |> List.of_seq,
    );

  let count_overlap3 = () =>
    Alcotest.(check(list(pair(pair(int64, int64), int))))(
      "same list",
      [
        ((0L, 1L), 2),
        ((1L, 2L), 4),
        ((2L, 3L), 5),
        ((3L, 4L), 4),
        ((4L, 5L), 3),
        ((5L, 6L), 2),
        ((6L, 7L), 2),
        ((7L, 15L), 1),
        ((16L, 17L), 2),
        ((17L, 19L), 1),
        ((19L, 20L), 2),
      ],
      Daypack_lib.Time_slots.count_overlap(
        List.to_seq([
          (0L, 5L),
          (0L, 7L),
          (1L, 2L),
          (1L, 6L),
          (2L, 3L),
          (2L, 4L),
          (6L, 15L),
          (16L, 17L),
          (16L, 20L),
          (19L, 20L),
        ]),
      )
      |> List.of_seq,
    );

  let count_overlap4 = () =>
    Alcotest.(check(list(pair(pair(int64, int64), int))))(
      "same list",
      [((1L, 2L), 3), ((2L, 3L), 5), ((3L, 4L), 7), ((4L, 5L), 9)],
      Daypack_lib.Time_slots.count_overlap(
        List.to_seq([
          (1L, 5L),
          (1L, 5L),
          (1L, 5L),
          (2L, 5L),
          (2L, 5L),
          (3L, 5L),
          (3L, 5L),
          (4L, 5L),
          (4L, 5L),
        ]),
      )
      |> List.of_seq,
    );

  let count_overlap5 = () =>
    Alcotest.(check(list(pair(pair(int64, int64), int))))(
      "same list",
      [
        ((1L, 2L), 1),
        ((2L, 3L), 2),
        ((3L, 4L), 3),
        ((4L, 5L), 5),
        ((5L, 6L), 2),
      ],
      Daypack_lib.Time_slots.count_overlap(
        List.to_seq([(1L, 5L), (2L, 5L), (3L, 5L), (4L, 6L), (4L, 6L)]),
      )
      |> List.of_seq,
    );

  let suite = [
    Alcotest.test_case("count_overlap1", `Quick, count_overlap1),
    Alcotest.test_case("count_overlap2", `Quick, count_overlap2),
    Alcotest.test_case("count_overlap3", `Quick, count_overlap3),
    Alcotest.test_case("count_overlap4", `Quick, count_overlap4),
    Alcotest.test_case("count_overlap5", `Quick, count_overlap5),
  ];
};

module Qc = {
  let slice_start =
    QCheck.Test.make(
      ~count=10_000,
      ~name="slice_start",
      QCheck.(pair(pos_int64, sorted_time_slots_maybe_gaps)),
      ((start, l)) =>
      l
      |> List.to_seq
      |> Daypack_lib.Time_slots.Slice.slice(~start)
      |> List.of_seq
      |> List.for_all(((x, _)) => start <= x)
    );

  let slice_end_exc =
    QCheck.Test.make(
      ~count=10_000,
      ~name="slice_end_exc",
      QCheck.(pair(pos_int64, sorted_time_slots_maybe_gaps)),
      ((end_exc, l)) =>
      l
      |> List.to_seq
      |> Daypack_lib.Time_slots.Slice.slice(~end_exc)
      |> List.of_seq
      |> List.for_all(((_, y)) => y <= end_exc)
    );

  let normalize_pairs_are_fine =
    QCheck.Test.make(
      ~count=10_000, ~name="normalize_pairs_are_fine", time_slots, l =>
      l
      |> List.to_seq
      |> Daypack_lib.Time_slots.Normalize.normalize
      |> List.of_seq
      |> List.for_all(((x, y)) => x <= y)
    );

  let normalize_time_slots_are_sorted =
    QCheck.Test.make(
      ~count=10_000, ~name="normalize_time_slots_are_sorted", time_slots, l =>
      l
      |> List.to_seq
      |> Daypack_lib.Time_slots.Normalize.normalize
      |> List.of_seq
      |> List.fold_left(
           ((res, last), (x, y)) =>
             if (res) {
               switch (last) {
               | None => (true, Some((x, y)))
               | Some((last_start, last_end_exc)) => (
                   last_start <= x && last_end_exc <= x,
                   Some((x, y)),
                 )
               };
             } else {
               (false, None);
             },
           (true, None),
         )
      |> (((x, _)) => x)
    );

  let normalize_time_slots_are_unique =
    QCheck.Test.make(
      ~count=10_000,
      ~name="normalize_time_slots_are_unique",
      time_slots,
      l => {
        let l =
          l
          |> List.to_seq
          |> Daypack_lib.Time_slots.Normalize.normalize
          |> List.of_seq;

        List.length(List.sort_uniq(compare, l)) == List.length(l);
      },
    );

  let normalize_time_slots_are_disjoint_with_gaps =
    QCheck.Test.make(
      ~count=10_000,
      ~name="normalize_time_slots_are_disjoint_with_gaps",
      time_slots,
      l =>
      l
      |> List.to_seq
      |> Daypack_lib.Time_slots.Normalize.normalize
      |> Seq.fold_left(
           ((res, last), (x, y)) =>
             if (res) {
               switch (last) {
               | None => (true, Some((x, y)))
               | Some((_, last_end_exc)) => (last_end_exc < x, Some((x, y)))
               };
             } else {
               (false, None);
             },
           (true, None),
         )
      |> (((x, _)) => x)
    );

  let normalize_idempotent_wrt_normalized_time_slots =
    QCheck.Test.make(
      ~count=10_000,
      ~name="normalize_idempotent_wrt_normalized_time_slots",
      sorted_time_slots_with_gaps,
      l =>
      l
      |> List.to_seq
      |> Daypack_lib.Time_slots.Normalize.normalize
      |> List.of_seq == l
    );

  let join_time_slots_are_disjoint_with_gaps =
    QCheck.Test.make(
      ~count=10_000,
      ~name="join_time_slots_are_disjoint_with_gaps",
      sorted_time_slots_maybe_gaps,
      l =>
      l
      |> List.to_seq
      |> Daypack_lib.Time_slots.join
      |> Seq.fold_left(
           ((res, last), (x, y)) =>
             if (res) {
               switch (last) {
               | None => (true, Some((x, y)))
               | Some((_, last_end_exc)) => (last_end_exc < x, Some((x, y)))
               };
             } else {
               (false, None);
             },
           (true, None),
         )
      |> (((x, _)) => x)
    );

  let join_idempotent_wrt_joined_time_slots =
    QCheck.Test.make(
      ~count=10_000,
      ~name="join_idempotent_wrt_joined_time_slots",
      sorted_time_slots_with_gaps,
      l =>
      l |> List.to_seq |> Daypack_lib.Time_slots.join |> List.of_seq == l
    );

  let invert_disjoint_from_original =
    QCheck.Test.make(
      ~count=10_000,
      ~name="invert_disjoint_from_original",
      QCheck.(triple(pos_int64, pos_int64, sorted_time_slots_maybe_gaps)),
      ((start, end_exc, l)) => {
        QCheck.assume(start <= end_exc);
        let sliced =
          l
          |> List.to_seq
          |> Daypack_lib.Time_slots.Slice.slice(~start, ~end_exc)
          |> List.of_seq;

        let inverted =
          l
          |> List.to_seq
          |> Daypack_lib.Time_slots.invert(~start, ~end_exc)
          |> List.of_seq;

        let sliced_count = List.length(sliced);
        let inverted_count = List.length(inverted);
        List.length(List.sort_uniq(compare, sliced @ inverted))
        == sliced_count
        + inverted_count;
      },
    );

  let invert_fit_gaps =
    QCheck.Test.make(
      ~count=10_000,
      ~name="invert_fit_gaps",
      QCheck.(triple(pos_int64, pos_int64, sorted_time_slots_maybe_gaps)),
      ((start, end_exc, l)) => {
        QCheck.assume(start < end_exc);
        let res =
          l
          |> List.to_seq
          |> Daypack_lib.Time_slots.invert(~start, ~end_exc)
          |> List.of_seq
          |> (
            inverted =>
              (
                Daypack_lib.Time_slots.Slice.slice(
                  ~start,
                  ~end_exc,
                  List.to_seq(l),
                )
                |> List.of_seq
              )
              @ inverted
          )
          |> List.to_seq
          |> Daypack_lib.Time_slots.Normalize.normalize
          |> List.of_seq;

        l != []
        && List.for_all(((x, y)) => y < start || end_exc < x, l)
        || [(start, end_exc)] == res;
      },
    );

  let relatvie_complement_result_disjoint_from_not_mem_of =
    QCheck.Test.make(
      ~count=10_000,
      ~name="relative_complement_disjoint_from_not_mem_of",
      QCheck.(
        pair(sorted_time_slots_maybe_gaps, sorted_time_slots_maybe_gaps)
      ),
      ((mem_of, not_mem_of)) => {
        let res =
          Daypack_lib.Time_slots.relative_complement(
            ~not_mem_of=List.to_seq(not_mem_of),
            List.to_seq(mem_of),
          )
          |> List.of_seq;

        let not_mem_of_count = List.length(not_mem_of);
        let res_count = List.length(res);
        List.length(List.sort_uniq(compare, not_mem_of @ res))
        == not_mem_of_count
        + res_count;
      },
    );

  let relatvie_complement_result_subset_of_mem_of =
    QCheck.Test.make(
      ~count=10_000,
      ~name="relatvie_complement_result_subset_of_mem_of",
      QCheck.(
        pair(sorted_time_slots_maybe_gaps, sorted_time_slots_maybe_gaps)
      ),
      ((mem_of, not_mem_of)) => {
        let res_s =
          Daypack_lib.Time_slots.relative_complement(
            ~not_mem_of=List.to_seq(not_mem_of),
            List.to_seq(mem_of),
          );

        let res = res_s |> List.of_seq;
        Daypack_lib.Time_slots.inter(List.to_seq(mem_of), res_s)
        |> List.of_seq == res;
      },
    );

  let relatvie_complement_self =
    QCheck.Test.make(
      ~count=10_000,
      ~name="relatvie_complement_self",
      sorted_time_slots_maybe_gaps,
      l => {
        let s = List.to_seq(l);
        Daypack_lib.Time_slots.relative_complement(~not_mem_of=s, s)
        |> List.of_seq == [];
      },
    );

  let inter_with_self =
    QCheck.Test.make(
      ~count=10_000,
      ~name="inter_with_self",
      sorted_time_slots_maybe_gaps,
      l => {
        let s = l |> List.to_seq;
        let res = Daypack_lib.Time_slots.inter(s, s) |> List.of_seq;
        l == res;
      },
    );

  let inter_commutative =
    QCheck.Test.make(
      ~count=10_000,
      ~name="inter_commutative",
      QCheck.(
        pair(sorted_time_slots_maybe_gaps, sorted_time_slots_maybe_gaps)
      ),
      ((l1, l2)) => {
        let s1 = l1 |> List.to_seq;
        let s2 = l2 |> List.to_seq;
        let inter1 = Daypack_lib.Time_slots.inter(s1, s2) |> List.of_seq;
        let inter2 = Daypack_lib.Time_slots.inter(s2, s1) |> List.of_seq;
        inter1 == inter2;
      },
    );

  let inter_associative =
    QCheck.Test.make(
      ~count=10_000,
      ~name="inter_associative",
      QCheck.(
        triple(
          sorted_time_slots_maybe_gaps,
          sorted_time_slots_maybe_gaps,
          sorted_time_slots_maybe_gaps,
        )
      ),
      ((l1, l2, l3)) => {
        let s1 = l1 |> List.to_seq;
        let s2 = l2 |> List.to_seq;
        let s3 = l3 |> List.to_seq;
        let inter1 =
          Daypack_lib.Time_slots.(inter(inter(s1, s2), s3)) |> List.of_seq;

        let inter2 =
          Daypack_lib.Time_slots.(inter(s1, inter(s2, s3))) |> List.of_seq;

        inter1 == inter2;
      },
    );

  let union_with_self =
    QCheck.Test.make(
      ~count=10_000,
      ~name="union_with_self",
      sorted_time_slots_with_gaps,
      l => {
        let s = l |> List.to_seq;
        let res = Daypack_lib.Time_slots.Union.union(s, s) |> List.of_seq;
        l == res;
      },
    );

  let union_commutative =
    QCheck.Test.make(
      ~count=10_000,
      ~name="union_commutative",
      QCheck.(
        pair(sorted_time_slots_maybe_gaps, sorted_time_slots_maybe_gaps)
      ),
      ((l1, l2)) => {
        let s1 = l1 |> List.to_seq;
        let s2 = l2 |> List.to_seq;
        let inter1 =
          Daypack_lib.Time_slots.Union.union(s1, s2) |> List.of_seq;
        let inter2 =
          Daypack_lib.Time_slots.Union.union(s2, s1) |> List.of_seq;
        inter1 == inter2;
      },
    );

  let union_associative =
    QCheck.Test.make(
      ~count=10_000,
      ~name="union_associative",
      QCheck.(
        triple(
          sorted_time_slots_with_gaps,
          sorted_time_slots_with_gaps,
          sorted_time_slots_with_gaps,
        )
      ),
      ((l1, l2, l3)) => {
        let s1 = l1 |> List.to_seq;
        let s2 = l2 |> List.to_seq;
        let s3 = l3 |> List.to_seq;
        let res1 =
          Daypack_lib.Time_slots.(Union.union(Union.union(s1, s2), s3))
          |> List.of_seq;

        let res2 =
          Daypack_lib.Time_slots.(Union.union(s1, Union.union(s2, s3)))
          |> List.of_seq;

        res1 == res2;
      },
    );

  let inter_union_distributive1 =
    QCheck.Test.make(
      ~count=10_000,
      ~name="inter_union_distributive1",
      QCheck.(
        triple(
          sorted_time_slots_maybe_gaps,
          sorted_time_slots_maybe_gaps,
          sorted_time_slots_maybe_gaps,
        )
      ),
      ((l1, l2, l3)) => {
        let s1 = l1 |> List.to_seq;
        let s2 = l2 |> List.to_seq;
        let s3 = l3 |> List.to_seq;
        let res1 =
          Daypack_lib.Time_slots.(Union.union(s1, inter(s2, s3)))
          |> List.of_seq;

        let res2 =
          Daypack_lib.Time_slots.(
            inter(Union.union(s1, s2), Union.union(s1, s3))
          )
          |> List.of_seq;

        res1 == res2;
      },
    );

  let inter_union_distributive2 =
    QCheck.Test.make(
      ~count=10_000,
      ~name="inter_union_distributive2",
      QCheck.(
        triple(
          sorted_time_slots_with_gaps,
          sorted_time_slots_maybe_gaps,
          sorted_time_slots_maybe_gaps,
        )
      ),
      ((l1, l2, l3)) => {
        let s1 = l1 |> List.to_seq;
        let s2 = l2 |> List.to_seq;
        let s3 = l3 |> List.to_seq;
        let res1 =
          Daypack_lib.Time_slots.(inter(s1, Union.union(s2, s3)))
          |> List.of_seq;

        let res2 =
          Daypack_lib.Time_slots.(Union.union(inter(s1, s2), inter(s1, s3)))
          |> List.of_seq;

        res1 == res2;
      },
    );

  let merge =
    QCheck.Test.make(
      ~count=10_000,
      ~name="merge",
      QCheck.(
        pair(sorted_time_slots_with_overlaps, sorted_time_slots_with_overlaps)
      ),
      ((l1, l2)) => {
        let s1 = l1 |> List.to_seq;
        let s2 = l2 |> List.to_seq;
        let res1 = Daypack_lib.Time_slots.Merge.merge(s1, s2) |> List.of_seq;
        let res2 = Daypack_lib.Time_slots.Sort.sort_time_slots_list(l1 @ l2);
        res1 == res2;
      },
    );

  let suite = [
    slice_start,
    slice_end_exc,
    normalize_pairs_are_fine,
    normalize_time_slots_are_sorted,
    normalize_time_slots_are_unique,
    normalize_time_slots_are_disjoint_with_gaps,
    normalize_idempotent_wrt_normalized_time_slots,
    join_time_slots_are_disjoint_with_gaps,
    join_idempotent_wrt_joined_time_slots,
    invert_disjoint_from_original,
    invert_fit_gaps,
    relatvie_complement_result_disjoint_from_not_mem_of,
    relatvie_complement_result_subset_of_mem_of,
    relatvie_complement_self,
    inter_with_self,
    inter_commutative,
    inter_associative,
    union_with_self,
    union_commutative,
    union_associative,
    inter_union_distributive1,
    inter_union_distributive2,
    merge,
  ];
};
