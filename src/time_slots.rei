exception Time_slots_are_not_sorted;

exception Time_slots_are_not_disjoint;

module Check: {
  let check_if_valid: Seq.t(Time_slot.t) => Seq.t(Time_slot.t);

  let check_if_not_empty: Seq.t(Time_slot.t) => Seq.t(Time_slot.t);

  let check_if_sorted: Seq.t(Time_slot.t) => Seq.t(Time_slot.t);

  let check_if_sorted_rev: Seq.t(Time_slot.t) => Seq.t(Time_slot.t);

  let check_if_disjoint: Seq.t(Time_slot.t) => Seq.t(Time_slot.t);

  let check_if_normalized: Seq.t(Time_slot.t) => Seq.t(Time_slot.t);
};

module Filter: {
  let filter_invalid: Seq.t(Time_slot.t) => Seq.t(Time_slot.t);

  let filter_invalid_list: list(Time_slot.t) => list(Time_slot.t);

  let filter_empty: Seq.t(Time_slot.t) => Seq.t(Time_slot.t);

  let filter_empty_list: list(Time_slot.t) => list(Time_slot.t);
};

module Sort: {
  let sort_time_slots_list:
    (~skip_check: bool=?, list(Time_slot.t)) => list(Time_slot.t);

  let sort_time_slots:
    (~skip_check: bool=?, Seq.t(Time_slot.t)) => Seq.t(Time_slot.t);

  let sort_uniq_time_slots_list:
    (~skip_check: bool=?, list(Time_slot.t)) => list(Time_slot.t);

  let sort_uniq_time_slots:
    (~skip_check: bool=?, Seq.t(Time_slot.t)) => Seq.t(Time_slot.t);
};

let join: (~skip_check: bool=?, Seq.t(Time_slot.t)) => Seq.t(Time_slot.t);

module Normalize: {
  let normalize:
    (
      ~skip_filter_invalid: bool=?,
      ~skip_filter_empty: bool=?,
      ~skip_sort: bool=?,
      Seq.t(Time_slot.t)
    ) =>
    Seq.t(Time_slot.t);

  let normalize_list_in_seq_out:
    (
      ~skip_filter_invalid: bool=?,
      ~skip_filter_empty: bool=?,
      ~skip_sort: bool=?,
      list(Time_slot.t)
    ) =>
    Seq.t(Time_slot.t);
};

module Slice: {
  let slice:
    (
      ~skip_check: bool=?,
      ~start: int64=?,
      ~end_exc: int64=?,
      Seq.t(Time_slot.t)
    ) =>
    Seq.t(Time_slot.t);

  let slice_rev:
    (
      ~skip_check: bool=?,
      ~start: int64=?,
      ~end_exc: int64=?,
      Seq.t(Time_slot.t)
    ) =>
    Seq.t(Time_slot.t);
};

let invert:
  (~skip_check: bool=?, ~start: int64, ~end_exc: int64, Seq.t(Time_slot.t)) =>
  Seq.t(Time_slot.t);

let relative_complement:
  (
    ~skip_check: bool=?,
    ~not_mem_of: Seq.t(Time_slot.t),
    Seq.t(Time_slot.t)
  ) =>
  Seq.t(Time_slot.t);

module Merge: {
  let merge:
    (~skip_check: bool=?, Seq.t(Time_slot.t), Seq.t(Time_slot.t)) =>
    Seq.t(Time_slot.t);

  let merge_multi_seq:
    (~skip_check: bool=?, Seq.t(Seq.t(Time_slot.t))) => Seq.t(Time_slot.t);

  let merge_multi_list:
    (~skip_check: bool=?, list(Seq.t(Time_slot.t))) => Seq.t(Time_slot.t);
};

module Round_robin: {
  let collect_round_robin_non_decreasing:
    (~skip_check: bool=?, list(Seq.t(Time_slot.t))) =>
    Seq.t(list(option(Time_slot.t)));

  let merge_multi_seq_round_robin_non_decreasing:
    (~skip_check: bool=?, Seq.t(Seq.t(Time_slot.t))) => Seq.t(Time_slot.t);

  let merge_multi_list_round_robin_non_decreasing:
    (~skip_check: bool=?, list(Seq.t(Time_slot.t))) => Seq.t(Time_slot.t);
};

let inter:
  (~skip_check: bool=?, Seq.t(Time_slot.t), Seq.t(Time_slot.t)) =>
  Seq.t(Time_slot.t);

module Union: {
  let union:
    (~skip_check: bool=?, Seq.t(Time_slot.t), Seq.t(Time_slot.t)) =>
    Seq.t(Time_slot.t);

  let union_multi_seq:
    (~skip_check: bool=?, Seq.t(Seq.t(Time_slot.t))) => Seq.t(Time_slot.t);

  let union_multi_list:
    (~skip_check: bool=?, list(Seq.t(Time_slot.t))) => Seq.t(Time_slot.t);
};

let chunk:
  (
    ~skip_check: bool=?,
    ~drop_partial: bool=?,
    ~chunk_size: int64,
    Seq.t(Time_slot.t)
  ) =>
  Seq.t(Time_slot.t);

module Sum: {
  let sum_length: (~skip_check: bool=?, Seq.t(Time_slot.t)) => int64;

  let sum_length_list: (~skip_check: bool=?, list(Time_slot.t)) => int64;
};

module Bound: {
  let min_start_and_max_end_exc:
    (~skip_check: bool=?, Seq.t(Time_slot.t)) => option((int64, int64));

  let min_start_and_max_end_exc_list:
    (~skip_check: bool=?, list(Time_slot.t)) => option((int64, int64));
};

let shift_list: (~offset: int64, list(Time_slot.t)) => list(Time_slot.t);

let equal: (list(Time_slot.t), list(Time_slot.t)) => bool;

let a_is_subset_of_b:
  (~a: Seq.t(Time_slot.t), ~b: Seq.t(Time_slot.t)) => bool;

let count_overlap:
  (~skip_check: bool=?, Seq.t(Time_slot.t)) => Seq.t((Time_slot.t, int));

module Serialize: {
  let pack_time_slots:
    list((int64, int64)) => list(((int32, int32), (int32, int32)));
};

module Deserialize: {
  let unpack_time_slots:
    list(((int32, int32), (int32, int32))) => list((int64, int64));
};
