let single_task_seg_shift:
  (
    ~incre: int64,
    ~cur_pos: int64,
    ~task_seg: Task.task_seg,
    Seq.t(Time_slot.t)
  ) =>
  Seq.t(Task.task_seg_place);

let single_task_seg_shift_rev:
  (
    ~incre: int64,
    ~cur_end_pos_exc: int64,
    ~task_seg: Task.task_seg,
    Seq.t(Time_slot.t)
  ) =>
  Seq.t(Task.task_seg_place);

let multi_task_segs_shift:
  (~incre: int64, ~task_segs: list(Task.task_seg), Seq.t(Time_slot.t)) =>
  Seq.t(list(Task.task_seg_place));

let single_task_seg_single_split:
  (
    ~min_seg_size: int64,
    ~max_seg_size: int64,
    ~cur_split_pos: int64,
    ~task_seg: Task.task_seg
  ) =>
  Seq.t((Task.task_seg, Task.task_seg));

let single_task_seg_multi_splits_exact:
  (
    ~min_seg_size: int64,
    ~max_seg_size: option(int64),
    ~split_count: int64,
    ~task_seg: Task.task_seg
  ) =>
  Seq.t(list(Task.task_seg));

let single_task_seg_multi_splits_max:
  (
    ~min_seg_size: int64,
    ~max_seg_size: option(int64),
    ~split_count: int64,
    ~task_seg: Task.task_seg
  ) =>
  Seq.t(list(Task.task_seg));

let single_task_seg_multi_splits_exact_shift:
  (
    ~min_seg_size: int64,
    ~max_seg_size: option(int64),
    ~split_count: int64,
    ~incre: int64,
    ~task_seg: Task.task_seg,
    Seq.t(Time_slot.t)
  ) =>
  Seq.t(list(Task.task_seg_place));

let single_task_seg_multi_splits_max_shift:
  (
    ~min_seg_size: int64,
    ~max_seg_size: option(int64),
    ~split_count: int64,
    ~incre: int64,
    ~task_seg: Task.task_seg,
    Seq.t(Time_slot.t)
  ) =>
  Seq.t(list(Task.task_seg_place));

let multi_task_segs_interleave:
  (
    ~interval_size: int64,
    ~task_segs: list(Task.task_seg),
    Seq.t(Time_slot.t)
  ) =>
  Seq.t(Task.task_seg_place);

let single_task_seg_multi_even_splits:
  (
    ~incre: int64,
    ~task_seg: Task.task_seg,
    ~buckets: list(Time_slot.t),
    ~usable_time_slots: Seq.t(Time_slot.t)
  ) =>
  Seq.t(list(Task.task_seg_place));
