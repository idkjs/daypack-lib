include Set.Make({
  type t = Task.task_seg_place;

  let compare =
      ((task_seg_id1, start1, end_exc1), (task_seg_id2, start2, end_exc2)) =>
    switch (Time_slot.compare((start1, end_exc1), (start2, end_exc2))) {
    | 0 => compare(task_seg_id1, task_seg_id2)
    | n => n
    };
});

module Serialize = {
  let pack = (t: t): list(Task.task_seg_place) => t |> to_seq |> List.of_seq;
};

module Deserialize = {
  let unpack = (l: list(Task.task_seg_place)): t =>
    l |> List.to_seq |> of_seq;
};
