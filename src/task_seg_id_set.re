include Set.Make({
  type t = Task.task_seg_id;

  let compare = compare;
});

module Serialize = {
  let pack = (t: t): list(Task_t.task_seg_id) =>
    t |> to_seq |> Seq.map(Task.Serialize.pack_task_seg_id) |> List.of_seq;
};

module Deserialize = {
  let unpack = (l: list(Task_t.task_seg_id)): t =>
    l |> List.to_seq |> Seq.map(Task.Deserialize.unpack_task_seg_id) |> of_seq;
};
