include Map.Make({
  type t = Task.task_seg_id;

  let compare = compare;
});
