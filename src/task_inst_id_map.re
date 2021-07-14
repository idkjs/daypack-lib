include Map.Make({
  type t = Task.task_inst_id;

  let compare = compare;
});
