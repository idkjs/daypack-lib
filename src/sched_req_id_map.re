include Map.Make({
  type t = Sched_req.sched_req_id;

  let compare = compare;
});
