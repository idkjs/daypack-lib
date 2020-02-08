type t = { mutable history : Sched.sched list }

(* let fold_head ~none:(f_none : unit -> Sched.sched)
 *     ~some:(f : Sched.sched -> Sched.sched) (t : t) : unit =
 *   match t.history with
 *   | [] -> t.history <- [ f_none () ]
 *   | hd :: tl ->
 *     let hd = f hd in
 *     t.history <- hd :: tl *)

let map_head (f : Sched.sched -> Sched.sched) (t : t) : unit =
  match t.history with
  | [] -> t.history <- [ f Sched.empty ]
  | hd :: tl ->
    let hd = f hd in
    t.history <- hd :: tl

module In_place_head = struct
  let add_task ~parent_user_id (data : Task.task_data)
      (task_inst_data_list : Task.task_inst_data list) (t : t) : unit =
    map_head
      (fun sched ->
          let _, _, sched =
            sched |>
            Sched.Task_store.add_task ~parent_user_id data task_inst_data_list
          in
          sched)
      t

  let queue_sched_req (data : Sched_req.sched_req_data) (t : t) : unit =
    map_head
      (fun sched ->
sched |>
            Sched.Sched_req_store.queue_sched_req_data data
        )
      t
end

module Maybe_append_head = struct
end

module Append_head = struct
end

module Serialize = struct
  let to_base_and_diffs (l : Sched.sched list) :
    (Sched.sched * Sched.sched_diff list) option =
    let rec aux
        (base_and_last_and_diffs :
           (Sched.sched * Sched.sched * Sched.sched_diff list) option)
        (l : Sched.sched list) =
      match l with
      | [] -> (
          match base_and_last_and_diffs with
          | None -> None
          | Some (base, _, diffs) -> Some (base, List.rev diffs) )
      | sched :: rest -> (
          match base_and_last_and_diffs with
          | None -> aux (Some (sched, sched, [])) rest
          | Some (base, last, diffs) ->
            let diff = Sched.Diff.diff_sched ~old:last sched in
            aux (Some (base, sched, diff :: diffs)) rest )
    in
    aux None (List.rev l)
end

module Deserialize = struct
  let of_base_and_diffs (base : Sched.sched) (diffs : Sched.sched_diff list) :
    Sched.sched list =
    let rec aux (acc : Sched.sched list) (cur : Sched.sched)
        (diffs : Sched.sched_diff list) : Sched.sched list =
      match diffs with
      | [] -> acc
      | diff :: diffs ->
        let next = Sched.Diff.add_diff_sched diff cur in
        aux (next :: acc) next diffs
    in
    aux [ base ] base diffs
end
