module Task_ = Task;

type t;

type action_record =
  | Updated_head(Sched.sched_id)
  | Added_new_head(Sched.sched_id)
  | Did_nothing;

let make_empty: unit => t;

let of_sched_list: list(Sched.sched) => t;

module Read: {let get_head: t => Sched.sched;};

module In_place_head: {
  module Task: {
    module Add: {
      let add_task:
        (
          ~parent_user_id: int64,
          Task_.task_data,
          list(Task_.task_inst_data),
          t
        ) =>
        (Task_.task, list(Task_.task_inst), action_record);
    };

    module Move: {
      let move_task_to_completed: (Task_.task_id, t) => action_record;

      let move_task_to_uncompleted: (Task_.task_id, t) => action_record;

      let move_task_to_discarded: (Task_.task_id, t) => action_record;
    };
  };

  module Task_inst: {
    module Add: {
      let add_task_inst:
        (~parent_task_id: Task_.task_id, Task_.task_inst_data, t) =>
        (Task_.task_inst, action_record);
    };

    module Move: {
      let move_task_inst_to_completed:
        (Task_.task_inst_id, t) => action_record;

      let move_task_inst_to_uncompleted:
        (Task_.task_inst_id, t) => action_record;

      let move_task_inst_to_discarded:
        (Task_.task_inst_id, t) => action_record;
    };
  };

  module Task_seg: {
    module Move: {
      let move_task_seg_to_completed: (Task_.task_seg_id, t) => action_record;

      let move_task_seg_to_uncompleted:
        (Task_.task_seg_id, t) => action_record;

      let move_task_seg_to_discarded: (Task_.task_seg_id, t) => action_record;
    };
  };

  module Sched_req: {
    module Add: {
      let add_sched_req:
        (Sched_req.sched_req_data, t) =>
        (result(Sched_req.sched_req, unit), action_record);
    };
  };

  module Recur: {
    let instantiate: (~start: int64, ~end_exc: int64, t) => action_record;
  };

  module Progress: {
    module Add: {
      let add_task_seg_progress_chunk:
        (Task_.task_seg_id, (int64, int64), t) => action_record;

      let add_task_inst_progress_chunk:
        (Task_.task_inst_id, (int64, int64), t) => action_record;
    };
  };
};

module Maybe_append_to_head: {
  let remove_task: (Task_.task_id, t) => action_record;

  let remove_task_inst: (Task_.task_inst_id, t) => action_record;

  let remove_task_seg_progress_chunk:
    (Task_.task_seg_id, (int64, int64), t) => action_record;

  let remove_task_inst_progress_chunk:
    (Task_.task_inst_id, (int64, int64), t) => action_record;

  let remove_pending_sched_req: (Sched_req.sched_req_id, t) => action_record;

  let sched:
    (
      ~start: int64,
      ~end_exc: int64,
      ~include_sched_reqs_starting_within_time_slot: bool,
      ~include_sched_reqs_ending_within_time_slot: bool,
      ~up_to_sched_req_id_inc: option(Sched_req.sched_req_id),
      t
    ) =>
    (result(unit, unit), action_record);
};

module Append_to_head: {let snapshot: t => action_record;};

module Equal: {let equal: (t, t) => bool;};

module Serialize: {
  let base_and_diffs_of_list:
    list(Sched.sched) => option((Sched.sched, list(Sched.sched_diff)));

  let to_base_and_diffs: t => option((Sched.sched, list(Sched.sched_diff)));

  let write_to_dir: (~dir: string, t) => result(unit, string);
};

module Deserialize: {
  let list_of_base_and_diffs:
    (Sched.sched, list(Sched.sched_diff)) => list(Sched.sched);

  let of_base_and_diffs: (Sched.sched, list(Sched.sched_diff)) => t;

  let read_from_dir: (~dir: string) => result(t, string);
};

module To_string: {
  let debug_string_of_sched_ver_history:
    (~indent_level: int=?, ~buffer: Buffer.t=?, t) => string;

  let debug_string_of_action_record:
    (~indent_level: int=?, ~buffer: Buffer.t=?, action_record) => string;
};

module Print: {
  let debug_print_sched_ver_history: (~indent_level: int=?, t) => unit;

  let debug_print_action_record: (~indent_level: int=?, action_record) => unit;
};
