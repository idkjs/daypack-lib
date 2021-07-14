module Task_ = Task;
module Sched_req_ = Sched_req;

type sched_id = int;

type task_store = Task_id_map.t(Task_.task_data);

type task_store_diff = Task_id_map_utils.diff(Task_.task_data);

type task_inst_store = Task_inst_id_map.t(Task_.task_inst_data);

type task_inst_store_diff = Task_inst_id_map_utils.diff(Task_.task_inst_data);

type task_seg_store = Task_seg_id_map.t(Task_.task_seg_size);

type task_seg_store_diff = Task_seg_id_map_utils.diff(Task_.task_seg_size);

type sched_req_store = Sched_req_id_map.t(Sched_req_.sched_req_data);

type sched_req_store_diff =
  Sched_req_id_map_utils.diff(Sched_req_.sched_req_data);

type sched_req_record_store =
  Sched_req_id_map.t(Sched_req_.sched_req_record_data);

type sched_req_record_store_diff =
  Sched_req_id_map_utils.diff(Sched_req_.sched_req_record_data);

type task_seg_place_map = Int64_map.t(Task_seg_id_set.t);

type task_seg_place_map_diff = Int64_map_utils.Task_seg_id_bucketed.diff_bucketed;

type task_related_status = [ | `Uncompleted | `Completed | `Discarded];

type sched_req_status = [ | `Pending | `Discarded | `Recorded];

type store = {
  task_uncompleted_store: task_store,
  task_completed_store: task_store,
  task_discarded_store: task_store,
  task_inst_uncompleted_store: task_inst_store,
  task_inst_completed_store: task_inst_store,
  task_inst_discarded_store: task_inst_store,
  task_seg_uncompleted_store: task_seg_store,
  task_seg_completed_store: task_seg_store,
  task_seg_discarded_store: task_seg_store,
  user_id_to_task_ids: User_id_map.t(Int64_set.t),
  task_id_to_task_inst_ids: Task_id_map.t(Int64_set.t),
  task_inst_id_to_task_seg_ids: Task_inst_id_map.t(Int64_int64_option_set.t),
  sched_req_ids: Int64_set.t,
  sched_req_pending_store: sched_req_store,
  sched_req_discarded_store: sched_req_store,
  sched_req_record_store,
  quota: Task_inst_id_map.t(int64),
  task_seg_id_to_progress: Task_seg_id_map.t(Task_.progress),
  task_inst_id_to_progress: Task_inst_id_map.t(Task_.progress),
};

type store_diff = {
  task_uncompleted_store_diff: task_store_diff,
  task_completed_store_diff: task_store_diff,
  task_discarded_store_diff: task_store_diff,
  task_inst_uncompleted_store_diff: task_inst_store_diff,
  task_inst_completed_store_diff: task_inst_store_diff,
  task_inst_discarded_store_diff: task_inst_store_diff,
  task_seg_uncompleted_store_diff: task_seg_store_diff,
  task_seg_completed_store_diff: task_seg_store_diff,
  task_seg_discarded_store_diff: task_seg_store_diff,
  user_id_to_task_ids_diff: User_id_map_utils.Int64_bucketed.diff_bucketed,
  task_id_to_task_inst_ids_diff: Task_id_map_utils.Int64_bucketed.diff_bucketed,
  task_inst_id_to_task_seg_ids_diff: Task_inst_id_map_utils.Int64_int64_option_bucketed.diff_bucketed,
  sched_req_ids_diff: Int64_set_utils.diff,
  sched_req_pending_store_diff: sched_req_store_diff,
  sched_req_discarded_store_diff: sched_req_store_diff,
  sched_req_record_store_diff,
  quota_diff: Task_inst_id_map_utils.diff(int64),
  task_seg_id_to_progress_diff: Task_seg_id_map_utils.diff(Task_.progress),
  task_inst_id_to_progress_diff: Task_inst_id_map_utils.diff(Task_.progress),
};

type agenda = {
  indexed_by_task_seg_id: Task_seg_id_map.t((int64, int64)),
  indexed_by_start: task_seg_place_map,
  indexed_by_end_exc: task_seg_place_map,
};

type agenda_diff = {
  indexed_by_task_seg_id_diff: Task_seg_id_map_utils.diff((int64, int64)),
  indexed_by_start_diff: task_seg_place_map_diff,
  indexed_by_end_exc_diff: task_seg_place_map_diff,
};

type sched_data = {
  store,
  agenda,
};

type sched_data_diff = {
  store_diff,
  agenda_diff,
};

type sched = (sched_id, sched_data);

type sched_diff = (sched_id, sched_id, sched_data_diff);

let sched_data_empty: sched_data;

let empty: sched;

module Quota: {
  let update_quota: (Task_inst_id_map.t(int64), sched) => sched;

  let add_quota: (Task_inst_id_map.t(int64), sched) => sched;
};

module Task: {
  module Status: {
    let get_task_status:
      (Task_.task_id, sched) => option(task_related_status);
  };

  module Add: {
    let add_task:
      (
        ~parent_user_id: Task_.user_id,
        Task_.task_data,
        list(Task_.task_inst_data),
        sched
      ) =>
      (Task_.task, list(Task_.task_inst), sched);
  };

  module To_seq: {
    let task_seq_uncompleted: sched => Seq.t(Task_.task);

    let task_seq_completed: sched => Seq.t(Task_.task);

    let task_seq_discarded: sched => Seq.t(Task_.task);

    let task_seq_all: sched => Seq.t(Task_.task);
  };

  module Find: {
    let find_task_uncompleted_opt:
      (Task_.task_id, sched) => option(Task_.task_data);

    let find_task_completed_opt:
      (Task_.task_id, sched) => option(Task_.task_data);

    let find_task_discarded_opt:
      (Task_.task_id, sched) => option(Task_.task_data);

    let find_task_any_opt: (Task_.task_id, sched) => option(Task_.task_data);

    let find_task_any_with_status_opt:
      (Task_.task_id, sched) =>
      option((Task_.task_data, task_related_status));
  };

  module Remove: {
    let remove_task_uncompleted:
      (
        ~remove_children_task_insts: bool=?,
        ~remove_children_task_segs: bool=?,
        Task_.task_id,
        sched
      ) =>
      sched;

    let remove_task_completed:
      (
        ~remove_children_task_insts: bool=?,
        ~remove_children_task_segs: bool=?,
        Task_.task_id,
        sched
      ) =>
      sched;

    let remove_task_discarded:
      (
        ~remove_children_task_insts: bool=?,
        ~remove_children_task_segs: bool=?,
        Task_.task_id,
        sched
      ) =>
      sched;

    let remove_task_all:
      (
        ~remove_children_task_insts: bool=?,
        ~remove_children_task_segs: bool=?,
        Task_.task_id,
        sched
      ) =>
      sched;
  };

  module Move: {
    let move_task_to_completed: (Task_.task_id, sched) => sched;

    let move_task_to_uncompleted: (Task_.task_id, sched) => sched;

    let move_task_to_discarded: (Task_.task_id, sched) => sched;
  };
};

module Task_inst: {
  module Status: {
    let get_task_inst_status:
      (Task_.task_inst_id, sched) => option(task_related_status);
  };

  module Add: {
    let add_task_inst:
      (~parent_task_id: Task_.task_id, Task_.task_inst_data, sched) =>
      (Task_.task_inst, sched);

    let add_task_inst_list:
      (~parent_task_id: Task_.task_id, list(Task_.task_inst_data), sched) =>
      (list(Task_.task_inst), sched);
  };

  module To_seq: {
    let task_inst_seq_uncompleted: sched => Seq.t(Task_.task_inst);

    let task_inst_seq_completed: sched => Seq.t(Task_.task_inst);

    let task_inst_seq_discarded: sched => Seq.t(Task_.task_inst);

    let task_inst_seq_all: sched => Seq.t(Task_.task_inst);
  };

  module Find: {
    let find_task_inst_uncompleted_opt:
      (Task_.task_inst_id, sched) => option(Task_.task_inst_data);

    let find_task_inst_completed_opt:
      (Task_.task_inst_id, sched) => option(Task_.task_inst_data);

    let find_task_inst_discarded_opt:
      (Task_.task_inst_id, sched) => option(Task_.task_inst_data);

    let find_task_inst_any_opt:
      (Task_.task_inst_id, sched) => option(Task_.task_inst_data);

    let find_task_inst_any_with_status_opt:
      (Task_.task_inst_id, sched) =>
      option((Task_.task_inst_data, task_related_status));

    let find_task_inst_ids_by_task_id:
      (Task_.task_id, sched) => Seq.t(Task_.task_inst_id);

    let find_task_inst_seq_uncompleted_by_task_id:
      (Task_.task_id, sched) => Seq.t(Task_.task_inst);

    let find_task_inst_seq_completed_by_task_id:
      (Task_.task_id, sched) => Seq.t(Task_.task_inst);

    let find_task_inst_seq_discarded_by_task_id:
      (Task_.task_id, sched) => Seq.t(Task_.task_inst);

    let find_task_inst_seq_any_by_task_id:
      (Task_.task_id, sched) => Seq.t(Task_.task_inst);

    let find_task_inst_seq_any_with_status_by_task_id:
      (Task_.task_id, sched) => Seq.t((Task_.task_inst, task_related_status));
  };

  module Remove: {
    let remove_task_inst_uncompleted:
      (~remove_children_task_segs: bool=?, Task_.task_inst_id, sched) => sched;

    let remove_task_inst_completed:
      (~remove_children_task_segs: bool=?, Task_.task_inst_id, sched) => sched;

    let remove_task_inst_discarded:
      (~remove_children_task_segs: bool=?, Task_.task_inst_id, sched) => sched;

    let remove_task_inst_all:
      (~remove_children_task_segs: bool=?, Task_.task_inst_id, sched) => sched;

    /* val remove_task_inst_uncompleted_strict :
     *   ?remove_children_task_segs:bool ->
     *   Task_.task_inst_id ->
     *   sched ->
     *   (sched, unit) result
     *
     * val remove_task_inst_completed_strict :
     *   ?remove_children_task_segs:bool ->
     *   Task_.task_inst_id ->
     *   sched ->
     *   (sched, unit) result
     *
     * val remove_task_inst_discarded_strict :
     *   ?remove_children_task_segs:bool ->
     *   Task_.task_inst_id ->
     *   sched ->
     *   (sched, unit) result */

    let remove_task_inst_uncompleted_seq:
      (
        ~remove_children_task_segs: bool=?,
        Seq.t(Task_.task_inst_id),
        sched
      ) =>
      sched;

    let remove_task_inst_completed_seq:
      (
        ~remove_children_task_segs: bool=?,
        Seq.t(Task_.task_inst_id),
        sched
      ) =>
      sched;

    let remove_task_inst_discarded_seq:
      (
        ~remove_children_task_segs: bool=?,
        Seq.t(Task_.task_inst_id),
        sched
      ) =>
      sched;
  };

  module Move: {
    let move_task_inst_to_completed: (Task_.task_inst_id, sched) => sched;

    let move_task_inst_to_uncompleted: (Task_.task_inst_id, sched) => sched;

    let move_task_inst_to_discarded: (Task_.task_inst_id, sched) => sched;
  };
};

module Task_seg: {
  module Status: {
    let get_task_seg_status:
      (Task_.task_seg_id, sched) => option(task_related_status);
  };

  module Add: {
    let add_task_seg:
      (~parent_task_inst_id: Task_.task_inst_id, Task_.task_seg_size, sched) =>
      (Task_.task_seg, sched);

    let add_task_seg_via_task_seg_alloc_req:
      (Task_.task_seg_alloc_req, sched) => (Task_.task_seg, sched);

    let add_task_segs_via_task_seg_alloc_req_list:
      (list(Task_.task_seg_alloc_req), sched) =>
      (list(Task_.task_seg), sched);

    let add_task_seg_via_task_seg_place:
      (Task_.task_seg_place, sched) => sched;

    let add_task_segs_via_task_seg_place_list:
      (list(Task_.task_seg_place), sched) => sched;

    let add_task_segs_via_task_seg_place_seq:
      (Seq.t(Task_.task_seg_place), sched) => sched;
  };

  module To_seq: {
    let task_seg_seq_uncompleted: sched => Seq.t(Task_.task_seg);

    let task_seg_seq_completed: sched => Seq.t(Task_.task_seg);

    let task_seg_seq_discarded: sched => Seq.t(Task_.task_seg);

    let task_seg_seq_all: sched => Seq.t(Task_.task_seg);
  };

  module Find: {
    let find_task_seg_uncompleted_opt:
      (Task_.task_seg_id, sched) => option(Task_.task_seg_size);

    let find_task_seg_completed_opt:
      (Task_.task_seg_id, sched) => option(Task_.task_seg_size);

    let find_task_seg_discarded_opt:
      (Task_.task_seg_id, sched) => option(Task_.task_seg_size);

    let find_task_seg_any_opt:
      (Task_.task_seg_id, sched) => option(Task_.task_seg_size);

    let find_task_seg_any_with_status_opt:
      (Task_.task_seg_id, sched) =>
      option((Task_.task_seg_size, task_related_status));

    let find_task_seg_ids_by_task_inst_id:
      (Task_.task_inst_id, sched) => Seq.t(Task_.task_seg_id);

    let find_task_seg_seq_uncompleted_by_task_inst_id:
      (Task_.task_inst_id, sched) => Seq.t(Task_.task_seg);

    let find_task_seg_seq_completed_by_task_inst_id:
      (Task_.task_inst_id, sched) => Seq.t(Task_.task_seg);

    let find_task_seg_seq_discarded_by_task_inst_id:
      (Task_.task_inst_id, sched) => Seq.t(Task_.task_seg);

    let find_task_seg_seq_any_by_task_inst_id:
      (Task_.task_inst_id, sched) => Seq.t(Task_.task_seg);

    let find_task_seg_seq_any_with_status_by_task_inst_id:
      (Task_.task_inst_id, sched) =>
      Seq.t((Task_.task_seg, task_related_status));

    let find_task_seg_ids_by_task_id:
      (Task_.task_id, sched) => Seq.t(Task_.task_seg_id);

    let find_task_seg_seq_uncompleted_by_task_id:
      (Task_.task_id, sched) => Seq.t(Task_.task_seg);

    let find_task_seg_seq_completed_by_task_id:
      (Task_.task_id, sched) => Seq.t(Task_.task_seg);

    let find_task_seg_seq_discarded_by_task_id:
      (Task_.task_id, sched) => Seq.t(Task_.task_seg);

    let find_task_seg_seq_any_by_task_id:
      (Task_.task_id, sched) => Seq.t(Task_.task_seg);

    let find_task_seg_seq_any_with_status_by_task_id:
      (Task_.task_id, sched) => Seq.t((Task_.task_seg, task_related_status));
  };

  module Remove: {
    let remove_task_seg_uncompleted: (Task_.task_seg_id, sched) => sched;

    let remove_task_seg_completed: (Task_.task_seg_id, sched) => sched;

    let remove_task_seg_discarded: (Task_.task_seg_id, sched) => sched;

    let remove_task_seg_all: (Task_.task_seg_id, sched) => sched;

    let remove_task_seg_uncompleted_seq:
      (Seq.t(Task_.task_seg_id), sched) => sched;

    let remove_task_seg_completed_seq:
      (Seq.t(Task_.task_seg_id), sched) => sched;

    let remove_task_seg_discarded_seq:
      (Seq.t(Task_.task_seg_id), sched) => sched;
  };

  module Move: {
    let move_task_seg_to_completed: (Task_.task_seg_id, sched) => sched;

    let move_task_seg_to_uncompleted: (Task_.task_seg_id, sched) => sched;

    let move_task_seg_to_discarded: (Task_.task_seg_id, sched) => sched;
  };
};

module Progress: {
  module Add: {
    let add_task_seg_progress_chunk:
      (Task_.task_seg_id, (int64, int64), sched) => sched;

    let add_task_seg_progress_chunk:
      (Task_.task_seg_id, (int64, int64), sched) => sched;

    let add_task_inst_progress_chunk:
      (Task_.task_inst_id, (int64, int64), sched) => sched;
  };

  module Find: {
    let find_task_seg_progress:
      (Task_.task_seg_id, sched) => option(Task_.progress);

    let find_task_seg_progress_chunk_set:
      (Task_.task_seg_id, sched) => Int64_int64_set.t;

    let find_task_seg_progress_chunk_seq:
      (Task_.task_seg_id, sched) => Seq.t((int64, int64));

    let find_task_seg_progress:
      (Task_.task_seg_id, sched) => option(Task_.progress);

    let find_task_seg_progress_seq_by_task_inst_id:
      (Task_.task_inst_id, sched) => Seq.t(Task_.progress);

    let find_task_seg_progress_seq_by_task_id:
      (Task_.task_id, sched) => Seq.t(Task_.progress);

    let find_task_seg_progress_chunk_set:
      (Task_.task_seg_id, sched) => Int64_int64_set.t;

    let find_task_seg_progress_chunk_seq:
      (Task_.task_seg_id, sched) => Seq.t((int64, int64));

    let find_task_seg_progress_chunk_seq_by_task_inst_id:
      (Task_.task_inst_id, sched) => Seq.t((int64, int64));

    let find_task_seg_progress_chunk_seq_by_task_id:
      (Task_.task_id, sched) => Seq.t((int64, int64));

    let find_task_inst_progress:
      (Task_.task_inst_id, sched) => option(Task_.progress);

    let find_task_inst_progress_seq_by_task_id:
      (Task_.task_id, sched) => Seq.t(Task_.progress);

    let find_task_inst_progress_chunk_set:
      (Task_.task_inst_id, sched) => Int64_int64_set.t;

    let find_task_inst_progress_chunk_seq:
      (Task_.task_inst_id, sched) => Seq.t((int64, int64));

    let find_task_inst_progress_chunk_seq_by_task_id:
      (Task_.task_id, sched) => Seq.t((int64, int64));
  };

  module Remove: {
    let remove_task_seg_progress_chunk:
      (Task_.task_seg_id, (int64, int64), sched) => sched;

    let remove_task_seg_progress_chunk:
      (Task_.task_seg_id, (int64, int64), sched) => sched;

    let remove_task_inst_progress_chunk:
      (Task_.task_inst_id, (int64, int64), sched) => sched;
  };
};

module Agenda: {
  module Add: {
    let add_task_seg_place: (Task_.task_seg_place, sched) => sched;

    let add_task_seg_place_list: (list(Task_.task_seg_place), sched) => sched;

    let add_task_seg_place_seq: (Seq.t(Task_.task_seg_place), sched) => sched;
  };

  module Range: {
    let task_seg_id_set:
      (
        ~start: option(int64),
        ~end_exc: option(int64),
        ~include_task_seg_place_starting_within_time_slot: bool,
        ~include_task_seg_place_ending_within_time_slot: bool,
        sched
      ) =>
      Task_seg_id_set.t;

    let task_seg_place_set:
      (
        ~start: option(int64),
        ~end_exc: option(int64),
        ~include_task_seg_place_starting_within_time_slot: bool,
        ~include_task_seg_place_ending_within_time_slot: bool,
        sched
      ) =>
      Task_seg_place_set.t;
  };

  module Filter: {
    let filter_task_seg_place_seq:
      (
        ~start: int64=?,
        ~end_exc: int64=?,
        ~include_task_seg_place_starting_within_time_slot: bool=?,
        ~include_task_seg_place_ending_within_time_slot: bool=?,
        Task_.task_seg_place => bool,
        sched
      ) =>
      Seq.t(Task_.task_seg_place);
  };

  module To_seq: {
    let task_seg_place_uncompleted:
      (
        ~start: int64=?,
        ~end_exc: int64=?,
        ~include_task_seg_place_starting_within_time_slot: bool=?,
        ~include_task_seg_place_ending_within_time_slot: bool=?,
        sched
      ) =>
      Seq.t(Task_.task_seg_place);

    let task_seg_place_completed:
      (
        ~start: int64=?,
        ~end_exc: int64=?,
        ~include_task_seg_place_starting_within_time_slot: bool=?,
        ~include_task_seg_place_ending_within_time_slot: bool=?,
        sched
      ) =>
      Seq.t(Task_.task_seg_place);

    let task_seg_place_discarded:
      (
        ~start: int64=?,
        ~end_exc: int64=?,
        ~include_task_seg_place_starting_within_time_slot: bool=?,
        ~include_task_seg_place_ending_within_time_slot: bool=?,
        sched
      ) =>
      Seq.t(Task_.task_seg_place);

    let task_seg_place_all:
      (
        ~start: int64=?,
        ~end_exc: int64=?,
        ~include_task_seg_place_starting_within_time_slot: bool=?,
        ~include_task_seg_place_ending_within_time_slot: bool=?,
        sched
      ) =>
      Seq.t(Task_.task_seg_place);
  };

  module Find: {
    let find_task_seg_place_seq_by_task_id:
      (Task_.task_id, sched) => Seq.t(Task_.task_seg_place);

    let find_task_seg_place_seq_by_task_inst_id:
      (Task_.task_inst_id, sched) => Seq.t(Task_.task_seg_place);

    let find_task_seg_place_opt_by_task_seg_id:
      (Task_.task_seg_id, sched) => option(Task_.task_seg_place);
  };

  module Remove: {
    let remove_task_seg_place: (Task_.task_seg_place, sched) => sched;

    let remove_task_seg_place_seq:
      (Seq.t(Task_.task_seg_place), sched) => sched;

    let remove_task_seg_place_by_task_id: (Task_.task_id, sched) => sched;

    let remove_task_seg_place_by_task_inst_id:
      (Task_.task_inst_id, sched) => sched;

    let remove_task_seg_place_by_task_seg_id:
      (Task_.task_seg_id, sched) => sched;
  };

  module Time_slot: {
    let get_occupied_time_slots:
      (
        ~exclude_parallelizable_task_seg_places: bool=?,
        ~start: int64=?,
        ~end_exc: int64=?,
        sched
      ) =>
      Seq.t((int64, int64));

    let get_occupied_time_slots_with_task_seg_place_count:
      (
        ~exclude_parallelizable_task_seg_places: bool=?,
        ~start: int64=?,
        ~end_exc: int64=?,
        sched
      ) =>
      Seq.t(((int64, int64), int));

    let get_occupied_time_slots_up_to_task_seg_place_count:
      (
        ~exclude_parallelizable_task_seg_places: bool=?,
        ~start: int64=?,
        ~end_exc: int64=?,
        ~up_to_task_seg_place_count_inc: int,
        sched
      ) =>
      Seq.t((int64, int64));

    let get_free_time_slots:
      (
        ~include_parallelizable_task_seg_places: bool=?,
        ~start: int64,
        ~end_exc: int64,
        sched
      ) =>
      Seq.t((int64, int64));

    let get_free_or_occupied_time_slots_up_to_task_seg_place_count:
      (
        ~include_parallelizable_task_seg_places: bool=?,
        ~start: int64,
        ~end_exc: int64,
        ~up_to_task_seg_place_count_inc: int,
        sched
      ) =>
      Seq.t((int64, int64));

    let task_seg_place_count_in_time_slot:
      (~start: int64, ~end_exc: int64, sched) => int;
  };
};

module Sched_req: {
  module Status: {
    let get_sched_req_status:
      (Sched_req_.sched_req_id, sched) => option(sched_req_status);
  };

  module Add: {
    let add_sched_req_data:
      (Sched_req_.sched_req_data, sched) =>
      result((Sched_req_.sched_req, sched), unit);

    let add_sched_req_data_list:
      (list(Sched_req_.sched_req_data), sched) =>
      result((list(Sched_req_.sched_req), sched), unit);
  };

  module Partition: {
    type partition_based_on_time_point('a) = {
      before: Sched_req_id_map.t('a),
      after: Sched_req_id_map.t('a),
      crossing: Sched_req_id_map.t('a),
    };

    type partition_based_on_time_slot('a) = {
      fully_within: Sched_req_id_map.t('a),
      starting_within: Sched_req_id_map.t('a),
      ending_within: Sched_req_id_map.t('a),
      outside: Sched_req_id_map.t('a),
    };

    module Pending: {
      let partition_based_on_time_point:
        (int64, sched) =>
        partition_based_on_time_point(Sched_req_.sched_req_data);

      let partition_based_on_time_slot:
        (~start: int64, ~end_exc: int64, sched) =>
        partition_based_on_time_slot(Sched_req_.sched_req_data);
    };

    module Record: {
      let partition_based_on_time_point:
        (int64, sched) =>
        partition_based_on_time_point(Sched_req_.sched_req_record_data);

      let partition_based_on_time_slot:
        (~start: int64, ~end_exc: int64, sched) =>
        partition_based_on_time_slot(Sched_req_.sched_req_record_data);
    };
  };

  module To_seq: {
    module Pending: {
      let pending_sched_req_seq:
        (
          ~start: int64=?,
          ~end_exc: int64=?,
          ~include_sched_req_starting_within_time_slot: bool=?,
          ~include_sched_req_ending_within_time_slot: bool=?,
          sched
        ) =>
        Seq.t(Sched_req_.sched_req);
    };

    module Record: {
      let sched_req_record_seq:
        (
          ~start: int64=?,
          ~end_exc: int64=?,
          ~include_sched_req_record_starting_within_time_slot: bool=?,
          ~include_sched_req_record_ending_within_time_slot: bool=?,
          sched
        ) =>
        Seq.t(Sched_req_.sched_req_record);
    };
  };

  module Filter: {
    module Pending: {
      let filter_pending_sched_req_seq:
        (
          ~start: int64=?,
          ~end_exc: int64=?,
          ~include_sched_req_starting_within_time_slot: bool=?,
          ~include_sched_req_ending_within_time_slot: bool=?,
          Sched_req_.sched_req => bool,
          sched
        ) =>
        Seq.t(Sched_req_.sched_req);
    };

    module Record: {
      let filter_sched_req_record_seq:
        (
          ~start: int64=?,
          ~end_exc: int64=?,
          ~include_sched_req_record_starting_within_time_slot: bool=?,
          ~include_sched_req_record_ending_within_time_slot: bool=?,
          Sched_req_.sched_req_record => bool,
          sched
        ) =>
        Seq.t(Sched_req_.sched_req_record);
    };
  };

  module Find: {
    module Pending: {
      let find_pending_sched_req:
        (Sched_req_.sched_req_id, sched) => option(Sched_req_.sched_req_data);

      let find_pending_sched_req_by_task_id:
        (Task_.task_id, sched) => Seq.t(Sched_req_.sched_req);

      let find_pending_sched_req_by_task_inst_id:
        (Task_.task_inst_id, sched) => Seq.t(Sched_req_.sched_req);
    };

    module Record: {
      let find_sched_req_record:
        (Sched_req_.sched_req_id, sched) =>
        option(Sched_req_.sched_req_record_data);

      let find_sched_req_record_by_task_id:
        (Task_.task_id, sched) => Seq.t(Sched_req_.sched_req_record);

      let find_sched_req_record_by_task_inst_id:
        (Task_.task_inst_id, sched) => Seq.t(Sched_req_.sched_req_record);

      let find_sched_req_record_by_task_seg_id:
        (Task_.task_seg_id, sched) => Seq.t(Sched_req_.sched_req_record);
    };
  };

  module Remove: {
    module Pending: {
      let remove_pending_sched_req: (Sched_req_.sched_req_id, sched) => sched;

      let remove_pending_sched_req_if_contains_matching_task_seg_alloc_req:
        (Task_.task_seg_alloc_req => bool, sched) => sched;

      let
        remove_pending_sched_req_data_unit_if_contains_matching_task_seg_alloc_req:
        (Task_.task_seg_alloc_req => bool, sched) => sched;

      let remove_pending_sched_req_by_task_id: (Task_.task_id, sched) => sched;

      let remove_pending_sched_req_by_task_inst_id:
        (Task_.task_inst_id, sched) => sched;

      let remove_pending_sched_req_by_task_seg_id:
        (Task_.task_seg_id, sched) => sched;

      let remove_pending_sched_req_data_unit_by_task_id:
        (Task_.task_id, sched) => sched;

      let remove_pending_sched_req_data_unit_by_task_inst_id:
        (Task_.task_inst_id, sched) => sched;

      let remove_pending_sched_req_data_unit_by_task_seg_id:
        (Task_.task_seg_id, sched) => sched;
    };

    module Record: {
      let remove_sched_req_record: (Sched_req_.sched_req_id, sched) => sched;

      let remove_sched_req_record_if_contains_matching_task_seg:
        (Task_.task_seg => bool, sched) => sched;

      let remove_sched_req_record_data_unit_if_contains_matching_task_seg:
        (Task_.task_seg => bool, sched) => sched;

      let remove_sched_req_record_by_task_id: (Task_.task_id, sched) => sched;

      let remove_sched_req_record_by_task_inst_id:
        (Task_.task_inst_id, sched) => sched;

      let remove_sched_req_record_by_task_seg_id:
        (Task_.task_seg_id, sched) => sched;

      let remove_sched_req_record_data_unit_by_task_id:
        (Task_.task_id, sched) => sched;

      let remove_sched_req_record_data_unit_by_task_inst_id:
        (Task_.task_inst_id, sched) => sched;

      let remove_sched_req_record_data_unit_by_task_seg_id:
        (Task_.task_seg_id, sched) => sched;
    };
  };

  module Discard: {
    let discard_pending_sched_req: (Sched_req_.sched_req_id, sched) => sched;
  };

  module Allocate_task_segs: {
    let allocate_task_segs_for_pending_sched_reqs:
      (
        ~start: int64,
        ~end_exc: int64,
        ~include_sched_reqs_starting_within_time_slot: bool,
        ~include_sched_reqs_ending_within_time_slot: bool,
        ~up_to_sched_req_id_inc: option(Sched_req_.sched_req_id),
        sched
      ) =>
      (list(Sched_req_.sched_req_record), sched);
  };
};

module Recur: {
  let instantiate: (~start: int64, ~end_exc: int64, sched) => sched;
};

module Overdue: {
  let get_overdue_task_seg_places:
    (~deadline: int64, sched) => Seq.t(Task_.task_seg_place);

  let get_overdue_task_segs:
    (~deadline: int64, sched) => Seq.t(Task_.task_seg);

  let add_sched_reqs_for_overdue_task_segs:
    (~start: int64, ~end_exc: int64, sched) => sched;
};

module Serialize: {
  let pack_task_uncompleted_store: task_store => list(Sched_t.task);

  let pack_task_completed_store: task_store => list(Sched_t.task);

  let pack_task_discarded_store: task_store => list(Sched_t.task);

  let pack_task_inst_uncompleted_store:
    task_inst_store => list(Sched_t.task_inst);

  let pack_task_inst_completed_store:
    task_inst_store => list(Sched_t.task_inst);

  let pack_task_inst_discarded_store:
    task_inst_store => list(Sched_t.task_inst);

  let pack_task_seg_uncompleted_store:
    task_seg_store => list(Sched_t.task_seg);

  let pack_task_seg_completed_store: task_seg_store => list(Sched_t.task_seg);

  let pack_task_seg_discarded_store: task_seg_store => list(Sched_t.task_seg);

  let pack_sched_req_pending_store:
    sched_req_store => list(Sched_req_t.sched_req);

  let pack_sched_req_record_store:
    sched_req_record_store => list(Sched_req_t.sched_req_record);

  let pack_quota:
    Task_inst_id_map.t(int64) =>
    list((Task_t.task_inst_id, (int32, int32)));

  let pack_user_id_to_task_ids:
    User_id_map.t(Int64_set.t) =>
    list((Task_t.user_id, list((int32, int32))));

  let pack_task_id_to_task_inst_ids:
    Task_id_map.t(Int64_set.t) =>
    list((Task_t.task_id, list((int32, int32))));

  let pack_task_inst_id_to_task_seg_ids:
    Task_inst_id_map.t(Int64_int64_option_set.t) =>
    list(
      (
        Task_t.task_inst_id,
        list(((int32, int32), option((int32, int32)))),
      ),
    );

  let pack_task_seg_id_to_progress:
    Task_seg_id_map.t(Task_.progress) =>
    list((Task_t.task_seg_id, Task_t.progress));

  let pack_task_inst_id_to_progress:
    Task_inst_id_map.t(Task_.progress) =>
    list((Task_t.task_inst_id, Task_t.progress));

  let pack_indexed_by_task_seg_id:
    Task_seg_id_map.t((int64, int64)) =>
    list((Task_t.task_seg_id, ((int32, int32), (int32, int32))));

  let pack_indexed_by_start:
    task_seg_place_map => list(((int32, int32), list(Task_t.task_seg_id)));

  let pack_indexed_by_end_exc:
    task_seg_place_map => list(((int32, int32), list(Task_t.task_seg_id)));

  let pack_sched_req_ids: Int64_set.t => list((int32, int32));

  let pack_sched: sched => Sched_t.sched;

  let pack_sched_diff: sched_diff => Sched_t.sched_diff;

  let json_string_of_sched: sched => string;

  let json_string_of_sched_diff: sched_diff => string;
};

module Deserialize: {
  let unpack_task_uncompleted_list: list(Sched_t.task) => task_store;

  let unpack_task_completed_list: list(Sched_t.task) => task_store;

  let unpack_task_discarded_list: list(Sched_t.task) => task_store;

  let unpack_task_inst_uncompleted_list:
    list(Sched_t.task_inst) => task_inst_store;

  let unpack_task_inst_completed_list:
    list(Sched_t.task_inst) => task_inst_store;

  let unpack_task_inst_discarded_list:
    list(Sched_t.task_inst) => task_inst_store;

  let unpack_task_seg_uncompleted_list:
    list(Sched_t.task_seg) => task_seg_store;

  let unpack_task_seg_completed_list:
    list(Sched_t.task_seg) => task_seg_store;

  let unpack_task_seg_discarded_list:
    list(Sched_t.task_seg) => task_seg_store;

  let unpack_sched_req_pending_list:
    list(Sched_req_t.sched_req) => sched_req_store;

  let unpack_sched_req_record_list:
    list(Sched_req_t.sched_req_record) => sched_req_record_store;

  let unpack_quota:
    list((Task_t.task_inst_id, (int32, int32))) =>
    Task_inst_id_map.t(int64);

  let unpack_user_id_to_task_ids:
    list((Task_t.user_id, list((int32, int32)))) =>
    User_id_map.t(Int64_set.t);

  let unpack_task_id_to_task_inst_ids:
    list((Task_t.task_id, list((int32, int32)))) =>
    Task_id_map.t(Int64_set.t);

  let unpack_task_inst_id_to_task_seg_ids:
    list(
      (
        Task_t.task_inst_id,
        list(((int32, int32), option((int32, int32)))),
      ),
    ) =>
    Task_inst_id_map.t(Int64_int64_option_set.t);

  let unpack_task_seg_id_to_progress:
    list((Task_t.task_seg_id, Task_t.progress)) =>
    Task_seg_id_map.t(Task_.progress);

  let unpack_task_inst_id_to_progress:
    list((Task_t.task_inst_id, Task_t.progress)) =>
    Task_inst_id_map.t(Task_.progress);

  let unpack_indexed_by_task_seg_id:
    list((Task_t.task_seg_id, ((int32, int32), (int32, int32)))) =>
    Task_seg_id_map.t((int64, int64));

  let unpack_indexed_by_end_exc:
    list(((int32, int32), list(Task_t.task_seg_id))) => task_seg_place_map;

  let unpack_indexed_by_start:
    list(((int32, int32), list(Task_t.task_seg_id))) => task_seg_place_map;

  let unpack_sched_req_ids: list((int32, int32)) => Int64_set.t;

  let unpack_sched: Sched_t.sched => sched;

  let unpack_sched_diff: Sched_t.sched_diff => sched_diff;

  let sched_of_json_string: string => sched;

  let sched_diff_of_json_string: string => sched_diff;
};

module Equal: {
  let sched_data_equal: (sched_data, sched_data) => bool;

  let sched_equal: (sched, sched) => bool;
};

module Diff: {
  let diff_sched_data: (~old: sched_data, sched_data) => sched_data_diff;

  let diff_sched: (~old: sched, sched) => sched_diff;

  let add_diff_sched_data: (sched_data_diff, sched_data) => sched_data;

  let add_diff_sched: (sched_diff, sched) => sched;

  let sub_diff_sched_data: (sched_data_diff, sched_data) => sched_data;

  let sub_diff_sched: (sched_diff, sched) => sched;
};

module To_string: {
  let string_of_task_related_status: task_related_status => string;

  let debug_string_of_sched:
    (~indent_level: int=?, ~buffer: Buffer.t=?, sched) => string;
};

module Print: {let debug_print_sched: (~indent_level: int=?, sched) => unit;};
