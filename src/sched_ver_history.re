module Task_ = Task;

type t = {mutable history: list(Sched.sched)};

type head_choice =
  | Replace_head(Sched.sched)
  | New_head(Sched.sched)
  | Do_nothing;

type action_record =
  | Updated_head(Sched.sched_id)
  | Added_new_head(Sched.sched_id)
  | Did_nothing;

let make_empty = () => {history: []};

let of_sched_list = history => {history: history};

let map_head =
    (f: Sched.sched => ('a, head_choice), t: t): ('a, action_record) =>
  switch (t.history) {
  | [] =>
    let (empty_sid, _) = Sched.empty;
    let (ret, choice) = f(Sched.empty);
    (
      ret,
      switch (choice) {
      | [@implicit_arity] Replace_head(_, sd) =>
        t.history = [(empty_sid, sd)];
        Added_new_head(empty_sid);
      | [@implicit_arity] New_head(sid, sd) =>
        t.history = [(sid, sd)];
        Added_new_head(sid);
      | Do_nothing => Did_nothing
      },
    );
  | [hd, ...tl] =>
    let (hd_sid, hd_sd) = hd;
    let (ret, choice) = f(hd);
    (
      ret,
      switch (choice) {
      | [@implicit_arity] Replace_head(_, sd) =>
        t.history = [(hd_sid, sd), ...tl];
        if (Sched.Equal.sched_data_equal(hd_sd, sd)) {
          Did_nothing;
        } else {
          Updated_head(hd_sid);
        };
      | [@implicit_arity] New_head(_, sd) =>
        let new_sid = succ(hd_sid);
        t.history = [(new_sid, sd), hd, ...tl];
        Added_new_head(new_sid);
      | Do_nothing => Did_nothing
      },
    );
  };

let map_head_no_ret = (f: Sched.sched => head_choice, t: t): action_record => {
  let ((), ret) = map_head(x => ((), f(x)), t);
  ret;
};

module Read = {
  let get_head = (t: t): Sched.sched => {
    let (res, _) = map_head(s => (s, Do_nothing), t);
    res;
  };
};

module In_place_head = {
  module Task = {
    module Add = {
      let add_task =
          (
            ~parent_user_id,
            data: Task_.task_data,
            task_inst_data_list: list(Task_.task_inst_data),
            t: t,
          )
          : (Task_.task, list(Task_.task_inst), action_record) => {
        let ((task, task_inst_list), ar) =
          map_head(
            sched => {
              let (task, task_inst_list, sched) =
                Sched.Task.Add.add_task(
                  ~parent_user_id,
                  data,
                  task_inst_data_list,
                  sched,
                );

              ((task, task_inst_list), Replace_head(sched));
            },
            t,
          );

        (task, task_inst_list, ar);
      };
    };

    module Move = {
      let move_task_internal =
          (
            ~move_task_by_id: (Task_.task_id, Sched.sched) => Sched.sched,
            task_inst_id: Task_.task_id,
            t: t,
          )
          : action_record =>
        map_head_no_ret(
          sched => {
            let sched = move_task_by_id(task_inst_id, sched);
            Replace_head(sched);
          },
          t,
        );

      let move_task_to_completed =
          (task_id: Task_.task_id, t: t): action_record =>
        move_task_internal(
          ~move_task_by_id=Sched.Task.Move.move_task_to_completed,
          task_id,
          t,
        );

      let move_task_to_uncompleted =
          (task_id: Task_.task_id, t: t): action_record =>
        move_task_internal(
          ~move_task_by_id=Sched.Task.Move.move_task_to_uncompleted,
          task_id,
          t,
        );

      let move_task_to_discarded =
          (task_id: Task_.task_id, t: t): action_record =>
        move_task_internal(
          ~move_task_by_id=Sched.Task.Move.move_task_to_discarded,
          task_id,
          t,
        );
    };
  };

  module Task_inst = {
    module Add = {
      let add_task_inst =
          (~parent_task_id, data: Task_.task_inst_data, t: t)
          : (Task_.task_inst, action_record) =>
        map_head(
          sched => {
            let (task_inst, sched) =
              Sched.Task_inst.Add.add_task_inst(~parent_task_id, data, sched);

            (task_inst, Replace_head(sched));
          },
          t,
        );
    };

    module Move = {
      let move_task_inst_internal =
          (
            ~move_task_inst_by_id:
               (Task_.task_inst_id, Sched.sched) => Sched.sched,
            task_inst_id: Task_.task_inst_id,
            t: t,
          )
          : action_record =>
        map_head_no_ret(
          sched => {
            let sched = move_task_inst_by_id(task_inst_id, sched);
            Replace_head(sched);
          },
          t,
        );

      let move_task_inst_to_completed =
          (task_inst_id: Task_.task_inst_id, t: t): action_record =>
        move_task_inst_internal(
          ~move_task_inst_by_id=Sched.Task_inst.Move.move_task_inst_to_completed,
          task_inst_id,
          t,
        );

      let move_task_inst_to_uncompleted =
          (task_inst_id: Task_.task_inst_id, t: t): action_record =>
        move_task_inst_internal(
          ~move_task_inst_by_id=Sched.Task_inst.Move.move_task_inst_to_uncompleted,
          task_inst_id,
          t,
        );

      let move_task_inst_to_discarded =
          (task_inst_id: Task_.task_inst_id, t: t): action_record =>
        move_task_inst_internal(
          ~move_task_inst_by_id=Sched.Task_inst.Move.move_task_inst_to_discarded,
          task_inst_id,
          t,
        );
    };
  };

  module Task_seg = {
    module Move = {
      let move_task_seg_internal =
          (
            ~move_task_seg_by_id:
               (Task_.task_seg_id, Sched.sched) => Sched.sched,
            task_seg_id: Task_.task_seg_id,
            t: t,
          )
          : action_record =>
        map_head_no_ret(
          sched => {
            let sched = move_task_seg_by_id(task_seg_id, sched);
            Replace_head(sched);
          },
          t,
        );

      let move_task_seg_to_completed =
          (task_seg_id: Task_.task_seg_id, t: t): action_record =>
        move_task_seg_internal(
          ~move_task_seg_by_id=Sched.Task_seg.Move.move_task_seg_to_completed,
          task_seg_id,
          t,
        );

      let move_task_seg_to_uncompleted =
          (task_seg_id: Task_.task_seg_id, t: t): action_record =>
        move_task_seg_internal(
          ~move_task_seg_by_id=Sched.Task_seg.Move.move_task_seg_to_uncompleted,
          task_seg_id,
          t,
        );

      let move_task_seg_to_discarded =
          (task_seg_id: Task_.task_seg_id, t: t): action_record =>
        move_task_seg_internal(
          ~move_task_seg_by_id=Sched.Task_seg.Move.move_task_seg_to_discarded,
          task_seg_id,
          t,
        );
    };
  };

  module Sched_req = {
    module Add = {
      let add_sched_req =
          (data: Sched_req.sched_req_data, t: t)
          : (result(Sched_req.sched_req, unit), action_record) =>
        map_head(
          sched =>
            switch (Sched.Sched_req.Add.add_sched_req_data(data, sched)) {
            | [@implicit_arity] Ok(sched_req, sched) => (
                Ok(sched_req),
                Replace_head(sched),
              )
            | Error () => (Error(), Do_nothing)
            },
          t,
        );
    };
  };

  module Recur = {
    let instantiate = (~start, ~end_exc, t: t): action_record =>
      map_head_no_ret(
        sched => {
          let sched = Sched.Recur.instantiate(~start, ~end_exc, sched);
          Replace_head(sched);
        },
        t,
      );
  };

  module Progress = {
    module Add = {
      let add_task_seg_progress_chunk =
          (task_seg_id: Task_.task_seg_id, chunk: (int64, int64), t: t)
          : action_record =>
        map_head_no_ret(
          sched => {
            let sched =
              Sched.Progress.Add.add_task_seg_progress_chunk(
                task_seg_id,
                chunk,
                sched,
              );

            Replace_head(sched);
          },
          t,
        );

      let add_task_inst_progress_chunk =
          (task_inst_id: Task_.task_inst_id, chunk: (int64, int64), t: t)
          : action_record =>
        map_head_no_ret(
          sched => {
            let sched =
              Sched.Progress.Add.add_task_inst_progress_chunk(
                task_inst_id,
                chunk,
                sched,
              );

            Replace_head(sched);
          },
          t,
        );
    };
  };
};

module Maybe_append_to_head = {
  let remove_task = (task_id: Task_.task_id, t: t): action_record =>
    map_head_no_ret(
      hd => {
        let task_seg_place_seq =
          Sched.Agenda.Find.find_task_seg_place_seq_by_task_id(task_id, hd);

        let no_task_seg_places_recorded = OSeq.is_empty(task_seg_place_seq);
        let no_task_inst_progress_recorded =
          OSeq.is_empty(
            Sched.Progress.Find.find_task_inst_progress_chunk_seq_by_task_id(
              task_id,
              hd,
            ),
          );

        let no_task_seg_progress_recorded =
          OSeq.is_empty(
            Sched.Progress.Find.find_task_seg_progress_chunk_seq_by_task_id(
              task_id,
              hd,
            ),
          );

        let hd' =
          hd
          |> Sched.Task.Remove.remove_task_all(task_id)
          |> Sched.Sched_req.Remove.Pending.remove_pending_sched_req_by_task_id(
               task_id,
             )
          |> Sched.Sched_req.Remove.Record.remove_sched_req_record_by_task_id(
               task_id,
             );

        if (no_task_seg_places_recorded
            && no_task_inst_progress_recorded
            && no_task_seg_progress_recorded) {
          Replace_head(hd');
        } else {
          let hd' =
            hd'
            |> Sched.Sched_req.Remove.Record.remove_sched_req_record_by_task_id(
                 task_id,
               )
            |> Sched.Agenda.Remove.remove_task_seg_place_seq(
                 task_seg_place_seq,
               );

          New_head(hd');
        };
      },
      t,
    );

  let remove_task_inst =
      (task_inst_id: Task_.task_inst_id, t: t): action_record =>
    map_head_no_ret(
      hd => {
        let task_seg_place_seq =
          Sched.Agenda.Find.find_task_seg_place_seq_by_task_inst_id(
            task_inst_id,
            hd,
          );

        let no_task_seg_places_recorded = OSeq.is_empty(task_seg_place_seq);
        let no_task_inst_progress_recorded =
          OSeq.is_empty(
            Sched.Progress.Find.find_task_inst_progress_chunk_seq(
              task_inst_id,
              hd,
            ),
          );

        let no_task_seg_progress_recorded =
          OSeq.is_empty(
            Sched.Progress.Find.find_task_seg_progress_chunk_seq_by_task_inst_id(
              task_inst_id,
              hd,
            ),
          );

        let hd' =
          hd
          |> Sched.Task_inst.Remove.remove_task_inst_all(task_inst_id)
          |> Sched.Sched_req.Remove.Pending.remove_pending_sched_req_by_task_inst_id(
               task_inst_id,
             )
          |> Sched.Sched_req.Remove.Record.remove_sched_req_record_by_task_inst_id(
               task_inst_id,
             );

        if (no_task_seg_places_recorded
            && no_task_inst_progress_recorded
            && no_task_seg_progress_recorded) {
          Replace_head(hd');
        } else {
          let hd' =
            hd'
            |> Sched.Sched_req.Remove.Record.remove_sched_req_record_by_task_inst_id(
                 task_inst_id,
               )
            |> Sched.Agenda.Remove.remove_task_seg_place_seq(
                 task_seg_place_seq,
               );

          New_head(hd');
        };
      },
      t,
    );

  let remove_task_seg_progress_chunk =
      (task_seg_id: Task_.task_seg_id, chunk: (int64, int64), t: t)
      : action_record =>
    map_head_no_ret(
      sched => {
        let chunks =
          Sched.Progress.Find.find_task_seg_progress_chunk_set(
            task_seg_id,
            sched,
          );

        if (Int64_int64_set.mem(chunk, chunks)) {
          let hd' =
            sched
            |> Sched.Progress.Remove.remove_task_seg_progress_chunk(
                 task_seg_id,
                 chunk,
               );

          Replace_head(hd');
        } else {
          Do_nothing;
        };
      },
      t,
    );

  let remove_task_inst_progress_chunk =
      (task_inst_id: Task_.task_inst_id, chunk: (int64, int64), t: t)
      : action_record =>
    map_head_no_ret(
      sched => {
        let chunks =
          Sched.Progress.Find.find_task_inst_progress_chunk_set(
            task_inst_id,
            sched,
          );

        if (Int64_int64_set.mem(chunk, chunks)) {
          let hd' =
            sched
            |> Sched.Progress.Remove.remove_task_inst_progress_chunk(
                 task_inst_id,
                 chunk,
               );

          Replace_head(hd');
        } else {
          Do_nothing;
        };
      },
      t,
    );

  let remove_pending_sched_req =
      (sched_req_id: Sched_req.sched_req_id, t: t): action_record =>
    map_head_no_ret(
      sched =>
        switch (
          Sched.Sched_req.Find.Pending.find_pending_sched_req(
            sched_req_id,
            sched,
          )
        ) {
        | None => Do_nothing
        | Some(_) =>
          New_head(
            Sched.Sched_req.Remove.Pending.remove_pending_sched_req(
              sched_req_id,
              sched,
            ),
          )
        },
      t,
    );

  let sched =
      (
        ~start,
        ~end_exc,
        ~include_sched_reqs_starting_within_time_slot,
        ~include_sched_reqs_ending_within_time_slot,
        ~up_to_sched_req_id_inc,
        t: t,
      )
      : (result(unit, unit), action_record) =>
    map_head(
      hd => {
        let (sched_req_records, hd') =
          hd
          |> Sched.Sched_req.Allocate_task_segs.allocate_task_segs_for_pending_sched_reqs(
               ~start,
               ~end_exc,
               ~include_sched_reqs_starting_within_time_slot,
               ~include_sched_reqs_ending_within_time_slot,
               ~up_to_sched_req_id_inc,
             );

        switch (sched_req_records) {
        | [] => (Ok(), Do_nothing)
        | _ =>
          let possible_scheds =
            Sched_search.backtracking_search_multi(
              ~start,
              ~end_exc,
              ~base=hd',
              sched_req_records,
            );

          switch (possible_scheds()) {
          | Seq.Nil => (Error(), Do_nothing)
          | [@implicit_arity] Seq.Cons(hd', _) => (Ok(), New_head(hd'))
          };
        };
      },
      t,
    );
};

module Append_to_head = {
  let snapshot = (t: t): action_record =>
    map_head_no_ret(sched => New_head(sched), t);
};

module Equal = {
  let equal = (t1, t2) =>
    List.for_all2(
      (s1, s2) => Sched.Equal.sched_equal(s1, s2),
      t1.history,
      t2.history,
    );
};

module Serialize = {
  let base_and_diffs_of_list =
      (l: list(Sched.sched))
      : option((Sched.sched, list(Sched.sched_diff))) => {
    let rec aux =
            (
              base_and_last_and_diffs:
                option((Sched.sched, Sched.sched, list(Sched.sched_diff))),
              l: list(Sched.sched),
            ) =>
      switch (l) {
      | [] =>
        switch (base_and_last_and_diffs) {
        | None => None
        | Some((base, _, diffs)) => Some((base, List.rev(diffs)))
        }
      | [sched, ...rest] =>
        switch (base_and_last_and_diffs) {
        | None => aux(Some((sched, sched, [])), rest)
        | Some((base, last, diffs)) =>
          let diff = Sched.Diff.diff_sched(~old=last, sched);
          aux(Some((base, sched, [diff, ...diffs])), rest);
        }
      };

    aux(None, List.rev(l));
  };

  let to_base_and_diffs =
      (t: t): option((Sched.sched, list(Sched.sched_diff))) =>
    base_and_diffs_of_list(t.history);

  let write_to_dir = (~dir: string, t: t): result(unit, string) =>
    try(
      if (Sys.is_directory(dir)) {
        switch (to_base_and_diffs(t)) {
        | None => Ok()
        | Some((base, diffs)) =>
          {
            let base_str = Sched.Serialize.json_string_of_sched(base);
            let oc = open_out(Filename.concat(dir, "sched_v0.json"));
            Fun.protect(
              ~finally=() => close_out(oc),
              () => output_string(oc, base_str),
            );
          };
          diffs
          |> List.to_seq
          |> Seq.map(Sched.Serialize.json_string_of_sched_diff)
          |> OSeq.iteri((i, sched_diff_str) =>{
               let oc =
                 open_out(
                   Filename.concat(
                     dir,
                     Printf.sprintf("sched_v%d.json", i + 1),
                   ),
                 );

               Fun.protect(
                 ~finally=() => close_out(oc),
                 () => output_string(oc, sched_diff_str),
               );
             });
          Ok();
        };
      } else {
        Error("File is not a directory");
      }
    ) {
    | Sys_error(msg) => Error(msg)
    | _ => Error("Failed to write to directory")
    };
};

module Deserialize = {
  let list_of_base_and_diffs =
      (base: Sched.sched, diffs: list(Sched.sched_diff)): list(Sched.sched) => {
    let rec aux =
            (
              acc: list(Sched.sched),
              cur: Sched.sched,
              diffs: list(Sched.sched_diff),
            )
            : list(Sched.sched) =>
      switch (diffs) {
      | [] => acc
      | [diff, ...diffs] =>
        let next = Sched.Diff.add_diff_sched(diff, cur);
        aux([next, ...acc], next, diffs);
      };

    aux([base], base, diffs);
  };

  let of_base_and_diffs = (base, diffs): t => {
    let history = list_of_base_and_diffs(base, diffs);
    {history: history};
  };

  let read_from_dir = (~dir: string): result(t, string) =>
    try(
      if (Sys.is_directory(dir)) {
        if (Sys.readdir(dir) == [||]) {
          Ok(make_empty());
        } else {
          let base = {
            let ic = open_in(Filename.concat(dir, "sched_v0.json"));
            Fun.protect(
              ~finally=() => close_in(ic),
              () => really_input_string(ic, in_channel_length(ic)),
            )
            |> Sched.Deserialize.sched_of_json_string;
          };

          let diffs =
            Sys.readdir(dir)
            |> Array.to_seq
            |> Seq.filter_map(s =>
                 try(
                   Scanf.sscanf(s, "sched_v%d.json", i =>
                     if (i == 0) {
                       None;
                     } else {
                       Some((i, s));
                     }
                   )
                 ) {
                 | Stdlib.Scanf.Scan_failure(_) => None
                 }
               )
            |> Seq.map(((i, s)) =>{
                 let ic = open_in(Filename.concat(dir, s));
                 (
                   i,
                   Fun.protect(
                     ~finally=() => close_in(ic),
                     () => really_input_string(ic, in_channel_length(ic)),
                   ),
                 );
               })
            |> OSeq.sort(~cmp=((i1, _), (i2, _)) => compare(i1, i2))
            |> Seq.map(((_i, s)) =>
                 Sched.Deserialize.sched_diff_of_json_string(s)
               )
            |> List.of_seq;

          Ok(of_base_and_diffs(base, diffs));
        };
      } else {
        Error("Path is not a directory");
      }
    ) {
    | Sys_error(msg) => Error(msg)
    | _ => Error("Failed to read from directory")
    };
};

module To_string = {
  let debug_string_of_sched_ver_history =
      (~indent_level=0, ~buffer=Buffer.create(4096), t: t) => {
    Debug_print.bprintf(~indent_level, buffer, "sched ver history\n");
    List.iter(
      sched =>
        Sched.To_string.debug_string_of_sched(
          ~indent_level=indent_level + 1,
          ~buffer,
          sched,
        )
        |> ignore,
      List.rev(t.history),
    );
    Buffer.contents(buffer);
  };

  let debug_string_of_action_record =
      (~indent_level=0, ~buffer=Buffer.create(4096), ar: action_record) => {
    Debug_print.bprintf(
      ~indent_level,
      buffer,
      "action record: %s",
      switch (ar) {
      | Updated_head(id) => Printf.sprintf("updated head sched #%d", id)
      | Added_new_head(id) => Printf.sprintf("added new head sched #%d", id)
      | Did_nothing => "did nothing"
      },
    );
    Buffer.contents(buffer);
  };
};

module Print = {
  let debug_print_sched_ver_history = (~indent_level=0, t: t) =>
    print_string(
      To_string.debug_string_of_sched_ver_history(~indent_level, t),
    );

  let debug_print_action_record = (~indent_level=0, ar: action_record) =>
    print_endline(
      To_string.debug_string_of_action_record(~indent_level, ar),
    );
};
