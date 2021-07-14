type t = {mutable profiles: String_map.t(Time_profile.data)};

let make_empty = (): t => {profiles: String_map.empty};

let of_profile_list = profiles => {
  profiles: profiles |> List.to_seq |> String_map.of_seq,
};

let matching_time_slots_of_profile = {
  let cache: Hashtbl.t(string, list(Time_slot.t)) = (
    Hashtbl.create(20): Hashtbl.t(string, list(Time_slot.t))
  );
  (~start, ~end_exc, ~profile: string, t: t) => (
    switch (Hashtbl.find_opt(cache, profile)) {
    | None =>
      String_map.find_opt(profile, t.profiles)
      |> Option.map(data =>{
           let time_slots =
             Time_profile.matching_time_slots_of_data(~start, ~end_exc, data)
             |> List.of_seq;

           Hashtbl.add(cache, profile, time_slots);
           time_slots;
         })
    | Some(time_slots) => Some(time_slots)
    }:
      option(list(Time_slot.t))
  );
};

let add_profile = (~profile: string, data: Time_profile.data, t: t): unit =>
  t.profiles = String_map.add(profile, data, t.profiles);

module Serialize = {
  let pack_store = (t: t): Time_profile_store_t.t =>
    t.profiles
    |> String_map.to_seq
    |> Seq.map(Time_profile.Serialize.pack_profile)
    |> List.of_seq;

  let write_to_dir = (~dir: string, t: t): result(unit, string) =>
    try(
      if (Sys.is_directory(dir)) {
        t.profiles
        |> String_map.to_seq
        |> Seq.map(((name, data)) =>
             (name, Time_profile.Serialize.json_string_of_data(data))
           )
        |> Seq.iter(((name, data)) =>{
             let path = Filename.concat(dir, name ++ ".json");
             let oc = open_out(path);
             Fun.protect(
               ~finally=() => close_out(oc),
               () => output_string(oc, data),
             );
           });
        Ok();
      } else {
        Error("File is not a directory");
      }
    ) {
    | Sys_error(msg) => Error(msg)
    };
};

module Deserialize = {
  let unpack_store = (t: Time_profile_store_t.t): t => {
    let profiles =
      t
      |> List.to_seq
      |> Seq.map(Time_profile.Deserialize.unpack_profile)
      |> String_map.of_seq;

    {profiles: profiles};
  };

  let read_from_dir = (~dir: string): result(t, string) =>
    try({
      let profiles =
        Sys.readdir(dir)
        |> Array.to_seq
        |> Seq.filter_map(s =>
             Filename.chop_suffix_opt(~suffix=".json", s)
             |> Option.map(name => (name, Filename.concat(dir, s)))
           )
        |> Seq.map(((name, path)) =>{
             let ic = open_in(path);
             Fun.protect(
               ~finally=() => close_in(ic),
               () => {
                 let s = really_input_string(ic, in_channel_length(ic));
                 (name, s);
               },
             );
           })
        |> Seq.map(((name, s)) =>
             (name, Time_profile.Deserialize.data_of_json_string(s))
           )
        |> String_map.of_seq;

      Ok({profiles: profiles});
    }) {
    | Sys_error(msg) => Error(msg)
    };
};

module Equal = {
  let equal = (t1: t, t2: t): bool =>
    String_map.equal(Time_profile.Equal.data_equal, t1.profiles, t2.profiles);
};

module To_string = {
  let debug_string_of_time_profile_store =
      (~indent_level=0, ~buffer=Buffer.create(4096), t: t): string => {
    open Time_profile;
    Debug_print.bprintf(~indent_level, buffer, "time profile store\n");
    t.profiles
    |> String_map.to_seq
    |> Seq.iter(((name, data)) => {
         Debug_print.bprintf(
           ~indent_level=indent_level + 1,
           buffer,
           "profile : %s\n",
           name,
         );
         Debug_print.bprintf(
           ~indent_level=indent_level + 1,
           buffer,
           "periods :\n",
         );
         List.iter(
           ((start, end_exc)) => {
             Debug_print.bprintf(
               ~indent_level=indent_level + 2,
               buffer,
               "start\n",
             );
             Time_pattern.To_string.debug_string_of_time_pattern(
               ~indent_level=indent_level + 3,
               ~buffer,
               start,
             )
             |> ignore;
             Debug_print.bprintf(
               ~indent_level=indent_level + 2,
               buffer,
               "end\n",
             );
             Time_pattern.To_string.debug_string_of_time_pattern(
               ~indent_level=indent_level + 3,
               ~buffer,
               end_exc,
             )
             |> ignore;
           },
           data.periods,
         );
       });
    Buffer.contents(buffer);
  };
};

module Print = {
  let debug_print_time_profile_store = (~indent_level=0, t: t) =>
    print_string(
      To_string.debug_string_of_time_profile_store(~indent_level, t),
    );
};
