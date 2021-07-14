include Set.Make({
  type t = (int64, option(int64));

  let compare = compare;
});

module Serialize = {
  let pack = (set: t): list(((int32, int32), option((int32, int32)))) =>
    set
    |> to_seq
    |> Seq.map(((x, y)) =>
         (
           Misc_utils.int32_int32_of_int64(x),
           Option.map(Misc_utils.int32_int32_of_int64, y),
         )
       )
    |> List.of_seq;
};

module Deserialize = {
  let unpack = (l: list(((int32, int32), option((int32, int32))))): t =>
    l
    |> List.to_seq
    |> Seq.map(((x, y)) =>
         (
           Misc_utils.int64_of_int32_int32(x),
           Option.map(Misc_utils.int64_of_int32_int32, y),
         )
       )
    |> of_seq;
};
