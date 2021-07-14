include Set.Make({
  type t = (int64, int64);

  let compare = compare;
});

module Serialize = {
  let pack = (set: t): list(((int32, int32), (int32, int32))) =>
    set
    |> to_seq
    |> Seq.map(((x, y)) =>
         (
           Misc_utils.int32_int32_of_int64(x),
           Misc_utils.int32_int32_of_int64(y),
         )
       )
    |> List.of_seq;
};

module Deserialize = {
  let unpack = (l: list(((int32, int32), (int32, int32)))): t =>
    l
    |> List.to_seq
    |> Seq.map(((x, y)) =>
         (
           Misc_utils.int64_of_int32_int32(x),
           Misc_utils.int64_of_int32_int32(y),
         )
       )
    |> of_seq;
};
