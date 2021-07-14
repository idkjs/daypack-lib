include Set.Make(Int64);

module Serialize = {
  let pack = (set: t): list((int32, int32)) =>
    set |> to_seq |> Seq.map(Misc_utils.int32_int32_of_int64) |> List.of_seq;
};

module Deserialize = {
  let unpack = (l: list((int32, int32))): t =>
    l |> List.to_seq |> Seq.map(Misc_utils.int64_of_int32_int32) |> of_seq;
};
