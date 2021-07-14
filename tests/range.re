module Int64_range =
  Daypack_lib.Range.Make({
    type t = int64;

    let modulo = None;

    let to_int64 = x => x;

    let of_int64 = x => x;
  });

module Int_range =
  Daypack_lib.Range_small.Make({
    type t = int;

    let modulo = None;

    let to_int = x => x;

    let of_int = x => x;
  });

module Alco = {
  let int64_range_flatten1 = () =>
    Alcotest.(check(list(int64)))(
      "same list",
      [1L, 2L, 3L, 4L, 5L, 6L, 7L],
      Int64_range.Flatten.flatten_into_list(`Range_exc((1L, 8L))),
    );

  let int64_range_flatten2 = () =>
    Alcotest.(check(list(int64)))(
      "same list",
      [1L, 2L, 3L, 4L, 5L, 6L, 7L],
      Int64_range.Flatten.flatten_into_list(`Range_inc((1L, 7L))),
    );

  let suite = [
    Alcotest.test_case("int64_range_flatten1", `Quick, int64_range_flatten1),
    Alcotest.test_case("int64_range_flatten2", `Quick, int64_range_flatten2),
  ];
};
