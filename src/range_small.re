let int_range_of_range =
    (type a, ~to_int: a => int, x: Range.range(a)): Range.range(int) => {
  let f = ((x, y)) => (to_int(x), to_int(y));
  Range.map(~f_inc=f, ~f_exc=f, x);
};

let int_exc_range_of_range =
    (type a, ~to_int: a => int, x: Range.range(a)): (int, int) =>
  switch (x) {
  | `Range_inc(x, y) => (to_int(x), y |> to_int |> Int.succ)
  | `Range_exc(x, y) => (to_int(x), to_int(y))
  };

let inc_range_of_range =
    (type a, ~to_int: a => int, ~of_int: int => a, x: Range.range(a))
    : (a, a) =>
  switch (x) {
  | `Range_inc(x, y) => (x, y)
  | `Range_exc(x, y) => (x, y |> to_int |> Int.pred |> of_int)
  };

let exc_range_of_range =
    (type a, ~to_int: a => int, ~of_int: int => a, x: Range.range(a))
    : (a, a) =>
  switch (x) {
  | `Range_inc(x, y) => (x, y |> to_int |> Int.succ |> of_int)
  | `Range_exc(x, y) => (x, y)
  };

let join =
    (
      type a,
      ~to_int: a => int,
      ~of_int: int => a,
      x: Range.range(a),
      y: Range.range(a),
    )
    : option(Range.range(a)) => {
  let to_int64 = Misc_utils.convert_to_int_to_int64(to_int);
  let of_int64 = Misc_utils.convert_of_int_to_int64(of_int);
  Range.join(~to_int64, ~of_int64, x, y);
};

module Flatten = {
  let flatten_into_seq =
      (
        type a,
        ~modulo: option(int),
        ~to_int: a => int,
        ~of_int: int => a,
        t: Range.range(a),
      )
      : Seq.t(a) => {
    let modulo = Option.map(Int64.of_int, modulo);
    let to_int64 = Misc_utils.convert_to_int_to_int64(to_int);
    let of_int64 = Misc_utils.convert_of_int_to_int64(of_int);
    Range.Flatten.flatten_into_seq(~modulo, ~to_int64, ~of_int64, t);
  };

  let flatten_into_list =
      (
        type a,
        ~modulo: option(int),
        ~to_int: a => int,
        ~of_int: int => a,
        t: Range.range(a),
      )
      : list(a) =>
    flatten_into_seq(~modulo, ~to_int, ~of_int, t) |> List.of_seq;
};

module type B = {
  type t;

  let modulo: option(int);

  let to_int: t => int;

  let of_int: int => t;
};

module type S = {
  type t;

  let int_range_of_range: Range.range(t) => Range.range(int);

  let int_exc_range_of_range: Range.range(t) => (int, int);

  let inc_range_of_range: Range.range(t) => (t, t);

  let exc_range_of_range: Range.range(t) => (t, t);

  let join: (Range.range(t), Range.range(t)) => option(Range.range(t));

  module Flatten: {
    let flatten_into_seq: Range.range(t) => Seq.t(t);

    let flatten_into_list: Range.range(t) => list(t);
  };
};

module Make = (B: B) : (S with type t := B.t) => {
  open B;

  let int_range_of_range = (x: Range.range(t)): Range.range(int) =>
    int_range_of_range(~to_int, x);

  let int_exc_range_of_range = (x: Range.range(t)): (int, int) =>
    int_exc_range_of_range(~to_int, x);

  let inc_range_of_range = (x: Range.range(t)): (t, t) =>
    inc_range_of_range(~to_int, ~of_int, x);

  let exc_range_of_range = (x: Range.range(t)): (t, t) =>
    exc_range_of_range(~to_int, ~of_int, x);

  let join =
      (x: Range.range(t), y: Range.range(t)): option(Range.range(t)) =>
    join(~to_int, ~of_int, x, y);

  module Flatten = {
    let flatten_into_seq = (t: Range.range(t)): Seq.t(t) =>
      Flatten.flatten_into_seq(~modulo, ~to_int, ~of_int, t);

    let flatten_into_list = (t: Range.range(t)): list(t) =>
      Flatten.flatten_into_seq(~modulo, ~to_int, ~of_int, t) |> List.of_seq;
  };
};
