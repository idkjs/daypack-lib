let int_range_of_range:
  (~to_int: 'a => int, Range.range('a)) => Range.range(int);

let int_exc_range_of_range:
  (~to_int: 'a => int, Range.range('a)) => (int, int);

let inc_range_of_range:
  (~to_int: 'a => int, ~of_int: int => 'a, Range.range('a)) => ('a, 'a);

let exc_range_of_range:
  (~to_int: 'a => int, ~of_int: int => 'a, Range.range('a)) => ('a, 'a);

let join:
  (
    ~to_int: 'a => int,
    ~of_int: int => 'a,
    Range.range('a),
    Range.range('a)
  ) =>
  option(Range.range('a));

module Flatten: {
  let flatten_into_seq:
    (
      ~modulo: option(int),
      ~to_int: 'a => int,
      ~of_int: int => 'a,
      Range.range('a)
    ) =>
    Seq.t('a);

  let flatten_into_list:
    (
      ~modulo: option(int),
      ~to_int: 'a => int,
      ~of_int: int => 'a,
      Range.range('a)
    ) =>
    list('a);
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

module Make: (B: B) => S with type t := B.t;
