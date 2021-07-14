exception Modulo_is_invalid;

exception Range_is_invalid;

type range('a) = [ | `Range_inc('a, 'a) | `Range_exc('a, 'a)];

let map:
  (
    ~f_inc: (('a, 'a)) => ('b, 'b),
    ~f_exc: (('a, 'a)) => ('b, 'b),
    range('a)
  ) =>
  range('b);

let int64_range_of_range:
  (~to_int64: 'a => int64, range('a)) => range(int64);

let int64_inc_range_of_range:
  (~to_int64: 'a => int64, range('a)) => (int64, int64);

let int64_exc_range_of_range:
  (~to_int64: 'a => int64, range('a)) => (int64, int64);

let inc_range_of_range:
  (~to_int64: 'a => int64, ~of_int64: int64 => 'a, range('a)) => ('a, 'a);

let exc_range_of_range:
  (~to_int64: 'a => int64, ~of_int64: int64 => 'a, range('a)) => ('a, 'a);

let join:
  (~to_int64: 'a => int64, ~of_int64: int64 => 'a, range('a), range('a)) =>
  option(range('a));

let is_valid:
  (~modulo: option(int64), ~to_int64: 'a => int64, range('a)) => bool;

module Flatten: {
  let flatten_into_seq:
    (
      ~modulo: option(int64),
      ~to_int64: 'a => int64,
      ~of_int64: int64 => 'a,
      range('a)
    ) =>
    Seq.t('a);

  let flatten_into_list:
    (
      ~modulo: option(int64),
      ~to_int64: 'a => int64,
      ~of_int64: int64 => 'a,
      range('a)
    ) =>
    list('a);
};

module type B = {
  type t;

  let modulo: option(int64);

  let to_int64: t => int64;

  let of_int64: int64 => t;
};

module type S = {
  type t;

  let int64_range_of_range: range(t) => range(int64);

  let int64_inc_range_of_range: range(t) => (int64, int64);

  let int64_exc_range_of_range: range(t) => (int64, int64);

  let inc_range_of_range: range(t) => (t, t);

  let exc_range_of_range: range(t) => (t, t);

  let join: (range(t), range(t)) => option(range(t));

  let is_valid: range(t) => bool;

  module Flatten: {
    let flatten_into_seq: range(t) => Seq.t(t);

    let flatten_into_list: range(t) => list(t);
  };
};

module Make: (B: B) => S with type t := B.t;
