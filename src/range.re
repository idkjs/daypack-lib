exception Modulo_is_invalid;

exception Range_is_invalid;

type range('a) = [ | `Range_inc('a, 'a) | `Range_exc('a, 'a)];

let map =
    (
      ~f_inc: (('a, 'a)) => ('b, 'b),
      ~f_exc: (('a, 'a)) => ('b, 'b),
      t: range('a),
    )
    : range('b) =>
  switch (t) {
  | `Range_inc(x, y) =>
    let (x, y) = f_inc((x, y));
    `Range_inc((x, y));
  | `Range_exc(x, y) =>
    let (x, y) = f_exc((x, y));
    `Range_exc((x, y));
  };

let int64_range_of_range =
    (type a, ~to_int64: a => int64, x: range(a)): range(int64) => {
  let f = ((x, y)) => (to_int64(x), to_int64(y));
  map(~f_inc=f, ~f_exc=f, x);
};

let int64_inc_range_of_range =
    (type a, ~to_int64: a => int64, x: range(a)): (int64, int64) =>
  switch (x) {
  | `Range_inc(x, y) => (to_int64(x), to_int64(y))
  | `Range_exc(x, y) => (to_int64(x), y |> to_int64 |> Int64.pred)
  };

let int64_exc_range_of_range =
    (type a, ~to_int64: a => int64, x: range(a)): (int64, int64) =>
  switch (x) {
  | `Range_inc(x, y) => (to_int64(x), y |> to_int64 |> Int64.succ)
  | `Range_exc(x, y) => (to_int64(x), to_int64(y))
  };

let inc_range_of_range =
    (type a, ~to_int64: a => int64, ~of_int64: int64 => a, x: range(a))
    : (a, a) =>
  switch (x) {
  | `Range_inc(x, y) => (x, y)
  | `Range_exc(x, y) => (x, y |> to_int64 |> Int64.pred |> of_int64)
  };

let exc_range_of_range =
    (type a, ~to_int64: a => int64, ~of_int64: int64 => a, x: range(a))
    : (a, a) =>
  switch (x) {
  | `Range_inc(x, y) => (x, y |> to_int64 |> Int64.succ |> of_int64)
  | `Range_exc(x, y) => (x, y)
  };

let join =
    (
      type a,
      ~to_int64: a => int64,
      ~of_int64: int64 => a,
      x: range(a),
      y: range(a),
    )
    : option(range(a)) => {
  let x = int64_exc_range_of_range(~to_int64, x);
  let y = int64_exc_range_of_range(~to_int64, y);
  Time_slot.join(x, y)
  |> Option.map(((x, y)) => `Range_exc((of_int64(x), of_int64(y))));
};

let is_valid =
    (type a, ~modulo: option(int64), ~to_int64: a => int64, t: range(a))
    : bool =>
  switch (modulo) {
  | None =>
    let (x, y) = int64_exc_range_of_range(~to_int64, t);
    x <= y;
  | Some(_) => true
  };

module Flatten = {
  let flatten_into_seq =
      (
        type a,
        ~modulo: option(int64),
        ~to_int64: a => int64,
        ~of_int64: int64 => a,
        t: range(a),
      )
      : Seq.t(a) =>
    switch (t) {
    | `Range_inc(start, end_inc) =>
      let start = to_int64(start);
      let end_inc = to_int64(end_inc);
      if (start <= end_inc) {
        Seq_utils.a_to_b_inc_int64(~a=start, ~b=end_inc) |> Seq.map(of_int64);
      } else {
        switch (modulo) {
        | None => raise(Range_is_invalid)
        | Some(modulo) =>
          if (modulo <= 0L) {
            raise(Modulo_is_invalid);
          } else {
            OSeq.append(
              Seq_utils.a_to_b_exc_int64(~a=start, ~b=modulo),
              Seq_utils.a_to_b_inc_int64(~a=0L, ~b=end_inc),
            )
            |> Seq.map(of_int64);
          }
        };
      };
    | `Range_exc(start, end_exc) =>
      let start = to_int64(start);
      let end_exc = to_int64(end_exc);
      if (start <= end_exc) {
        Seq_utils.a_to_b_exc_int64(~a=start, ~b=end_exc) |> Seq.map(of_int64);
      } else {
        switch (modulo) {
        | None => raise(Range_is_invalid)
        | Some(modulo) =>
          if (modulo <= 0L) {
            raise(Modulo_is_invalid);
          } else {
            OSeq.append(
              Seq_utils.a_to_b_exc_int64(~a=start, ~b=modulo),
              Seq_utils.a_to_b_exc_int64(~a=0L, ~b=end_exc),
            )
            |> Seq.map(of_int64);
          }
        };
      };
    };

  let flatten_into_list =
      (
        type a,
        ~modulo: option(int64),
        ~to_int64: a => int64,
        ~of_int64: int64 => a,
        t: range(a),
      )
      : list(a) =>
    flatten_into_seq(~modulo, ~to_int64, ~of_int64, t) |> List.of_seq;
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

module Make = (B: B) : (S with type t := B.t) => {
  open B;

  let int64_range_of_range = (x: range(t)): range(int64) =>
    int64_range_of_range(~to_int64, x);

  let int64_inc_range_of_range = (x: range(t)): (int64, int64) =>
    int64_inc_range_of_range(~to_int64, x);

  let int64_exc_range_of_range = (x: range(t)): (int64, int64) =>
    int64_exc_range_of_range(~to_int64, x);

  let inc_range_of_range = (x: range(t)): (t, t) =>
    inc_range_of_range(~to_int64, ~of_int64, x);

  let exc_range_of_range = (x: range(t)): (t, t) =>
    exc_range_of_range(~to_int64, ~of_int64, x);

  let join = (x: range(t), y: range(t)): option(range(t)) =>
    join(~to_int64, ~of_int64, x, y);

  let is_valid = (x: range(t)): bool => is_valid(~modulo, ~to_int64, x);

  module Flatten = {
    let flatten_into_seq = (t: range(t)): Seq.t(t) =>
      Flatten.flatten_into_seq(~modulo, ~to_int64, ~of_int64, t);

    let flatten_into_list = (t: range(t)): list(t) =>
      Flatten.flatten_into_seq(~modulo, ~to_int64, ~of_int64, t)
      |> List.of_seq;
  };
};
