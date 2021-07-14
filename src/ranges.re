let normalize =
    (
      type a,
      ~skip_filter_invalid=false,
      ~skip_filter_empty=false,
      ~skip_sort=false,
      ~modulo: option(int64),
      ~to_int64: a => int64,
      ~of_int64: int64 => a,
      s: Seq.t(Range.range(a)),
    )
    : Seq.t(Range.range(a)) =>
  switch (modulo) {
  | None =>
    s
    |> Seq.map(Range.int64_exc_range_of_range(~to_int64))
    |> Time_slots.Normalize.normalize(
         ~skip_filter_invalid,
         ~skip_filter_empty,
         ~skip_sort,
       )
    |> Seq.map(((x, y)) => (of_int64(x), y |> Int64.pred |> of_int64))
    |> Seq.map(((x, y)) => `Range_inc((x, y)))
  | Some(_) =>
    /* not sure what would be a reasonable normalization procedure when domain is a field */
    s
  };

module Check = {
  let seq_is_valid =
      (
        type a,
        ~modulo: option(int64),
        ~to_int64: a => int64,
        s: Seq.t(Range.range(a)),
      )
      : bool =>
    OSeq.for_all(Range.is_valid(~modulo, ~to_int64), s);

  let list_is_valid =
      (
        type a,
        ~modulo: option(int64),
        ~to_int64: a => int64,
        s: list(Range.range(a)),
      )
      : bool =>
    List.for_all(Range.is_valid(~modulo, ~to_int64), s);
};

module Flatten = {
  let flatten =
      (
        type a,
        ~modulo: option(int64),
        ~to_int64: a => int64,
        ~of_int64: int64 => a,
        s: Seq.t(Range.range(a)),
      )
      : Seq.t(a) =>
    Seq.flat_map(
      Range.Flatten.flatten_into_seq(~modulo, ~to_int64, ~of_int64),
      s,
    );

  let flatten_list =
      (
        type a,
        ~modulo: option(int64),
        ~to_int64: a => int64,
        ~of_int64: int64 => a,
        l: list(Range.range(a)),
      )
      : list(a) =>
    l |> List.to_seq |> flatten(~modulo, ~to_int64, ~of_int64) |> List.of_seq;
};

module Of_seq = {
  let range_seq_of_seq =
      (
        type a,
        ~skip_filter_invalid=false,
        ~skip_filter_empty=false,
        ~skip_sort=false,
        ~modulo: option(int64),
        ~to_int64: a => int64,
        ~of_int64: int64 => a,
        s: Seq.t(a),
      )
      : Seq.t(Range.range(a)) =>
    s
    |> Seq.map(x => `Range_inc((x, x)))
    |> normalize(
         ~skip_filter_invalid,
         ~skip_filter_empty,
         ~skip_sort,
         ~modulo,
         ~to_int64,
         ~of_int64,
       );

  let range_list_of_seq =
      (
        type a,
        ~skip_filter_invalid=false,
        ~skip_filter_empty=false,
        ~skip_sort=false,
        ~modulo: option(int64),
        ~to_int64: a => int64,
        ~of_int64: int64 => a,
        s: Seq.t(a),
      )
      : list(Range.range(a)) =>
    range_seq_of_seq(
      ~skip_filter_invalid,
      ~skip_filter_empty,
      ~skip_sort,
      ~modulo,
      ~to_int64,
      ~of_int64,
      s,
    )
    |> List.of_seq;
};

module Of_list = {
  let range_seq_of_list =
      (
        type a,
        ~skip_filter_invalid=false,
        ~skip_filter_empty=false,
        ~skip_sort=false,
        ~modulo: option(int64),
        ~to_int64: a => int64,
        ~of_int64: int64 => a,
        l: list(a),
      )
      : Seq.t(Range.range(a)) =>
    List.to_seq(l)
    |> Of_seq.range_seq_of_seq(
         ~skip_filter_invalid,
         ~skip_filter_empty,
         ~skip_sort,
         ~modulo,
         ~to_int64,
         ~of_int64,
       );

  let range_list_of_list =
      (
        type a,
        ~skip_filter_invalid=false,
        ~skip_filter_empty=false,
        ~skip_sort=false,
        ~modulo: option(int64),
        ~to_int64: a => int64,
        ~of_int64: int64 => a,
        l: list(a),
      )
      : list(Range.range(a)) =>
    List.to_seq(l)
    |> Of_seq.range_seq_of_seq(
         ~skip_filter_invalid,
         ~skip_filter_empty,
         ~skip_sort,
         ~modulo,
         ~to_int64,
         ~of_int64,
       )
    |> List.of_seq;
};

module type S = {
  type t;

  let normalize:
    (
      ~skip_filter_invalid: bool=?,
      ~skip_filter_empty: bool=?,
      ~skip_sort: bool=?,
      Seq.t(Range.range(t))
    ) =>
    Seq.t(Range.range(t));

  module Check: {
    let seq_is_valid: Seq.t(Range.range(t)) => bool;

    let list_is_valid: list(Range.range(t)) => bool;
  };

  module Flatten: {
    let flatten: Seq.t(Range.range(t)) => Seq.t(t);

    let flatten_list: list(Range.range(t)) => list(t);
  };

  module Of_seq: {
    let range_seq_of_seq: Seq.t(t) => Seq.t(Range.range(t));

    let range_list_of_seq: Seq.t(t) => list(Range.range(t));
  };

  module Of_list: {
    let range_seq_of_list: list(t) => Seq.t(Range.range(t));

    let range_list_of_list: list(t) => list(Range.range(t));
  };
};

module Make = (B: Range.B) : (S with type t := B.t) => {
  open B;

  let normalize =
      (
        ~skip_filter_invalid=false,
        ~skip_filter_empty=false,
        ~skip_sort=false,
        s: Seq.t(Range.range(t)),
      ) =>
    normalize(
      ~skip_filter_invalid,
      ~skip_filter_empty,
      ~skip_sort,
      ~modulo,
      ~to_int64,
      ~of_int64,
      s,
    );

  module Check = {
    let seq_is_valid = s => Check.seq_is_valid(~modulo, ~to_int64, s);

    let list_is_valid = l => Check.list_is_valid(~modulo, ~to_int64, l);
  };

  module Flatten = {
    let flatten = (s: Seq.t(Range.range(t))): Seq.t(t) =>
      Flatten.flatten(~modulo, ~to_int64, ~of_int64, s);

    let flatten_list = (l: list(Range.range(t))): list(t) =>
      Flatten.flatten_list(~modulo, ~to_int64, ~of_int64, l);
  };

  module Of_seq = {
    let range_seq_of_seq = (s: Seq.t(t)): Seq.t(Range.range(t)) =>
      Of_seq.range_seq_of_seq(~modulo, ~to_int64, ~of_int64, s);

    let range_list_of_seq = (s: Seq.t(t)): list(Range.range(t)) =>
      Of_seq.range_list_of_seq(~modulo, ~to_int64, ~of_int64, s);
  };

  module Of_list = {
    let range_seq_of_list = (l: list(t)): Seq.t(Range.range(t)) =>
      List.to_seq(l) |> Of_seq.range_seq_of_seq;

    let range_list_of_list = (l: list(t)): list(Range.range(t)) =>
      List.to_seq(l) |> Of_seq.range_seq_of_seq |> List.of_seq;
  };
};
