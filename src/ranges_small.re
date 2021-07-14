let normalize =
    (
      type a,
      ~skip_filter_invalid=false,
      ~skip_filter_empty=false,
      ~skip_sort=false,
      ~modulo: option(int),
      ~to_int: a => int,
      ~of_int: int => a,
      s: Seq.t(Range.range(a)),
    )
    : Seq.t(Range.range(a)) => {
  let modulo = Option.map(Int64.of_int, modulo);
  let to_int64 = Misc_utils.convert_to_int_to_int64(to_int);
  let of_int64 = Misc_utils.convert_of_int_to_int64(of_int);
  Ranges.normalize(
    ~skip_filter_invalid,
    ~skip_filter_empty,
    ~skip_sort,
    ~modulo,
    ~to_int64,
    ~of_int64,
    s,
  );
};

module Check = {
  let seq_is_valid =
      (
        type a,
        ~modulo: option(int),
        ~to_int: a => int,
        s: Seq.t(Range.range(a)),
      )
      : bool => {
    let modulo = Option.map(Int64.of_int, modulo);
    let to_int64 = Misc_utils.convert_to_int_to_int64(to_int);
    Ranges.Check.seq_is_valid(~modulo, ~to_int64, s);
  };

  let list_is_valid =
      (
        type a,
        ~modulo: option(int),
        ~to_int: a => int,
        l: list(Range.range(a)),
      )
      : bool => {
    let modulo = Option.map(Int64.of_int, modulo);
    let to_int64 = Misc_utils.convert_to_int_to_int64(to_int);
    Ranges.Check.list_is_valid(~modulo, ~to_int64, l);
  };
};

module Flatten = {
  let flatten =
      (
        type a,
        ~modulo: option(int),
        ~to_int: a => int,
        ~of_int: int => a,
        s: Seq.t(Range.range(a)),
      )
      : Seq.t(a) => {
    let modulo = Option.map(Int64.of_int, modulo);
    let to_int64 = Misc_utils.convert_to_int_to_int64(to_int);
    let of_int64 = Misc_utils.convert_of_int_to_int64(of_int);
    Ranges.Flatten.flatten(~modulo, ~to_int64, ~of_int64, s);
  };

  let flatten_list =
      (
        type a,
        ~modulo: option(int),
        ~to_int: a => int,
        ~of_int: int => a,
        l: list(Range.range(a)),
      )
      : list(a) =>
    l |> List.to_seq |> flatten(~modulo, ~to_int, ~of_int) |> List.of_seq;
};

module Of_seq = {
  let range_seq_of_seq =
      (
        type a,
        ~skip_filter_invalid=false,
        ~skip_filter_empty=false,
        ~skip_sort=false,
        ~modulo: option(int),
        ~to_int: a => int,
        ~of_int: int => a,
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
         ~to_int,
         ~of_int,
       );

  let range_list_of_seq =
      (
        type a,
        ~skip_filter_invalid=false,
        ~skip_filter_empty=false,
        ~skip_sort=false,
        ~modulo: option(int),
        ~to_int: a => int,
        ~of_int: int => a,
        s: Seq.t(a),
      )
      : list(Range.range(a)) =>
    range_seq_of_seq(
      ~skip_filter_invalid,
      ~skip_filter_empty,
      ~skip_sort,
      ~modulo,
      ~to_int,
      ~of_int,
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
        ~modulo: option(int),
        ~to_int: a => int,
        ~of_int: int => a,
        l: list(a),
      )
      : Seq.t(Range.range(a)) =>
    List.to_seq(l)
    |> Of_seq.range_seq_of_seq(
         ~skip_filter_invalid,
         ~skip_filter_empty,
         ~skip_sort,
         ~modulo,
         ~to_int,
         ~of_int,
       );

  let range_list_of_list =
      (
        type a,
        ~skip_filter_invalid=false,
        ~skip_filter_empty=false,
        ~skip_sort=false,
        ~modulo: option(int),
        ~to_int: a => int,
        ~of_int: int => a,
        l: list(a),
      )
      : list(Range.range(a)) =>
    List.to_seq(l)
    |> Of_seq.range_seq_of_seq(
         ~skip_filter_invalid,
         ~skip_filter_empty,
         ~skip_sort,
         ~modulo,
         ~to_int,
         ~of_int,
       )
    |> List.of_seq;
};

module Make = (B: Range_small.B) : (Ranges.S with type t := B.t) => {
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
      ~to_int,
      ~of_int,
      s,
    );

  module Check = {
    let seq_is_valid = s => Check.seq_is_valid(~modulo, ~to_int, s);

    let list_is_valid = l => Check.list_is_valid(~modulo, ~to_int, l);
  };

  module Flatten = {
    let flatten = (s: Seq.t(Range.range(t))): Seq.t(t) =>
      Flatten.flatten(~modulo, ~to_int, ~of_int, s);

    let flatten_list = (l: list(Range.range(t))): list(t) =>
      Flatten.flatten_list(~modulo, ~to_int, ~of_int, l);
  };

  module Of_seq = {
    let range_seq_of_seq = (s: Seq.t(t)): Seq.t(Range.range(t)) =>
      Of_seq.range_seq_of_seq(~modulo, ~to_int, ~of_int, s);

    let range_list_of_seq = (s: Seq.t(t)): list(Range.range(t)) =>
      Of_seq.range_list_of_seq(~modulo, ~to_int, ~of_int, s);
  };

  module Of_list = {
    let range_seq_of_list = (l: list(t)): Seq.t(Range.range(t)) =>
      List.to_seq(l) |> Of_seq.range_seq_of_seq;

    let range_list_of_list = (l: list(t)): list(Range.range(t)) =>
      List.to_seq(l) |> Of_seq.range_seq_of_seq |> List.of_seq;
  };
};
