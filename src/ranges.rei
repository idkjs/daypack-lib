let normalize:
  (
    ~skip_filter_invalid: bool=?,
    ~skip_filter_empty: bool=?,
    ~skip_sort: bool=?,
    ~modulo: option(int64),
    ~to_int64: 'a => int64,
    ~of_int64: int64 => 'a,
    Seq.t(Range.range('a))
  ) =>
  Seq.t(Range.range('a));

module Check: {
  let seq_is_valid:
    (
      ~modulo: option(int64),
      ~to_int64: 'a => int64,
      Seq.t(Range.range('a))
    ) =>
    bool;

  let list_is_valid:
    (
      ~modulo: option(int64),
      ~to_int64: 'a => int64,
      list(Range.range('a))
    ) =>
    bool;
};

module Flatten: {
  let flatten:
    (
      ~modulo: option(int64),
      ~to_int64: 'a => int64,
      ~of_int64: int64 => 'a,
      Seq.t(Range.range('a))
    ) =>
    Seq.t('a);

  let flatten_list:
    (
      ~modulo: option(int64),
      ~to_int64: 'a => int64,
      ~of_int64: int64 => 'a,
      list(Range.range('a))
    ) =>
    list('a);
};

module Of_seq: {
  let range_seq_of_seq:
    (
      ~skip_filter_invalid: bool=?,
      ~skip_filter_empty: bool=?,
      ~skip_sort: bool=?,
      ~modulo: option(int64),
      ~to_int64: 'a => int64,
      ~of_int64: int64 => 'a,
      Seq.t('a)
    ) =>
    Seq.t(Range.range('a));

  let range_list_of_seq:
    (
      ~skip_filter_invalid: bool=?,
      ~skip_filter_empty: bool=?,
      ~skip_sort: bool=?,
      ~modulo: option(int64),
      ~to_int64: 'a => int64,
      ~of_int64: int64 => 'a,
      Seq.t('a)
    ) =>
    list(Range.range('a));
};

module Of_list: {
  let range_seq_of_list:
    (
      ~skip_filter_invalid: bool=?,
      ~skip_filter_empty: bool=?,
      ~skip_sort: bool=?,
      ~modulo: option(int64),
      ~to_int64: 'a => int64,
      ~of_int64: int64 => 'a,
      list('a)
    ) =>
    Seq.t(Range.range('a));

  let range_list_of_list:
    (
      ~skip_filter_invalid: bool=?,
      ~skip_filter_empty: bool=?,
      ~skip_sort: bool=?,
      ~modulo: option(int64),
      ~to_int64: 'a => int64,
      ~of_int64: int64 => 'a,
      list('a)
    ) =>
    list(Range.range('a));
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

module Make: (B: Range.B) => S with type t := B.t;
