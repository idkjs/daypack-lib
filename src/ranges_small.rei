let normalize:
  (
    ~skip_filter_invalid: bool=?,
    ~skip_filter_empty: bool=?,
    ~skip_sort: bool=?,
    ~modulo: option(int),
    ~to_int: 'a => int,
    ~of_int: int => 'a,
    Seq.t(Range.range('a))
  ) =>
  Seq.t(Range.range('a));

module Flatten: {
  let flatten:
    (
      ~modulo: option(int),
      ~to_int: 'a => int,
      ~of_int: int => 'a,
      Seq.t(Range.range('a))
    ) =>
    Seq.t('a);

  let flatten_list:
    (
      ~modulo: option(int),
      ~to_int: 'a => int,
      ~of_int: int => 'a,
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
      ~modulo: option(int),
      ~to_int: 'a => int,
      ~of_int: int => 'a,
      Seq.t('a)
    ) =>
    Seq.t(Range.range('a));

  let range_list_of_seq:
    (
      ~skip_filter_invalid: bool=?,
      ~skip_filter_empty: bool=?,
      ~skip_sort: bool=?,
      ~modulo: option(int),
      ~to_int: 'a => int,
      ~of_int: int => 'a,
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
      ~modulo: option(int),
      ~to_int: 'a => int,
      ~of_int: int => 'a,
      list('a)
    ) =>
    Seq.t(Range.range('a));

  let range_list_of_list:
    (
      ~skip_filter_invalid: bool=?,
      ~skip_filter_empty: bool=?,
      ~skip_sort: bool=?,
      ~modulo: option(int),
      ~to_int: 'a => int,
      ~of_int: int => 'a,
      list('a)
    ) =>
    list(Range.range('a));
};

module Make: (B: Range_small.B) => Ranges.S with type t := B.t;
