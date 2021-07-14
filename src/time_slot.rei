exception Time_slot_is_invalid;

exception Time_slot_is_empty;

type t = (int64, int64);

let lt: (t, t) => bool;

let le: (t, t) => bool;

let gt: (t, t) => bool;

let ge: (t, t) => bool;

let compare: (t, t) => int;

let to_string: t => string;

let join: (t, t) => option(t);

let overlap_of_a_over_b:
  (~a: t, ~b: t) => (option(t), option(t), option(t));

module Check: {
  let is_valid: t => bool;

  let is_not_empty: t => bool;

  let check_if_valid: t => t;

  let check_if_not_empty: t => t;
};

module Serialize: {
  let pack_time_slot: ((int64, int64)) => ((int32, int32), (int32, int32));
};

module Deserialize: {
  let unpack_time_slot:
    (((int32, int32), (int32, int32))) => (int64, int64);
};
