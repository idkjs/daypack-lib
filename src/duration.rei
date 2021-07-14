type t = {
  days: int,
  hours: int,
  minutes: int,
  seconds: int,
};

let zero: t;

let of_seconds: int64 => result(t, unit);

let to_seconds: t => int64;

let normalize: t => t;

let duration_expr_parser: MParser.t(t, unit);

let of_string: string => result(t, string);

module To_string: {let human_readable_string_of_duration: t => string;};
