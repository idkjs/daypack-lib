type t = {
  days : int;
  hours : int;
  minutes : int;
  seconds : int;
}

val zero : t

val of_seconds : int64 -> (t, unit) result

val to_seconds : t -> int64

val normalize : t -> t

module Of_string : sig
  val of_string : string -> (t, string) result
end

module To_string : sig
  val human_readable_string_of_duration : t -> string
end
