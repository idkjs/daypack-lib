let prefix_string_match =
    (choices: list((string, 'a)), s: string): list((string, 'a)) => {
  let regexp = Re.Str.regexp_case_fold(s);
  choices
  |> List.filter(((k, _)) =>
       try(Re.Str.search_forward(regexp, k, 0) == 0) {
       | Not_found => false
       }
     );
};

let int32_int32_of_int64 = (x: int64): (int32, int32) => (
  Int64.shift_right_logical(x, 32) |> Int64.to_int32,
  x |> Int64.to_int32,
);

let int64_of_int32_int32 = ((x, y): (int32, int32)): int64 => {
  let left = Int64.shift_left(Int64.of_int32(x), 32);
  let right = Int64.logand(0x00000000FFFFFFFFL, Int64.of_int32(y));
  Int64.logor(left, right);
};

let take_first_n_list = (n: int, l: list('a)): list('a) => {
  let rec aux = (n, acc, l) =>
    if (n == 0) {
      List.rev(acc);
    } else {
      switch (l) {
      | [] => aux(0, acc, [])
      | [x, ...xs] => aux(pred(n), [x, ...acc], xs)
      };
    };

  assert(n >= 0);
  aux(n, [], l);
};

let take_last_n_list = (n: int, l: list('a)): list('a) =>
  l |> List.rev |> take_first_n_list(n) |> List.rev;

let convert_of_int_to_int64 = (f: int => 'a): (int64 => 'a) =>
  x => x |> Int64.to_int |> f;

let convert_to_int_to_int64 = (f: 'a => int): ('a => int64) =>
  x => x |> f |> Int64.of_int;

let get_ok_error_list = (l: list(result('a, 'b))): result(list('a), 'b) =>
  List.find_opt(Result.is_error, l)
  |> (
    x =>
      switch (x) {
      | None => None
      | Some(Ok(_)) => None
      | Some(Error(x)) => Some(x)
      }
  )
  |> (
    x =>
      switch (x) {
      | None => Ok(List.map(Result.get_ok, l))
      | Some(x) => Error(x)
      }
  );

let list_concat_map = (f: 'a => list('b), l: list('a)): list('b) =>
  List.to_seq(l) |> Seq.flat_map(x => f(x) |> List.to_seq) |> List.of_seq;

let list_concat_mapi = (f: (int, 'a) => list('b), l: list('a)): list('b) =>
  List.to_seq(l)
  |> OSeq.mapi((i, x) => (i, x))
  |> Seq.flat_map(((i, x)) => f(i, x) |> List.to_seq)
  |> List.of_seq;
