let prefix_string_match (choices : (string * 'a) list) (s : string) :
  (string * 'a) list =
  let regexp = Str.regexp_case_fold s in
  choices
  |> List.filter (fun (k, _) ->
      try Str.search_forward regexp k 0 = 0 with Not_found -> false)

let int32_int32_of_int64 (x : int64) : int32 * int32 =
  (Int64.shift_right_logical x 32 |> Int64.to_int32, x |> Int64.to_int32)

let int64_of_int32_int32 ((x, y) : int32 * int32) : int64 =
  let left = Int64.shift_left (Int64.of_int32 x) 32 in
  let right = Int64.logand 0x00000000FFFFFFFFL (Int64.of_int32 y) in
  Int64.logor left right