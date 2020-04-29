type 'a t =
  [ `Range_inc of 'a * 'a
  | `Range_exc of 'a * 'a
  ]

let b_is_next_of_a_int64 (type a) ~(to_int64 : a -> int64) x y =
  let x = to_int64 x in
  let y = to_int64 y in
  Int64.succ x = y

let b_is_next_of_a (type a) ~(to_int : a -> int) x y =
  let x = to_int x in
  let y = to_int y in
  succ x = y

let map ~(f_inc : 'a * 'a -> 'b * 'b) ~(f_exc : 'a * 'a -> 'b * 'b) (t : 'a t) :
  'b t =
  match t with
  | `Range_inc (x, y) ->
    let x, y = f_inc (x, y) in
    `Range_inc (x, y)
  | `Range_exc (x, y) ->
    let x, y = f_exc (x, y) in
    `Range_exc (x, y)

module Flatten = struct
  let flatten_into_seq_internal_int64 ~(modulo : int64 option)
      ~(of_int64 : int64 -> 'a) ~(to_int64 : 'a -> int64) (t : 'a t) : 'a Seq.t
    =
    match t with
    | `Range_inc (start, end_inc) -> (
        let start = to_int64 start in
        let end_inc = to_int64 end_inc in
        if start <= end_inc then
          Seq_utils.a_to_b_inc_int64 ~a:start ~b:end_inc |> Seq.map of_int64
        else
          match modulo with
          | None -> raise (Invalid_argument "End is before start")
          | Some modulo ->
            if modulo <= 0L then raise (Invalid_argument "Modulo is <= 0")
            else
              OSeq.append
                (Seq_utils.a_to_b_exc_int64 ~a:start ~b:modulo)
                (Seq_utils.a_to_b_inc_int64 ~a:0L ~b:end_inc)
              |> Seq.map of_int64 )
    | `Range_exc (start, end_exc) -> (
        let start = to_int64 start in
        let end_exc = to_int64 end_exc in
        if start <= end_exc then
          Seq_utils.a_to_b_exc_int64 ~a:start ~b:end_exc |> Seq.map of_int64
        else
          match modulo with
          | None -> raise (Invalid_argument "End is before start")
          | Some modulo ->
            if modulo <= 0L then raise (Invalid_argument "Modulo is <= 0")
            else
              OSeq.append
                (Seq_utils.a_to_b_exc_int64 ~a:start ~b:modulo)
                (Seq_utils.a_to_b_exc_int64 ~a:0L ~b:end_exc)
              |> Seq.map of_int64 )

  let flatten_into_seq_internal ~(modulo : int option) ~(of_int : int -> 'a)
      ~(to_int : 'a -> int) (t : 'a t) : 'a Seq.t =
    let modulo = Option.map Int64.of_int modulo in
    let of_int64 = Misc_utils.convert_of_int_to_int64 of_int in
    let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
    flatten_into_seq_internal_int64 ~modulo ~of_int64 ~to_int64 t

  let big_flatten_into_seq ?(modulo : int64 option) ~(of_int64 : int64 -> 'a)
      ~(to_int64 : 'a -> int64) (t : 'a t) : 'a Seq.t =
    flatten_into_seq_internal_int64 ~modulo ~of_int64 ~to_int64 t

  let big_flatten_into_list ?(modulo : int64 option) ~(of_int64 : int64 -> 'a)
      ~(to_int64 : 'a -> int64) (t : 'a t) : 'a list =
    flatten_into_seq_internal_int64 ~modulo ~of_int64 ~to_int64 t |> List.of_seq

  let flatten_into_seq ?(modulo : int option) ~(of_int : int -> 'a)
      ~(to_int : 'a -> int) (t : 'a t) : 'a Seq.t =
    flatten_into_seq_internal ~modulo ~of_int ~to_int t

  let flatten_into_list ?(modulo : int option) ~(of_int : int -> 'a)
      ~(to_int : 'a -> int) (t : 'a t) : 'a list =
    flatten_into_seq_internal ~modulo ~of_int ~to_int t |> List.of_seq
end

module Of_seq = struct
  let big_range_seq_of_seq (type a) ~(to_int64 : a -> int64) (s : a Seq.t) :
    a t Seq.t =
    let rec aux to_int64 (acc_inc : (a * a) option) (s : a Seq.t) : a t Seq.t =
      match s () with
      | Seq.Nil -> (
          match acc_inc with
          | None -> Seq.empty
          | Some (start, end_inc) -> Seq.return (`Range_inc (start, end_inc)) )
      | Seq.Cons (x, rest) -> (
          match acc_inc with
          | None -> aux to_int64 (Some (x, x)) rest
          | Some (start, end_inc) ->
            if b_is_next_of_a_int64 ~to_int64 end_inc x then
              aux to_int64 (Some (start, x)) rest
            else fun () ->
              Seq.Cons (`Range_inc (start, end_inc), aux to_int64 None rest) )
    in
    aux to_int64 None s

  let big_range_list_of_seq (type a) ~(to_int64 : a -> int64) (s : a Seq.t) :
    a t list =
    big_range_seq_of_seq ~to_int64 s |> List.of_seq

  let range_seq_of_seq (type a) ~(to_int : a -> int) (s : a Seq.t) : a t Seq.t =
    let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
    big_range_seq_of_seq ~to_int64 s

  let range_list_of_seq (type a) ~(to_int : a -> int) (s : a Seq.t) : a t list =
    range_seq_of_seq ~to_int s |> List.of_seq
end

module Of_list = struct
  let big_range_seq_of_list (type a) ~(to_int64 : a -> int64) (l : a list) :
    a t Seq.t =
    List.to_seq l |> Of_seq.big_range_seq_of_seq ~to_int64

  let big_range_list_of_list (type a) ~(to_int64 : a -> int64) (l : a list) :
    a t list =
    List.to_seq l |> Of_seq.big_range_seq_of_seq ~to_int64 |> List.of_seq

  let range_seq_of_list (type a) ~(to_int : a -> int) (l : a list) : a t Seq.t =
    List.to_seq l |> Of_seq.range_seq_of_seq ~to_int

  let range_list_of_list (type a) ~(to_int : a -> int) (l : a list) : a t list =
    List.to_seq l |> Of_seq.range_seq_of_seq ~to_int |> List.of_seq
end

module Merge = struct
  let big_merge (type a) ~(to_int64 : a -> int64) (x : a t) (y : a t) :
    a t option =
    match (x, y) with
    | `Range_inc (x_start, x_end_inc), `Range_inc (y_start, y_end_inc) ->
      if b_is_next_of_a_int64 ~to_int64 x_end_inc y_start then
        Some (`Range_inc (x_start, y_end_inc))
      else None
    | `Range_inc (x_start, x_end_inc), `Range_exc (y_start, y_end_exc) ->
      if b_is_next_of_a_int64 ~to_int64 x_end_inc y_start then
        Some (`Range_exc (x_start, y_end_exc))
      else None
    | `Range_exc (x_start, x_end_exc), `Range_inc (y_start, y_end_inc) ->
      if x_end_exc = y_start then Some (`Range_inc (x_start, y_end_inc))
      else None
    | `Range_exc (x_start, x_end_exc), `Range_exc (y_start, y_end_exc) ->
      if x_end_exc = y_start then Some (`Range_exc (x_start, y_end_exc))
      else None

  let merge (type a) ~(to_int : a -> int) (x : a t) (y : a t) : a t option =
    let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
    big_merge ~to_int64 x y
end

module Compress = struct
  let big_compress_seq (type a) ~(to_int64 : a -> int64) (s : a t Seq.t) :
    a t Seq.t =
    let rec aux (to_int64 : a -> int64) (acc : a t option) (s : a t Seq.t) :
      a t Seq.t =
      match s () with
      | Seq.Nil -> (
          match acc with None -> Seq.empty | Some x -> Seq.return x )
      | Seq.Cons (x, rest) -> (
          match acc with
          | None -> aux to_int64 (Some x) rest
          | Some acc -> (
              match Merge.big_merge ~to_int64 acc x with
              | None -> fun () -> Seq.Cons (acc, aux to_int64 (Some x) rest)
              | Some res -> aux to_int64 (Some res) rest ) )
    in
    aux to_int64 None s

  let big_compress_list (type a) ~(to_int64 : a -> int64) (l : a t list) :
    a t list =
    List.to_seq l |> big_compress_seq ~to_int64 |> List.of_seq

  let compress_seq (type a) ~(to_int : a -> int) (s : a t Seq.t) : a t Seq.t =
    let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
    big_compress_seq ~to_int64 s

  let compress_list (type a) ~(to_int : a -> int) (l : a t list) : a t list =
    let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
    big_compress_list ~to_int64 l
end
