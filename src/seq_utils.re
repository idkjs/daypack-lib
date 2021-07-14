open Int64_utils;

let nat: Seq.t(int) = (
  {
    open OSeq.Generator;
    let rec aux = n => yield(n) >>= (() => aux(n + 1));
    run(aux(0));
  }:
    Seq.t(int)
);

/* let nat_int64 : int64 Seq.t =
 *   let open OSeq.Generator in
 *   let rec aux n = yield n >>= fun () -> aux (n ^+ 1L) in
 *   run (aux 0L) */

let zero_to_n_exc = (n): Seq.t(int) => {
  let rec aux = (cur, n) =>
    if (cur < n) {
      () => [@implicit_arity] Seq.Cons(cur, aux(cur + 1, n));
    } else {
      Seq.empty;
    };

  aux(0, n);
};

let zero_to_n_inc = n => zero_to_n_exc(n + 1);

let a_to_b_exc_int64 = (~a, ~b): Seq.t(int64) => {
  let rec aux = (cur, n) =>
    if (cur < n) {
      () => [@implicit_arity] Seq.Cons(cur, aux(cur +^ 1L, n));
    } else {
      Seq.empty;
    };

  aux(a, b);
};

let a_to_b_inc_int64 = (~a, ~b): Seq.t(int64) =>
  a_to_b_exc_int64(~a, ~b=b +^ 1L);

let zero_to_n_exc_int64 = (n): Seq.t(int64) =>
  a_to_b_exc_int64(~a=0L, ~b=n);

let zero_to_n_inc_int64 = n => zero_to_n_exc_int64(n +^ 1L);

let mod_int = n => {
  let rec aux = (cur, n) =>
    if (cur < n) {
      () => [@implicit_arity] Seq.Cons(cur, aux(cur + 1, n));
    } else {
      aux(0, n);
    };

  aux(0, n);
};

/* let mapi (f : int -> 'a -> 'b) (s : 'a Seq.t) : 'b Seq.t =
 *   let rec aux f s i =
 *     match s () with
 *     | Seq.Nil -> Seq.empty
 *     | Seq.Cons (x, rest) -> fun () -> Seq.Cons (f i x, aux f rest (i + 1))
 *   in
 *   aux f s 0 */

/* let mapi_int64 (f : int64 -> 'a -> 'b) (s : 'a Seq.t) : 'b Seq.t =
 *   let rec aux f s i =
 *     match s () with
 *     | Seq.Nil -> Seq.empty
 *     | Seq.Cons (x, rest) -> fun () -> Seq.Cons (f i x, aux f rest (i ^+ 1L))
 *   in
 *   aux f s 0L */

let collect_round_robin =
    (type a, f: (a, a) => bool, batches: list(Seq.t(a)))
    : Seq.t(list(option(a))) => {
  let rec get_usable_part = (compare, cur: a, seq: Seq.t(a)): Seq.t(a) =>
    switch (seq()) {
    | Seq.Nil => Seq.empty
    | [@implicit_arity] Seq.Cons(x, rest) as s =>
      if (f(cur, x)) {
        (() => s);
      } else {
        get_usable_part(compare, cur, rest);
      }
    };

  let rec aux =
          (compare, cur: option(a), batches: list(Seq.t(a)))
          : Seq.t(list(option(a))) => {
    let (cur, acc, new_batches) =
      List.fold_left(
        ((cur, acc, new_batches), seq) => {
          let usable =
            switch (cur) {
            | None => seq
            | Some(cur_start) => get_usable_part(compare, cur_start, seq)
            };

          switch (usable()) {
          | Seq.Nil => (cur, [None, ...acc], new_batches)
          | [@implicit_arity] Seq.Cons(x, rest) => (
              Some(x),
              [Some(x), ...acc],
              [rest, ...new_batches],
            )
          };
        },
        (cur, [], []),
        batches,
      );

    if (List.exists(Option.is_some, acc)) {
      let acc = List.rev(acc);
      let new_batches = List.rev(new_batches);
      () => [@implicit_arity] Seq.Cons(acc, aux(compare, cur, new_batches));
    } else {
      Seq.empty;
    };
  };

  aux(compare, None, batches);
};

let check_if_f_holds_for_immediate_neighbors =
    (type a, ~f: (a, a) => bool, ~f_exn: (a, a) => exn, s: Seq.t(a))
    : Seq.t(a) => {
  let rec aux = (f, f_exn, cur: option(a), s: Seq.t(a)): Seq.t(a) =>
    switch (s()) {
    | Seq.Nil =>
      switch (cur) {
      | None => Seq.empty
      | Some(x) => Seq.return(x)
      }
    | [@implicit_arity] Seq.Cons(x, rest) =>
      switch (cur) {
      | None => aux(f, f_exn, Some(x), rest)
      | Some(cur) =>
        if (f(cur, x)) {
          (
            () =>
              [@implicit_arity] Seq.Cons(cur, aux(f, f_exn, Some(x), rest))
          );
        } else {
          raise(f_exn(cur, x));
        }
      }
    };

  aux(f, f_exn, None, s);
};

let find_first_error_or_return_whole_list =
    (s: Seq.t(result('a, 'b))): result(list('a), 'b) => {
  let rec aux = (acc, s: Seq.t(result('a, 'b))): result(list('a), 'b) =>
    switch (s()) {
    | Seq.Nil => Ok(List.rev(acc))
    | [@implicit_arity] Seq.Cons(x, rest) =>
      switch (x) {
      | Ok(x) => aux([x, ...acc], rest)
      | Error(s) => Error(s)
      }
    };

  aux([], s);
};
