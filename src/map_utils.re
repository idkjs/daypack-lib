module type S = {
  type key;

  type t('a);

  type diff('a) = {
    /* common : 'a t; */
    /* updated : ('a * 'a) t; */
    added: t('a),
    removed: t('a),
  };

  let diff: (~old: t('a), t('a)) => diff('a);

  let add_diff: (diff('a), t('a)) => t('a);

  let sub_diff: (diff('a), t('a)) => t('a);

  let range: (~start: option(key), ~end_exc: option(key), t('a)) => t('a);
};

module type S_bucketed = {
  type map('a);

  type set;

  type diff_bucketed = {
    /* common : set map; */
    added: map(set),
    removed: map(set),
  };

  let diff_bucketed: (~old: map(set), map(set)) => diff_bucketed;

  let add_diff_bucketed: (diff_bucketed, map(set)) => map(set);

  let sub_diff_bucketed: (diff_bucketed, map(set)) => map(set);
};

module Make =
       (M: Map.S)
       : (S with type key := M.key and type t('a) := M.t('a)) => {
  type t('a) = M.t('a);

  type diff('a) = {
    /* common : 'a t; */
    /* updated : ('a * 'a) t; */
    added: t('a),
    removed: t('a),
  };

  /* let get_common (m1 : 'a t) (m2 : 'a t) : 'a t =
   *   M.merge
   *     (fun _key x1 x2 ->
   *        match (x1, x2) with
   *        | None, None -> None
   *        | Some _, None -> None
   *        | None, Some _ -> None
   *        | Some x1, Some x2 -> if x1 = x2 then Some x1 else None)
   *     m1 m2 */

  /* let get_updated (m1 : 'a t) (m2 : 'a t) : ('a * 'a) t =
   *   M.merge
   *     (fun _key x1 x2 ->
   *        match (x1, x2) with
   *        | None, None -> None
   *        | Some _, None -> None
   *        | None, Some _ -> None
   *        | Some x1, Some x2 -> if x1 <> x2 then Some (x1, x2) else None)
   *     m1 m2 */

  let get_added = (m1: t('a), m2: t('a)): t('a) =>
    M.merge(
      (_key, x1, x2) =>
        switch (x1, x2) {
        | (None, _) => x2
        | (Some(_), None) => None
        | (Some(x1), Some(x2)) =>
          if (x1 == x2) {
            None;
          } else {
            Some(x2);
          }
        },
      m1,
      m2,
    );

  let get_removed = (m1: t('a), m2: t('a)): t('a) =>
    M.merge(
      (_key, x1, x2) =>
        switch (x1, x2) {
        | (None, _) => None
        | (Some(_), None) => x1
        | (Some(x1), Some(x2)) =>
          if (x1 == x2) {
            None;
          } else {
            Some(x1);
          }
        },
      m1,
      m2,
    );

  let diff = (~old: t('a), m: t('a)): diff('a) => {
    /* common = get_common old m; */
    /* updated = get_updated old m; */
    added: get_added(old, m),
    removed: get_removed(old, m),
  };

  let add_diff = (diff: diff('a), m: t('a)): t('a) =>
    m
    /* apply updates */
    /* |> M.mapi (fun key x ->
     *     match M.find_opt key diff.updated with
     *     | None -> x
     *     | Some (x1, x2) -> if x1 = x then x2 else raise Exceptions.Invalid_diff) */
    /* remove */
    |> M.merge(
         (_key, to_be_removed, x) =>
           switch (to_be_removed, x) {
           | (None, _) => x
           | (_, None) => raise(Exceptions.Invalid_diff)
           | (Some(to_be_removed), Some(x)) =>
             if (x == to_be_removed) {
               None;
             } else {
               raise(Exceptions.Invalid_diff);
             }
           },
         diff.removed,
       )
    /* add */
    |> M.union((_key, added, _) => Some(added), diff.added);

  let sub_diff = (diff: diff('a), m: t('a)): t('a) =>
    m
    /* revert updates */
    /* |> M.mapi (fun key x ->
     *     match M.find_opt key diff.updated with
     *     | None -> x
     *     | Some (x1, x2) -> if x2 = x then x1 else raise Exceptions.Invalid_diff) */
    /* revert add */
    |> M.merge(
         (_key, to_be_removed, x) =>
           switch (to_be_removed, x) {
           | (None, _)
           | (_, None) => x
           | (Some(to_be_removed), Some(x)) =>
             if (x == to_be_removed) {
               None;
             } else {
               raise(Exceptions.Invalid_diff);
             }
           },
         diff.added,
       )
    /* revert remove */
    |> M.union((_key, removed, _) => Some(removed), diff.removed);

  let range =
      (~start: option(M.key), ~end_exc: option(M.key), m: t('a)): t('a) => {
    let add' = (key: M.key, x: option('a), m: t('a)) =>
      switch (x) {
      | None => m
      | Some(x) => M.add(key, x, m)
      };

    switch (start, end_exc) {
    | (None, None) => m
    | (Some(start), None) =>
      let (_, eq, after) = M.split(start, m);
      add'(start, eq, after);
    | (None, Some(end_exc)) =>
      let (before, eq, _) = M.split(end_exc, m);
      add'(end_exc, eq, before);
    | (Some(start), Some(end_exc)) =>
      let after_or_from_start = {
        let (_, eq, after) = M.split(start, m);
        add'(start, eq, after);
      };

      let before_or_on_end_exc = {
        let (before, eq, _) = M.split(end_exc, after_or_from_start);
        add'(end_exc, eq, before);
      };

      before_or_on_end_exc;
    };
  };
};

module Make_bucketed =
       (Map: Map.S, Set: Set.S)
       : (S_bucketed with type map('a) := Map.t('a) and type set := Set.t) => {
  type map('a) = Map.t('a);

  type set = Set.t;

  type diff_bucketed = {
    /* common : set map; */
    added: map(set),
    removed: map(set),
  };

  /* let get_common (m1 : set map) (m2 : set map) : set map =
   *   Map.merge
   *     (fun _key s1 s2 ->
   *        match (s1, s2) with
   *        | None, None -> None
   *        | Some _, None -> None
   *        | None, Some _ -> None
   *        | Some s1, Some s2 -> Some (Set.inter s1 s2))
   *     m1 m2 */

  let get_added = (m1: map(set), m2: map(set)): map(set) =>
    Map.merge(
      (_key, s1, s2) =>
        switch (s1, s2) {
        | (None, _) => s2
        | (Some(_), None) => None
        | (Some(s1), Some(s2)) =>
          if (Set.equal(s1, s2)) {
            None;
          } else {
            Some(Set.diff(s2, s1));
          }
        },
      m1,
      m2,
    );

  let get_removed = (m1: map(set), m2: map(set)): map(set) =>
    Map.merge(
      (_key, s1, s2) =>
        switch (s1, s2) {
        | (None, _) => None
        | (Some(_), None) => s1
        | (Some(s1), Some(s2)) =>
          if (Set.equal(s1, s2)) {
            None;
          } else {
            Some(Set.diff(s1, s2));
          }
        },
      m1,
      m2,
    );

  let diff_bucketed = (~old: map(set), m: map(set)): diff_bucketed => {
    /* common = get_common old m; */
    added: get_added(old, m),
    removed: get_removed(old, m),
  };

  let add_diff_bucketed = (diff: diff_bucketed, m: map(set)): map(set) =>
    m
    /* remove */
    |> Map.merge(
         (_key, to_be_removed, s) =>
           switch (to_be_removed, s) {
           | (None, _) => s
           | (_, None) => raise(Exceptions.Invalid_diff)
           | (Some(to_be_removed), Some(s)) =>
             if (Set.equal(to_be_removed, s)) {
               None;
             } else {
               Some(Set.diff(s, to_be_removed));
             }
           },
         diff.removed,
       )
    /* add */
    |> Map.union((_key, s1, s2) => Some(Set.union(s1, s2)), diff.added);

  let sub_diff_bucketed = (diff: diff_bucketed, m: map(set)): map(set) =>
    m
    /* revert add */
    |> Map.merge(
         (_key, to_be_removed, s) =>
           switch (to_be_removed, s) {
           | (None, _) => s
           | (_, None) => raise(Exceptions.Invalid_diff)
           | (Some(to_be_removed), Some(s)) =>
             if (Set.equal(to_be_removed, s)) {
               None;
             } else {
               Some(Set.diff(s, to_be_removed));
             }
           },
         diff.added,
       )
    /* revert remove */
    |> Map.union((_key, s1, s2) => Some(Set.union(s1, s2)), diff.removed);
};
