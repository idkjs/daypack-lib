open MParser;

let alpha_string: t(string, unit) = (many1_chars(letter): t(string, unit));

let any_string: t(string, unit) = (many_satisfy(_ => true): t(string, unit));

let take_space: t(string, unit) = (many_chars(space): t(string, unit));

let ident_string = (~reserved_words: list(string)): t(string, unit) => {
  let reserved_words = List.map(String.lowercase_ascii, reserved_words);
  alpha_string
  >>= (
    s =>
      if (List.mem(String.lowercase_ascii(s), reserved_words)) {
        fail(Printf.sprintf("\"%s\" is a reserved word", s));
      } else {
        return(s);
      }
  );
};

let skip_non_num_string = (~end_markers) =>
  skip_satisfy(
    fun
    | '0' .. '9' => false
    | c =>
      switch (end_markers) {
      | None => true
      | Some(x) => !String.contains(x, c)
      },
  );

let nat_zero: t(int, unit) = (
  many1_satisfy(
    fun
    | '0' .. '9' => true
    | _ => false,
  )
  >>= (
    s =>
      try(return(int_of_string(s))) {
      | _ => fail(Printf.sprintf("Integer %s is out of range", s))
      }
  ):
    t(int, unit)
);

let float_non_neg: t(float, unit) = (
  many1_satisfy(
    fun
    | '0' .. '9' => true
    | _ => false,
  )
  >>= (
    x =>
      attempt(
        char('.')
        >> many1_satisfy(
             fun
             | '0' .. '9' => true
             | _ => false,
           ),
      )
      <|> return("0")
      >>= (
        y => {
          let s = x ++ "." ++ y;
          try(return(float_of_string(s))) {
          | _ => fail(Printf.sprintf("Float %s is out of range", s))
          };
        }
      )
  ):
    t(float, unit)
);

let comma: t(char, unit) = (char(','): t(char, unit));

let dot: t(char, unit) = (char('.'): t(char, unit));

let hyphen: t(char, unit) = (char('-'): t(char, unit));

let non_square_bracket_string: t(string, unit) = (
  many_satisfy(
    fun
    | '['
    | ']' => false
    | _ => true,
  ):
    t(string, unit)
);

let non_parenthesis_string: t(string, unit) = (
  many_satisfy(
    fun
    | '('
    | ')' => false
    | _ => true,
  ):
    t(string, unit)
);

let non_space_string: t(string, unit) = (
  many_chars(non_space): t(string, unit)
);

let sep_by_comma = (p: t('a, unit)): t(list('a), unit) =>
  sep_by(p, attempt(spaces >> comma >> spaces));

let sep_by_comma1 = (p: t('a, unit)): t(list('a), unit) =>
  sep_by1(p, attempt(spaces >> comma >> spaces));

let option = (default: 'a, p): t('a, 'b) => attempt(p) <|> return(default);

let string_of_pos = pos => {
  let (_index, lnum, cnum) = pos;
  Printf.sprintf("%d:%d", lnum, cnum);
};

/* let sep_res_seq ~by ~end_markers (p : ('a, 'b) t) : ('a result Seq.t, 'b) t =
 *   sep_by (char by)
 *     ( get_pos
 *       >>= fun pos ->
 *       many1_satisfy (fun c -> c <> by && not (String.contains end_markers c))
 *       >>= fun s -> return (pos, s) )
 *   |>> fun l ->
 *     ( l
 *       |> List.to_seq
 *       |> Seq.map (fun (pos, s) ->
 *           match
 *           parse_string
 *             ( p
 *               >>= fun x ->
 *               attempt eoi >> return x
 *               <|> ( get_pos
 *                     >>= fun pos ->
 *                     any_string
 *                     >>= fun s -> (fail (Printf.sprintf "Invalid syntax: %s, pos: %s" s (string_of_pos pos) )) )
 *             )
 *             s
 *             ()
 *           with
 *           | Success
 *     )
 *
 * let sep_res ~by ~end_markers (p : 'a t) : ('a, string) result list t =
 *   sep_res_seq ~by ~end_markers p >>= fun s -> return (List.of_seq s)
 *
 * let sep_fail_on_first_fail ~by ~end_markers (p : 'a t) : 'a list t =
 *   sep_res_seq ~by ~end_markers p
 *   >>= fun s ->
 *   match Seq_utils.find_first_error_or_return_whole_list s with
 *   | Ok l -> return l
 *   | Error s -> fail s */

/* let chainl1 x op =
 *   let rec aux a =
 *     attempt (spaces >> op << spaces)
 *     >>= (fun f -> x >>= fun b -> aux (f a b))
 *         <|> return a
 *   in
 *   x >>= aux */

let invalid_syntax = (~text, ~pos) =>
  fail(
    Printf.sprintf("Invalid syntax: %s, pos: %s", text, string_of_pos(pos)),
  );

let extraneous_text_check = (~end_markers) =>
  spaces
  >> get_pos
  >>= (
    pos =>
      many_satisfy(c => !String.contains(end_markers, c))
      >>= (
        s =>
          switch (s) {
          | "" => return()
          | text => invalid_syntax(~text, ~pos)
          }
      )
  );

let result_of_mparser_result = (x: result('a)): Result.t('a, string) =>
  switch (x) {
  | Success(x) => Ok(x)
  | [@implicit_arity] Failed(s, err) =>
    switch (err) {
    | No_error => Error("Unknown error")
    | [@implicit_arity] Parse_error(pos, msgs) =>
      switch (
        List.fold_left(
          (res, msg) =>
            switch (res) {
            | Some(x) => Some(x)
            | None =>
              switch (msg) {
              | Unexpected_error(s) =>
                Some(
                  Printf.sprintf(
                    "Unexpected: %s, pos: %s",
                    s,
                    string_of_pos(pos),
                  ),
                )
              | Expected_error(s) =>
                Some(
                  Printf.sprintf(
                    "Expected: %s, pos: %s",
                    s,
                    string_of_pos(pos),
                  ),
                )
              | Message_error(s) => Some(s)
              | [@implicit_arity] Compound_error(s, _) => Some(s)
              | Backtrack_error(_) => res
              | Unknown_error => res
              }
            },
          None,
          msgs,
        )
      ) {
      | None =>
        Error(Printf.sprintf("Unknown error, pos: %s", string_of_pos(pos)))
      | Some(s) => Error(s)
      }
    }
  };
