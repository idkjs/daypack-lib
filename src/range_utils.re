let result_range_get =
    (x: Range.range(result('a, 'b))): option(Range.range('a)) =>
  switch (x) {
  | `Range_inc(x, y) =>
    switch (x, y) {
    | (Ok(x), Ok(y)) => Some(`Range_inc((x, y)))
    | (_, _) => None
    }
  | `Range_exc(x, y) =>
    switch (x, y) {
    | (Ok(x), Ok(y)) => Some(`Range_exc((x, y)))
    | (_, _) => None
    }
  };
