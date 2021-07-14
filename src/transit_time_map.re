include Map.Make({
  type t = (string, string);

  let compare = compare;
});
