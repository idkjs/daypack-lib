let indent_single = "  ";

let bprintf = (~indent_level: int=0, buffer, fmt) => {
  for (_ in 0 to indent_level - 1) {
    Buffer.add_string(buffer, indent_single);
  };
  Printf.bprintf(buffer, fmt);
};
