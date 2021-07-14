type error =
  | Invalid_time_point_expr
  | Invalid_time_slot_expr;

type f_resolve_tse_name = string => option(Time_expr_ast.time_slot_expr);

type f_resolve_tpe_name = string => option(Time_expr_ast.time_point_expr);

type lang_fragment = [
  | `Time_point_expr
  | `Time_slot_expr
  | `Branching_time_slot_expr
  | `Time_pattern
];

let check_time_expr: Time_expr_ast.t => result(unit, unit);

module To_string: {
  let debug_string_of_hms_ranges: Time_expr_ast.hms_expr => string;
};

let time_expr_parser:
  (~enabled_fragments: list(lang_fragment)=?) =>
  MParser.t(Time_expr_ast.t, unit);

let of_string:
  (~enabled_fragments: list(lang_fragment)=?, string) =>
  result(Time_expr_ast.t, string);

let matching_time_slots:
  (
    ~f_resolve_tpe_name: f_resolve_tpe_name=?,
    ~f_resolve_tse_name: f_resolve_tse_name=?,
    Search_param.t,
    Time_expr_ast.t
  ) =>
  result(Seq.t(Time_slot.t), string);

let next_match_time_slot:
  (
    ~f_resolve_tpe_name: f_resolve_tpe_name=?,
    ~f_resolve_tse_name: f_resolve_tse_name=?,
    Search_param.t,
    Time_expr_ast.t
  ) =>
  result(option(Time_slot.t), string);
