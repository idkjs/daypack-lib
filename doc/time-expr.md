# Time expression specification

## Syntax

```
<hms> ::=
  | <hour>
  | <hour> : <minute>
  | <hour> : <minute> : <second>

<hms_mode> ::=
  | "am"
  | "pm"
  | ... (all other variants with different casing of above)

<weekday> ::=
  | "monday"
  | "tuesday"
  | "wednesday"
  | "thursday"
  | "friday"
  | "saturday"
  | "sunday"
  | ... (all prefixes that allow unique match of any of above)
  | ... (all other variants with different casing of above)

<month_day> ::=
  | "1" | ... | "31"

<day> ::=
  | <weekday>
  | <month_day>

<human_int_month> ::=
  | "1" | ... | "12"

<direct_pick_month> ::=
  | "january"
  | "february"
  | "march"
  | "april"
  | "may"
  | "june"
  | "july"
  | "august"
  | "september"
  | "october"
  | "november"
  | "december"
  | ... (all prefixes that allow unique match of any of above)
  | ... (all other variants with different casing of above)

<month> ::=
  | <human_int_month>
  | <direct_pick_month>

<year> ::=
  | "0" | ... | "9999" (or whatever the exact numbers that are representable in Ptime, TODO)

<time_point_expr> ::=
  | <hms> [<hms_mode>]
  | <day> <hms> [<hms_mode>]
  | <month> <month_day> <hms> [<hms_mode>]
  | <year> <direct_pick_month> <month_day> <hms> [<hms_mode>]
  | <year> '-' <human_int_month> '-' <month_day> <hms> [<hms_mode>]

<hms_range> ::=
  | <hms> "to" <hms>

<hms_ranges> ::=
  | <hms_range> [, <hms_ranges>]

<month_day_range> ::=
  | <month_day> "to" <month_day>

<month_days> ::=
  | <month_day> [',' <month_days>]
  | <month_day_range> [',' <month_days>]

<weekday_range> ::=
  | <weekday> "to" <weekday>

<weekdays> ::=
  | <weekday> [',' <weekdays>]
  | <weekday_range> [',' <weekdays>]

<month_range> ::=
  | <month> "to" <month>

<months> ::=
  | <month> [',' <months>]
  | <month_range> [',' <months>]

<year_range> ::=
  | <year> "to" <year>

<years> ::=
  | <year> [',' <years>]
  | <year_range> [',' <years>]

<unbounded_time_slot_expr> ::=
  | <time_point_expr> "to" <time_point_expr>
  |                            <weekdays> '.' <hms_ranges>
  |                          <month_days> '.' <hms_ranges>
  |             <months> '.' <month_days> '.' <hms_ranges>
  | <years> '.' <months> '.' <month_days> '.' <hms_ranges>

<time_slot_expr> ::=
  | ["next-batch"] <unbounded_time_slot_expr>

<cron_expr> ::=
  TODO

<time_pattern> ::=
  | <cron_expr>
  | [ 'y' [ '[' <years>      ']' ] ]
    [ 'm' [ '[' <months>     ']' ] ]
    [ 'w' [ '[' <weekdays>   ']' ] ]
    [ 'd' [ '[' <month_days> ']' ] ]
      'h' [ '[' <hours>      ']' ]
      'm' [ '[' <minutes>    ']' ]
    [ 's' [ '[' <seconds>    ']' ] ]

<time_expr> ::=
  | <time_point_expr>
  | <time_slot_expr>
  | <time_pattern>
  | "not" <time_expr>
  | <time_expr> "&&" <time_expr>
  | <time_expr> "||" <time_expr>
  | '(' <time_expr> ')'
  | "next" <time_expr>
  | "every" <time_expr>
```

## Semantics

Semantic functions:

```
eval_time_point_expr : time_point_expr -> (int64 * int64) Seq.t
eval_time_slot_expr : time_slot_expr -> (int64 * int64) Seq.t
eval_time_pattern : time_pattern -> (int64 * int64) Seq.t
eval_time_expr : time_expr -> (int64 * int64) Seq.t
```

Semantic equations:

```
TODO
```