open Test_utils

let qc_unpack_is_inverse_of_pack_days =
  QCheck.Test.make ~count:5000 ~name:"qc_unpack_is_inverse_of_pack_days" days
    (fun days ->
       let days' =
         days
         |> Daypack_lib.Time_pattern.Serialize.pack_days
         |> Daypack_lib.Time_pattern.Deserialize.unpack_days
       in
       days = days')

let qc_unpack_is_inverse_of_pack_pattern =
  QCheck.Test.make ~count:5000 ~name:"qc_unpack_is_inverse_of_pack_pattern"
    time_pattern (fun pattern ->
        let pattern' =
          pattern
          |> Daypack_lib.Time_pattern.Serialize.pack_pattern
          |> Daypack_lib.Time_pattern.Deserialize.unpack_pattern
        in
        pattern = pattern')

let suite =
  [ qc_unpack_is_inverse_of_pack_days; qc_unpack_is_inverse_of_pack_pattern ]