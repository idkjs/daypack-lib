open Test_utils;

module Qc = {
  let unpack_is_inverse_of_pack_period =
    QCheck.Test.make(
      ~count=5000,
      ~name="unpack_is_inverse_of_pack_period",
      QCheck.(pair(time_pattern, time_pattern)),
      p => {
        let p' =
          p
          |> Daypack_lib.Time_profile.Serialize.pack_period
          |> Daypack_lib.Time_profile.Deserialize.unpack_period;

        p == p';
      },
    );

  let unpack_is_inverse_of_pack_data =
    QCheck.Test.make(
      ~count=5000,
      ~name="unpack_is_inverse_of_pack_data",
      QCheck.(
        list_of_size(Gen.(int_bound(100)), pair(time_pattern, time_pattern))
      ),
      periods => {
        let d = Daypack_lib.Time_profile.{periods: periods};
        let d' =
          d
          |> Daypack_lib.Time_profile.Serialize.pack_data
          |> Daypack_lib.Time_profile.Deserialize.unpack_data;

        d == d';
      },
    );

  let suite = [
    unpack_is_inverse_of_pack_period,
    unpack_is_inverse_of_pack_data,
  ];
};
