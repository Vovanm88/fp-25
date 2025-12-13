let calculate_rms window =
  let n = List.length window in
  if n = 0 then 0.0
  else
    let sum_squares =
      List.fold_left (fun acc x -> acc +. (x *. x)) 0.0 window
    in
    Float.sqrt (sum_squares /. float_of_int n)

let calculate_envelope data window_size =
  let n = List.length data in
  if n = 0 then []
  else
    let data_array = Array.of_list data in
    let hop_size = max 1 (window_size / 4) in
    let rec calc acc idx =
      if idx >= n then List.rev acc
      else
        let window_end = min (idx + window_size) n in
        let window = Array.sub data_array idx (window_end - idx) in
        let rms = calculate_rms (Array.to_list window) in
        calc (rms :: acc) (idx + hop_size)
    in
    calc [] 0

let calculate_attack_release envelope sample_rate hop_size =
  let n = List.length envelope in
  if n < 2 then (0.0, 0.0)
  else
    let env_array = Array.of_list envelope in
    let max_val = Array.fold_left max 0.0 env_array in
    let threshold_10 = max_val *. 0.1 in
    let threshold_90 = max_val *. 0.9 in

    let attack_start = ref (-1) in
    let attack_end = ref (-1) in
    let found_attack = ref false in
    for i = 0 to n - 1 do
      if not !found_attack then (
        if !attack_start < 0 && env_array.(i) >= threshold_10 then
          attack_start := i;
        if env_array.(i) >= threshold_90 then (
          attack_end := i;
          if !attack_start >= 0 then found_attack := true))
    done;

    let release_start = ref (-1) in
    let release_end = ref (-1) in
    let found_release = ref false in
    for i = n - 1 downto 0 do
      if not !found_release then (
        if !release_start < 0 && env_array.(i) >= threshold_90 then
          release_start := i;
        if env_array.(i) <= threshold_10 then (
          release_end := i;
          if !release_start >= 0 then found_release := true))
    done;

    let attack_time =
      if !attack_start >= 0 && !attack_end >= 0 then
        float_of_int ((!attack_end - !attack_start) * hop_size)
        *. 1000.0 /. float_of_int sample_rate
      else 0.0
    in

    let release_time =
      if !release_start >= 0 && !release_end >= 0 then
        float_of_int ((!release_start - !release_end) * hop_size)
        *. 1000.0 /. float_of_int sample_rate
      else 0.0
    in

    (max 0.0 attack_time, max 0.0 release_time)
