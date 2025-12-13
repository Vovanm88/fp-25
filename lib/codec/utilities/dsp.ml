(* Digital Signal Processing utilities *)
open Complex

(* MDCT Reference: O(n²) - for testing *)
let mdct_transform_reference data =
  let n = List.length data in if n = 0 then [] else
  let arr = Array.of_list data and m = n / 2 and w = Float.pi /. float_of_int n in
  Array.to_list (Array.init m (fun k ->
    let s = ref 0.0 in for i = 0 to n-1 do
      s := !s +. arr.(i) *. Float.cos (w *. (float_of_int i +. 0.5 +. float_of_int m) *. (float_of_int k +. 0.5))
    done; !s))

(* IMDCT Reference: O(n²) - for testing *)
let imdct_transform_reference data =
  let m = List.length data in if m = 0 then [] else
  let arr = Array.of_list data and n = 2 * m and w = Float.pi /. float_of_int (2 * m) in
  Array.to_list (Array.init n (fun i ->
    let s = ref 0.0 in for k = 0 to m-1 do
      s := !s +. arr.(k) *. Float.cos (w *. (float_of_int i +. 0.5 +. float_of_int m) *. (float_of_int k +. 0.5))
    done; !s /. float_of_int m))

(* Fast MDCT: O(n log n) via FFT module *)
let mdct_transform data =
  match Fft.mdct_transform_fast data with [] -> mdct_transform_reference data | r -> r

(* Fast IMDCT: O(n log n) via FFT module *)
let imdct_transform data =
  match Fft.imdct_transform_fast data with [] -> imdct_transform_reference data | r -> r

(* DFT Reference: O(n²) - for testing *)
let dft_transform_reference data =
  let n = List.length data in if n = 0 then [] else
  let omega = 2. *. Float.pi /. float_of_int n in
  List.concat_map (fun k ->
    let sum = List.fold_left (fun acc (i, x) ->
      let angle = omega *. float_of_int k *. float_of_int i in
      add acc { re = x *. Float.cos angle; im = -.x *. Float.sin angle }
    ) zero (List.mapi (fun i x -> (i, x)) data) in
    [sum.re; sum.im]
  ) (List.init n Fun.id)

(* IDFT Reference: O(n²) - for testing *)
let idft_transform_reference data =
  let n = List.length data / 2 in if n = 0 then [] else
  let omega = 2. *. Float.pi /. float_of_int n in
  List.map (fun i ->
    let sum = List.fold_left (fun acc k ->
      let re = List.nth data (2*k) and im = List.nth data (2*k+1) in
      let angle = omega *. float_of_int k *. float_of_int i in
      add acc { re = re *. Float.cos angle -. im *. Float.sin angle;
                im = re *. Float.sin angle +. im *. Float.cos angle }
    ) zero (List.init n Fun.id) in
    sum.re /. float_of_int n
  ) (List.init n Fun.id)

(* Fast DFT: O(n log n) via FFT *)
let dft_transform data =
  let n = List.length data in
  if n = 0 then []
  else if n = 1 then [List.hd data; 0.0]
  else
    (* Find next power of 2 *)
    let rec next_pow2 x acc = if acc >= x then acc else next_pow2 x (acc * 2) in
    let n_padded = next_pow2 n 2 in
    (* Pad with zeros if needed *)
    let padded = if n_padded > n then
      data @ List.init (n_padded - n) (fun _ -> 0.0)
    else data in
    (* Find nbits *)
    let rec log2 x acc = if x <= 1 then acc else log2 (x lsr 1) (acc + 1) in
    let nbits = log2 n_padded 0 in
    (* Use FFT *)
    match Fft.fft_init nbits false with
    | None -> dft_transform_reference data
    | Some fft_ctx ->
      let fft_input = Array.of_list (List.map (fun x -> Fft.complex x 0.0) padded) in
      Fft.fft_calc fft_ctx fft_input;
      (* Extract first n results and interleave re/im *)
      List.concat_map (fun i -> 
        if i < n then [fft_input.(i).re; fft_input.(i).im]
        else []
      ) (List.init n_padded Fun.id)

(* Fast IDFT: O(n log n) via FFT *)
let idft_transform data =
  let n = List.length data / 2 in
  if n = 0 then []
  else if n = 1 then [List.nth data 0]
  else
    (* Find next power of 2 *)
    let rec next_pow2 x acc = if acc >= x then acc else next_pow2 x (acc * 2) in
    let n_padded = next_pow2 n 2 in
    (* Convert interleaved re/im to complex array *)
    let fft_input = Array.init n_padded (fun i ->
      if i < n then
        Fft.complex (List.nth data (2*i)) (List.nth data (2*i+1))
      else
        Fft.complex_zero ()
    ) in
    (* Conjugate input *)
    for i = 0 to Array.length fft_input - 1 do
      let c = fft_input.(i) in
      fft_input.(i) <- Fft.complex c.re (-.c.im)
    done;
    (* Apply FFT *)
    let rec log2 x acc = if x <= 1 then acc else log2 (x lsr 1) (acc + 1) in
    let nbits = log2 n_padded 0 in
    match Fft.fft_init nbits false with
    | None -> idft_transform_reference data
    | Some fft_ctx ->
      Fft.fft_calc fft_ctx fft_input;
      (* Conjugate output and scale *)
      List.map (fun i ->
        if i < n then
          fft_input.(i).re /. float_of_int n_padded
        else 0.0
      ) (List.init n Fun.id)

(* FFT Shift: Shift zero frequency to center *)
let fft_shift data =
  let n = List.length data in
  let mid = n / 2 in
  let first_half = List.init mid (fun i -> List.nth data i) in
  let second_half = List.init (n - mid) (fun i -> List.nth data (mid + i)) in
  second_half @ first_half

(* Window functions *)

(* Hamming window *)
let hamming_window data =
  let n = List.length data in
  if n <= 1 then data
  else
    List.mapi
      (fun i x ->
        let w = 0.54 -. 0.46 *. Float.cos (2. *. Float.pi *. float_of_int i /. float_of_int (n - 1)) in
        x *. w)
      data

(* Hanning window *)
let hanning_window data =
  let n = List.length data in
  if n <= 1 then data
  else
    List.mapi
      (fun i x ->
        let w = 0.5 *. (1. -. Float.cos (2. *. Float.pi *. float_of_int i /. float_of_int (n - 1))) in
        x *. w)
      data

(* Rectangular window (no modification) *)
let rectangular_window data = data

(* Filter Bank: Split signal into multiple frequency bands *)
(* Uses DFT to transform to frequency domain, masks bands, then IDFT back *)
let filter_bank data num_bands =
  let n = List.length data in
  if n = 0 || num_bands <= 0 then []
  else if num_bands = 1 then [data]
  else
    (* Transform to frequency domain *)
    let freq_domain = dft_transform data in
    let n_freq = List.length freq_domain / 2 in
    let band_size = n_freq / num_bands in
    (* Create each band *)
    let rec create_band band_idx =
      if band_idx = num_bands then []
      else
        let start_freq = band_idx * band_size in
        let end_freq = if band_idx = num_bands - 1 then n_freq else (band_idx + 1) * band_size in
        (* Create masked frequency domain: zero out frequencies outside this band *)
        let masked_freq = List.mapi
          (fun i x ->
            let freq_idx = i / 2 in
            if freq_idx >= start_freq && freq_idx < end_freq then x
            else 0.0)
          freq_domain in
        (* Transform back to time domain *)
        let band_signal = idft_transform masked_freq in
        band_signal :: create_band (band_idx + 1)
    in
    create_band 0

(* Filter Bank with custom frequency ranges *)
(* bands is a list of (start_freq, end_freq) pairs (normalized 0.0-1.0) *)
let filter_bank_custom data bands =
  let n = List.length data in
  if n = 0 || List.length bands = 0 then []
  else
    (* Transform to frequency domain *)
    let freq_domain = dft_transform data in
    let n_freq = List.length freq_domain / 2 in
    (* Create each band *)
    List.map
      (fun (start_norm, end_norm) ->
        let start_freq = int_of_float (start_norm *. float_of_int n_freq) in
        let end_freq = int_of_float (end_norm *. float_of_int n_freq) in
        (* Create masked frequency domain *)
        let masked_freq = List.mapi
          (fun i x ->
            let freq_idx = i / 2 in
            if freq_idx >= start_freq && freq_idx < end_freq then x
            else 0.0)
          freq_domain in
        (* Transform back to time domain *)
        idft_transform masked_freq)
      bands

(* SNR: Signal-to-Noise Ratio in dB *)
(* Calculates SNR between original signal and noisy/reconstructed signal *)
let snr original noisy =
  let n = List.length original in
  if n = 0 || n <> List.length noisy then 0.0
  else
    (* Optimize: calculate both signal_power and noise_power in a single pass *)
    (* This avoids traversing the lists twice *)
    let (signal_power, noise_power) = List.fold_left2
      (fun (sig_acc, noise_acc) orig nois -> 
        let diff = orig -. nois in
        (sig_acc +. orig *. orig, noise_acc +. diff *. diff))
      (0.0, 0.0)
      original noisy in
    if noise_power = 0.0 then infinity
    else if signal_power = 0.0 then neg_infinity
    else
      (* SNR in dB: 10 * log10(signal_power / noise_power) *)
      10.0 *. Float.log10 (signal_power /. noise_power)

(* Haar Wavelet Transform *)
(* Returns (approximations, details) where approximations are low-frequency components *)
(* and details are high-frequency components at each level *)
let haar_transform data =
  let n = List.length data in
  if n = 0 then ([], [])
  else if n = 1 then (data, [])
  else
    let rec haar_aux signal all_details =
      let len = List.length signal in
      if len < 2 then (signal, all_details)
      else
        (* Process pairs: (x[0], x[1]), (x[2], x[3]), ... *)
        let rec process_pairs acc_appr acc_detail = function
          | [] -> (List.rev acc_appr, List.rev acc_detail)
          | [x] -> (List.rev (x :: acc_appr), List.rev acc_detail)
          | x :: y :: rest ->
            let appr = (x +. y) /. 2.0 in
            let detail = (x -. y) /. 2.0 in
            process_pairs (appr :: acc_appr) (detail :: acc_detail) rest
        in
        let (approximations, details) = process_pairs [] [] signal in
        (* Recursively transform approximations *)
        haar_aux approximations (details @ all_details)
    in
    haar_aux data []

(* Inverse Haar Wavelet Transform *)
(* Reconstructs signal from (approximations, details) *)
(* Details are in order: [d_n, d_{n-1}, ..., d_1] where d_n is deepest level *)
let haar_inverse_transform approximations details =
  let rec haar_inverse_aux appr detail_list =
    let appr_len = List.length appr in
    if appr_len = 0 then []
    else if appr_len = 1 && List.length detail_list = 0 then appr
    else
      (* Get details for current level *)
      (* Details are stored as [deepest, ..., shallowest] *)
      (* We need to take the first appr_len details (deepest level) *)
      let current_level_size = appr_len in
      let rec split_list n acc = function
        | [] -> (List.rev acc, [])
        | x :: rest when n > 0 -> split_list (n - 1) (x :: acc) rest
        | rest -> (List.rev acc, rest)
      in
      let (current_details, remaining_details) = 
        if List.length detail_list >= current_level_size then
          split_list current_level_size [] detail_list
        else
          (detail_list, [])
      in
      (* Reconstruct signal from approximations and details *)
      (* For each pair (a, d): reconstruct (a+d, a-d) *)
      let reconstructed = List.fold_right2
        (fun a d acc ->
          let first = a +. d in
          let second = a -. d in
          first :: second :: acc)
        appr current_details [] in
      let rec_reconstructed = reconstructed in
      (* If we have remaining details, continue inverse transform *)
      if List.length remaining_details > 0 then
        haar_inverse_aux rec_reconstructed remaining_details
      else
        rec_reconstructed
  in
  haar_inverse_aux approximations details

(* Discrete Wavelet Transform using Haar wavelets *)
(* Returns full decomposition: [a_n, d_n, d_{n-1}, ..., d_1] *)
(* where a_n is the final approximation and d_i are detail coefficients at level i *)
let dwt_haar data =
  let (approximations, details) = haar_transform data in
  approximations @ details

(* Inverse Discrete Wavelet Transform using Haar wavelets *)
(* Reconstructs signal from DWT coefficients *)
(* Assumes coefficients are in format: [a_n, d_n, d_{n-1}, ..., d_1] *)
let idwt_haar coefficients =
  let n = List.length coefficients in
  if n = 0 then []
  else
    (* For full decomposition, approximation length is always 1 for n >= 1 *)
    let find_appr_len len = if len = 0 then 0 else 1 in
    let appr_len = find_appr_len n in
    let approximations = List.init appr_len (fun i -> List.nth coefficients i) in
    let details = List.init (n - appr_len) (fun i -> List.nth coefficients (appr_len + i)) in
    haar_inverse_transform approximations details
