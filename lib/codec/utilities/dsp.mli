(* Digital Signal Processing utilities interface *)

val dft_transform : float list -> float list
val idft_transform : float list -> float list
val mdct_transform : float list -> float list
val imdct_transform : float list -> float list
(* Reference implementations for testing (O(n²)) *)
val mdct_transform_reference : float list -> float list
val imdct_transform_reference : float list -> float list
val fft_shift : float list -> float list
val hamming_window : float list -> float list
val hanning_window : float list -> float list
val rectangular_window : float list -> float list
val filter_bank : float list -> int -> float list list
val filter_bank_custom : float list -> (float * float) list -> float list list
val snr : float list -> float list -> float
val haar_transform : float list -> float list * float list
val haar_inverse_transform : float list -> float list -> float list
val dwt_haar : float list -> float list
val idwt_haar : float list -> float list
