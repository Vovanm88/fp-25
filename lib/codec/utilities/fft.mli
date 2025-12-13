(* Fast FFT/MDCT implementation based on FFmpeg *)

(* Complex number type *)
type complex = { mutable re : float; mutable im : float }

(* FFT Context *)
type fft_context

(* MDCT Context *)
type mdct_context

(* Create complex number *)
val complex : float -> float -> complex
val complex_zero : unit -> complex

(* Initialize FFT context *)
(* nbits: log2 of FFT size (2-16) *)
(* inverse: true for IFFT *)
val fft_init : int -> bool -> fft_context option

(* Perform FFT in-place *)
val fft_calc : fft_context -> complex array -> unit

(* Initialize MDCT context *)
(* nbits: log2 of MDCT size (must be >= 4) *)
(* inverse: true for IMDCT *)
(* scale: scaling factor *)
val mdct_init : int -> bool -> float -> mdct_context option

(* Forward MDCT *)
(* input: n samples, output: n/2 coefficients *)
val mdct_calc : mdct_context -> float array -> float array

(* Inverse MDCT *)
(* input: n/2 coefficients, output: n samples *)
val imdct_calc : mdct_context -> float array -> float array

(* Inverse MDCT half *)
(* input: n/2 coefficients, output: n/2 samples (middle part) *)
val imdct_half : mdct_context -> float array -> float array

(* Convenience wrappers for list interface *)
val mdct_transform_fast : float list -> float list
val imdct_transform_fast : float list -> float list

