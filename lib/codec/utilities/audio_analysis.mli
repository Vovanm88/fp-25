(* Audio analysis utilities *)

(* Calculate RMS (Root Mean Square) for a window *)
val calculate_rms : float list -> float

(* Calculate envelope (RMS over windows) *)
val calculate_envelope : float list -> int -> float list

(* Calculate attack and release times from envelope *)
(* Returns: (attack_ms, release_ms) *)
val calculate_attack_release : float list -> int -> int -> float * float

