(* AudioFile serialization/deserialization interface *)

(* Serialize AudioFile to bytes *)
(* audio_file: AudioFile structure *)
(* Returns: list of bytes *)
val serialize_audio_file : Audiomodel.audio_file -> int list

(* Deserialize AudioFile from bytes *)
(* bytes: list of bytes *)
(* Returns: (audio_file, remaining_bytes) *)
val deserialize_audio_file : int list -> Audiomodel.audio_file * int list

(* Serialize AudioFile to file *)
(* filename: output file path *)
(* audio_file: AudioFile structure *)
val serialize_to_file : string -> Audiomodel.audio_file -> unit

(* Deserialize AudioFile from file *)
(* filename: input file path *)
(* Returns: AudioFile structure *)
val deserialize_from_file : string -> Audiomodel.audio_file

