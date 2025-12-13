#!/bin/bash
# Profile with ocamlcp (bytecode profiler)

set -e

echo "Building with profiling instrumentation..."

# Build bytecode with profiling
cd /home/vova/itmo/fp/labs/fp-25

# Create a simple test program
cat > /tmp/profile_test.ml << 'EOF'
(* Simple profiling test *)
module Encoder = Codec.Encoder
module Decoder = Codec.Decoder

let () =
  Printf.printf "Generating 15000 samples...\n";
  flush stdout;
  let samples = List.init 15000 (fun i -> Float.sin (float_of_int i *. 0.1)) in
  let sample_rate = 44100 in
  
  Printf.printf "Encoding...\n";
  flush stdout;
  let temp_file = Filename.temp_file "profile" ".audc" in
  let start = Unix.gettimeofday () in
  let _audio_file = Encoder.encode_to_file samples sample_rate temp_file in
  Printf.printf "Encode: %.3f sec\n" (Unix.gettimeofday () -. start);
  
  Printf.printf "Decoding...\n";
  flush stdout;
  let start = Unix.gettimeofday () in
  let (_reconstructed, _) = Decoder.decode_from_file temp_file in
  Printf.printf "Decode: %.3f sec\n" (Unix.gettimeofday () -. start);
  
  Sys.remove temp_file;
  Printf.printf "Done! Check ocamlprof.dump for profiling data\n"
EOF

# Compile with profiling
echo "Compiling with ocamlcp..."
ocamlfind ocamlcp -package unix -linkpkg \
  -I _build/default/lib/codec/.codec.objs/byte \
  -I _build/default/lib/codec/utilities/.codec_utilities.objs/byte \
  -I _build/default/lib/wav/.wav.objs/byte \
  -I _build/default/lib/io/.io.objs/byte \
  _build/default/lib/io/.io.objs/byte/io.cma \
  _build/default/lib/wav/.wav.objs/byte/wav.cma \
  _build/default/lib/codec/utilities/.codec_utilities.objs/byte/codec_utilities.cma \
  _build/default/lib/codec/.codec.objs/byte/codec.cma \
  -o /tmp/profile_test \
  /tmp/profile_test.ml 2>&1 || echo "Failed to compile with ocamlcp, trying alternative..."

if [ -f /tmp/profile_test ]; then
  echo "Running profiled program..."
  /tmp/profile_test
  
  echo ""
  echo "Profiling results:"
  if [ -f ocamlprof.dump ]; then
    ocamlprof /tmp/profile_test.ml | head -100
  else
    echo "No ocamlprof.dump found"
  fi
else
  echo "Could not build with ocamlcp. Using perf instead..."
  dune build test/codec/profile_decoder.exe
  perf record -g dune exec test/codec/profile_decoder.exe 2>&1 || echo "perf not available"
  perf report 2>&1 | head -50 || echo "perf report failed"
fi
EOF
chmod +x /tmp/profile_with_ocamlcp.sh
/tmp/profile_with_ocamlcp.sh
