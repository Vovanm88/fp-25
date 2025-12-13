---
name: Audio Model Implementation (Updated)
overview: Add efficient quantization encoding using ceil(log2(n)) bit-width calculation, create audio model module with split polybank filter (function + frequency bands constant), and implement incrementally with tests after each component.
todos:
  - id: quantization_encoding
    content: Add encode_quantized_indices and decode_quantized_indices to quantization.ml using ceil(log2(n)) for bit-width calculation
    status: pending
  - id: quantization_interface
    content: Update quantization.mli to export encoding/decoding functions
    status: pending
    dependencies:
      - quantization_encoding
  - id: quantization_tests
    content: Add tests for encode/decode roundtrip and bit-width calculation in test_quantization.ml
    status: pending
    dependencies:
      - quantization_encoding
  - id: test_quantization_phase
    content: Run quantization encoding tests to verify implementation
    status: pending
    dependencies:
      - quantization_tests
  - id: audiomodel_types
    content: Define data structures (window_type, segment, track, audio_file) in audiomodel.ml
    status: pending
  - id: audiomodel_types_tests
    content: Add basic tests for data structure creation in test_audiomodel.ml
    status: pending
    dependencies:
      - audiomodel_types
  - id: test_types_phase
    content: Run data structure tests
    status: pending
    dependencies:
      - audiomodel_types_tests
  - id: polybank_constant
    content: Define standard_10band_frequencies constant with frequency band boundaries
    status: pending
    dependencies:
      - audiomodel_types
  - id: polybank_filter
    content: Implement polybank_filter function that takes signal and frequency bands list, uses Dsp.filter_bank_custom
    status: pending
    dependencies:
      - polybank_constant
  - id: polybank_tests
    content: Add tests for polybank_filter with standard and custom bands
    status: pending
    dependencies:
      - polybank_filter
  - id: test_polybank_phase
    content: Run polybank filter tests
    status: pending
    dependencies:
      - polybank_tests
  - id: window_fsm
    content: Implement determine_window_type FSM with attack/release thresholds
    status: pending
    dependencies:
      - audiomodel_types
  - id: window_fsm_tests
    content: Add tests covering all FSM transitions
    status: pending
    dependencies:
      - window_fsm
  - id: test_window_phase
    content: Run window FSM tests
    status: pending
    dependencies:
      - window_fsm_tests
  - id: adaptive_quantization
    content: Implement find_optimal_quantization with binary search for SNR threshold
    status: pending
    dependencies:
      - audiomodel_types
  - id: adaptive_quant_tests
    content: Add tests for binary search and SNR threshold maintenance
    status: pending
    dependencies:
      - adaptive_quantization
  - id: test_adaptive_phase
    content: Run adaptive quantization tests
    status: pending
    dependencies:
      - adaptive_quant_tests
  - id: compression
    content: Implement compress_track and decompress_track for linear range transformation
    status: pending
    dependencies:
      - audiomodel_types
  - id: compression_tests
    content: Add roundtrip tests for compression/decompression
    status: pending
    dependencies:
      - compression
  - id: test_compression_phase
    content: Run compression tests
    status: pending
    dependencies:
      - compression_tests
  - id: channel_transform
    content: Implement lr_to_mid_side and mid_side_to_lr for stereo transformations
    status: pending
    dependencies:
      - audiomodel_types
  - id: channel_tests
    content: Add roundtrip tests for channel transformations
    status: pending
    dependencies:
      - channel_transform
  - id: test_channel_phase
    content: Run channel transformation tests
    status: pending
    dependencies:
      - channel_tests
  - id: audiomodel_interface
    content: Create audiomodel.mli with all public type and function signatures
    status: pending
    dependencies:
      - polybank_filter
      - window_fsm
      - adaptive_quantization
      - compression
      - channel_transform
  - id: update_dune
    content: Update lib/codec/dune to include audiomodel module and create test/codec/dune if needed
    status: pending
    dependencies:
      - audiomodel_interface
---

# Audio Model Implementation Plan (Updated)

## Overview

This plan adds efficient variable bit-width encoding to the quantization module (using ceil(log2(n)) for bit-width) and creates a new `audiomodel.ml` module. Implementation will be incremental: code → tests → test → next component.

## Implementation Details

### Phase 1: Quantization Encoding (First - Test Immediately)

**File**: [`lib/codec/utilities/quantization.ml`](lib/codec/utilities/quantization.ml)

Add function `encode_quantized_indices : int list -> int -> bytes` that:

- Takes a list of quantized indices (0 to n-1) and n (number of quantization levels)
- Calculates bit-width as `ceil(log2(n))` (minimum 1 bit, maximum 32 bits)
- Packs elements into bytes efficiently (e.g., `[1, 0, 2, 3]` with n=4 → `0b01001011`)
- Returns packed bytes

Add function `decode_quantized_indices : bytes -> int -> int -> int list` that:

- Takes bytes, number of elements, and n (quantization levels)
- Calculates bit-width as `ceil(log2(n))`
- Unpacks bits from bytes back to list of indices

**File**: [`lib/codec/utilities/quantization.mli`](lib/codec/utilities/quantization.mli)

Add signatures for the new encoding/decoding functions.

**File**: [`test/codec/utilities/test_quantization.ml`](test/codec/utilities/test_quantization.ml)

Add tests for encode/decode roundtrip, edge cases (n=1, n=2, n=3, n=4, n=5, n=32, n=256, etc.), and verify bit-width calculation.

**Test**: Run tests immediately after implementation.

### Phase 2: Audio Model Data Structures

**File**: [`lib/codec/audiomodel.ml`](lib/codec/audiomodel.ml) (new file)

Define data structures:

- **Window type**: `type window_type = Short | Long | VeryLong | EndFocused | StartFocused`
- **Segment**: Record with window_type, num_bands, frequency_bands, band_ranges, quantization_levels, quantized_data (bytes list option), raw_data (float list list option)
- **Track**: Record with num_segments, segments, length_samples, sample_rate, compression_params
- **AudioFile**: Record with num_tracks, tracks, bits_per_sample, sample_rate

**File**: [`test/codec/test_audiomodel.ml`](test/codec/test_audiomodel.ml) (new file)

Add basic tests for data structure creation and field access.

**Test**: Run tests immediately after implementation.

### Phase 3: Polybank Filter (Split Design)

**File**: [`lib/codec/audiomodel.ml`](lib/codec/audiomodel.ml)

1. Define constant `standard_10band_frequencies : (float * float) list` containing:

- Frequency band boundaries: 20Hz, 60Hz, 170Hz, 310Hz, 600Hz, 1kHz, 3kHz, 6kHz, 12kHz, 14kHz, 20kHz
- Stored as normalized pairs (0.0-1.0) or as Hz values (to be normalized based on sample_rate)

2. Function `polybank_filter : float list -> (float * float) list -> float list list` that:

- Takes signal and list of frequency band boundaries
- Normalizes frequencies to 0.0-1.0 range based on signal length/sample rate
- Uses `Dsp.filter_bank_custom` to split signal into bands
- Returns list of band signals

**File**: [`test/codec/test_audiomodel.ml`](test/codec/test_audiomodel.ml)

Add tests for polybank_filter with standard bands, custom bands, edge cases (empty signal, single band, etc.).

**Test**: Run tests immediately after implementation.

### Phase 4: Window Type FSM

**File**: [`lib/codec/audiomodel.ml`](lib/codec/audiomodel.ml)

Function `determine_window_type : float -> float -> window_type` that:

- Takes attack time (ms) and release time (ms)
- Uses standard MP3/AAC-like thresholds:
- Short: attack < 5ms OR release < 5ms
- EndFocused: attack < 5ms AND release > 20ms
- StartFocused: attack > 20ms AND release < 5ms
- VeryLong: attack > 50ms AND release > 50ms
- Long: default (all other cases)
- Returns appropriate window type

**File**: [`test/codec/test_audiomodel.ml`](test/codec/test_audiomodel.ml)

Add tests covering all FSM transitions and edge cases.

**Test**: Run tests immediately after implementation.

### Phase 5: Adaptive Quantization Module

**File**: [`lib/codec/audiomodel.ml`](lib/codec/audiomodel.ml)

Function `find_optimal_quantization : float list -> float -> (int * float * float)` that:

- Takes signal data and SNR threshold (default 40.0 dB)
- Uses binary search to find minimum quantization levels (n) that maintains SNR ≥ threshold
- Searches n in range [2, 256] (or up to 65536 if needed)
- Returns (n, min_val, max_val) tuple
- Uses `Quantization.quantize_with_range` and `Dsp.snr` for evaluation

**File**: [`test/codec/test_audiomodel.ml`](test/codec/test_audiomodel.ml)

Add tests for binary search correctness, SNR threshold maintenance, edge cases.

**Test**: Run tests immediately after implementation.

### Phase 6: Track Compression

**File**: [`lib/codec/audiomodel.ml`](lib/codec/audiomodel.ml)

Function `compress_track : float list -> (float list * float * float)` that:

- Takes raw audio samples
- Performs linear transformation to use full range: `(x - min) / (max - min) * 2.0 - 1.0`
- Returns (compressed_samples, original_min, original_max) for decompression

Function `decompress_track : float list -> float -> float -> float list` (inverse)

**File**: [`test/codec/test_audiomodel.ml`](test/codec/test_audiomodel.ml)

Add roundtrip tests for compression/decompression.

**Test**: Run tests immediately after implementation.

### Phase 7: Channel Transformations

**File**: [`lib/codec/audiomodel.ml`](lib/codec/audiomodel.ml)

Function `lr_to_mid_side : float list -> float list -> (float list * float list)` that:

- Takes left and right channels
- Computes: `mid = (left + right) / 2`, `side = left - mid`
- Returns (mid, side)

Function `mid_side_to_lr : float list -> float list -> (float list * float list)` (inverse)

**File**: [`test/codec/test_audiomodel.ml`](test/codec/test_audiomodel.ml)

Add roundtrip tests for channel transformations.

**Test**: Run tests immediately after implementation.

### Phase 8: Interface and Build Configuration

**File**: [`lib/codec/audiomodel.mli`](lib/codec/audiomodel.mli) (new file)

Export all public types and functions with proper documentation.

**File**: [`lib/codec/dune`](lib/codec/dune)

Add `audiomodel` to the modules list: `(modules encoder decoder audiomodel)`

**File**: [`test/codec/dune`](test/codec/dune) (create if needed)

Add test_audiomodel test target.

## Implementation Notes

- Bit-width calculation: `bits = max(1, ceil(log2(n)))` where n is number of quantization levels
- Polybank filter is split: generic function + constant frequency bands list
- Each phase must be tested before moving to the next
- All inverse transformations are provided for complete roundtrip capability
- Data structures support both quantized and raw data storage (mutually exclusive)