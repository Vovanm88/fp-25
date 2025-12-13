(* Fast FFT/MDCT implementation based on FFmpeg *)
(* Copyright (c) 2002 Fabrice Bellard, 2008 Loren Merritt *)
(* O(n log n) complexity *)

(* Complex number type *)
type complex = { mutable re : float; mutable im : float }

let complex re im = { re; im }
let complex_zero () = { re = 0.0; im = 0.0 }

(* FFT Context *)
type fft_context = {
  nbits : int;
  n : int;
  inverse : bool;
  revtab : int array;
  cos_tab : float array;  (* Precomputed cosines *)
} [@@warning "-69"]

(* MDCT Context *)
type mdct_context = {
  fft : fft_context;
  mdct_bits : int;
  mdct_size : int;
  tcos : float array;
  tsin : float array;
} [@@warning "-69"]

(* Split-radix permutation from FFmpeg *)
let rec split_radix_permutation i n inverse =
  if n <= 2 then i land 1
  else
    let m = n lsr 1 in
    if (i land m) = 0 then
      (split_radix_permutation i m inverse) * 2
    else
      let m2 = m lsr 1 in
      if (if inverse then (i land m2) = 0 else (i land m2) <> 0) then
        (split_radix_permutation i m2 inverse) * 4 + 1
      else
        (split_radix_permutation i m2 inverse) * 4 - 1

(* Initialize FFT context *)
let fft_init nbits inverse =
  if nbits < 2 || nbits > 16 then None
  else
    let n = 1 lsl nbits in
    (* Build reverse table using split-radix permutation *)
    let revtab = Array.make n 0 in
    for i = 0 to n - 1 do
      let j = (-(split_radix_permutation i n inverse)) land (n - 1) in
      revtab.(j) <- i
    done;
    (* Build cosine table for all needed sizes *)
    let cos_tab_size = n / 2 in
    let cos_tab = Array.make cos_tab_size 0.0 in
    for i = 0 to cos_tab_size - 1 do
      cos_tab.(i) <- Float.cos (2.0 *. Float.pi *. float_of_int i /. float_of_int n)
    done;
    Some { nbits; n; inverse; revtab; cos_tab }

(* BF macro from FFmpeg *)
let bf a b =
  let x = a -. b in
  let y = a +. b in
  (x, y)

(* FFT-4 from FFmpeg *)
let fft4 z off =
  let t3, t1 = bf z.(off + 0).re z.(off + 1).re in
  let t8, t6 = bf z.(off + 3).re z.(off + 2).re in
  let z2re, z0re = bf t1 t6 in
  let t4, t2 = bf z.(off + 0).im z.(off + 1).im in
  let t7, t5 = bf z.(off + 2).im z.(off + 3).im in
  let z3im, z1im = bf t4 t8 in
  let z3re, z1re = bf t3 t7 in
  let z2im, z0im = bf t2 t5 in
  z.(off + 0).re <- z0re;
  z.(off + 0).im <- z0im;
  z.(off + 1).re <- z1re;
  z.(off + 1).im <- z1im;
  z.(off + 2).re <- z2re;
  z.(off + 2).im <- z2im;
  z.(off + 3).re <- z3re;
  z.(off + 3).im <- z3im

(* TRANSFORM macro from FFmpeg *)
let transform z a0 a1 a2 a3 wre wim =
  let t1 = z.(a2).re *. wre +. z.(a2).im *. wim in
  let t2 = z.(a2).im *. wre -. z.(a2).re *. wim in
  let t5 = z.(a3).re *. wre -. z.(a3).im *. wim in
  let t6 = z.(a3).im *. wre +. z.(a3).re *. wim in
  (* BUTTERFLIES *)
  let t3, t5' = bf t5 t1 in
  let a2re, a0re = bf z.(a0).re t5' in
  let a3im, a1im = bf z.(a1).im t3 in
  let t4, t6' = bf t2 t6 in
  let a3re, a1re = bf z.(a1).re t4 in
  let a2im, a0im = bf z.(a0).im t6' in
  z.(a0).re <- a0re;
  z.(a0).im <- a0im;
  z.(a1).re <- a1re;
  z.(a1).im <- a1im;
  z.(a2).re <- a2re;
  z.(a2).im <- a2im;
  z.(a3).re <- a3re;
  z.(a3).im <- a3im

(* TRANSFORM_ZERO macro from FFmpeg *)
let transform_zero z a0 a1 a2 a3 =
  let t1 = z.(a2).re in
  let t2 = z.(a2).im in
  let t5 = z.(a3).re in
  let t6 = z.(a3).im in
  (* BUTTERFLIES *)
  let t3, t5' = bf t5 t1 in
  let a2re, a0re = bf z.(a0).re t5' in
  let a3im, a1im = bf z.(a1).im t3 in
  let t4, t6' = bf t2 t6 in
  let a3re, a1re = bf z.(a1).re t4 in
  let a2im, a0im = bf z.(a0).im t6' in
  z.(a0).re <- a0re;
  z.(a0).im <- a0im;
  z.(a1).re <- a1re;
  z.(a1).im <- a1im;
  z.(a2).re <- a2re;
  z.(a2).im <- a2im;
  z.(a3).re <- a3re;
  z.(a3).im <- a3im

let sqrthalf = Float.sqrt 0.5

(* FFT-8 from FFmpeg *)
let fft8 z off =
  fft4 z off;
  let t1, z5re = bf z.(off + 4).re (-.z.(off + 5).re) in
  let t2, z5im = bf z.(off + 4).im (-.z.(off + 5).im) in
  let t3, z7re = bf z.(off + 6).re (-.z.(off + 7).re) in
  let t4, z7im = bf z.(off + 6).im (-.z.(off + 7).im) in
  z.(off + 5).re <- z5re;
  z.(off + 5).im <- z5im;
  z.(off + 7).re <- z7re;
  z.(off + 7).im <- z7im;
  let t8, t1' = bf t3 t1 in
  let t7, t2' = bf t2 t4 in
  let z4re, z0re = bf z.(off + 0).re t1' in
  let z4im, z0im = bf z.(off + 0).im t2' in
  let z6re, z2re = bf z.(off + 2).re t7 in
  let z6im, z2im = bf z.(off + 2).im t8 in
  z.(off + 0).re <- z0re;
  z.(off + 0).im <- z0im;
  z.(off + 2).re <- z2re;
  z.(off + 2).im <- z2im;
  z.(off + 4).re <- z4re;
  z.(off + 4).im <- z4im;
  z.(off + 6).re <- z6re;
  z.(off + 6).im <- z6im;
  transform z (off + 1) (off + 3) (off + 5) (off + 7) sqrthalf sqrthalf

(* Pass function from FFmpeg *)
let pass z off cos_tab n =
  let o1 = 2 * n in
  let o2 = 4 * n in
  let o3 = 6 * n in
  transform_zero z (off + 0) (off + o1) (off + o2) (off + o3);
  if n > 1 then (
    transform z (off + 1) (off + o1 + 1) (off + o2 + 1) (off + o3 + 1) 
      cos_tab.(1) cos_tab.(o1 - 1);
    let rec loop i wre_idx wim_idx =
      if i < n then (
        transform z (off + i) (off + o1 + i) (off + o2 + i) (off + o3 + i)
          cos_tab.(wre_idx) cos_tab.(wim_idx);
        transform z (off + i + 1) (off + o1 + i + 1) (off + o2 + i + 1) (off + o3 + i + 1)
          cos_tab.(wre_idx + 1) cos_tab.(wim_idx - 1);
        loop (i + 2) (wre_idx + 2) (wim_idx - 2)
      )
    in
    loop 2 2 (o1 - 2)
  )

(* Recursive FFT *)
let rec fft_rec z off cos_tab n =
  if n = 4 then fft4 z off
  else if n = 8 then fft8 z off
  else (
    let n2 = n / 2 in
    let n4 = n / 4 in
    fft_rec z off cos_tab n2;
    fft_rec z (off + n4 * 2) cos_tab n4;
    fft_rec z (off + n4 * 3) cos_tab n4;
    pass z off cos_tab (n4 / 2)
  )

(* FFT permute *)
let fft_permute ctx z =
  let tmp = Array.init ctx.n (fun _ -> complex_zero ()) in
  for j = 0 to ctx.n - 1 do
    tmp.(ctx.revtab.(j)) <- z.(j)
  done;
  for j = 0 to ctx.n - 1 do
    z.(j) <- tmp.(j)
  done

(* Main FFT calculation *)
let fft_calc ctx z =
  fft_permute ctx z;
  fft_rec z 0 ctx.cos_tab ctx.n

(* Initialize MDCT context *)
let mdct_init nbits inverse scale =
  let n = 1 lsl nbits in
  let n4 = n lsr 2 in
  let fft_nbits = nbits - 2 in
  if fft_nbits < 2 then None
  else
    match fft_init fft_nbits inverse with
    | None -> None
    | Some fft ->
      let tcos = Array.make n4 0.0 in
      let tsin = Array.make n4 0.0 in
      let theta = 1.0 /. 8.0 +. (if scale < 0.0 then float_of_int n4 else 0.0) in
      let scale_factor = Float.sqrt (Float.abs scale) in
      for i = 0 to n4 - 1 do
        let alpha = 2.0 *. Float.pi *. (float_of_int i +. theta) /. float_of_int n in
        tcos.(i) <- -.Float.cos alpha *. scale_factor;
        tsin.(i) <- -.Float.sin alpha *. scale_factor
      done;
      Some { fft; mdct_bits = nbits; mdct_size = n; tcos; tsin }

(* CMUL macro from FFmpeg: (pre, pim) = (are, aim) * (bre, bim) *)
let cmul are aim bre bim =
  let pre = are *. bre -. aim *. bim in
  let pim = are *. bim +. aim *. bre in
  (pre, pim)

(* Forward MDCT *)
let mdct_calc ctx input =
  let n = ctx.mdct_size in
  let n2 = n lsr 1 in
  let n4 = n lsr 2 in
  let n8 = n lsr 3 in
  let n3 = 3 * n4 in
  let tcos = ctx.tcos in
  let tsin = ctx.tsin in
  let x = Array.init ctx.fft.n (fun _ -> complex_zero ()) in
  (* Pre-rotation *)
  for i = 0 to n8 - 1 do
    let re = -.input.(2 * i + n3) -. input.(n3 - 1 - 2 * i) in
    let im = -.input.(n4 + 2 * i) +. input.(n4 - 1 - 2 * i) in
    let j = ctx.fft.revtab.(i) in
    let xre, xim = cmul re im (-.tcos.(i)) tsin.(i) in
    x.(j).re <- xre;
    x.(j).im <- xim;
    
    let re2 = input.(2 * i) -. input.(n2 - 1 - 2 * i) in
    let im2 = -.(input.(n2 + 2 * i) +. input.(n - 1 - 2 * i)) in
    let j2 = ctx.fft.revtab.(n8 + i) in
    let xre2, xim2 = cmul re2 im2 (-.tcos.(n8 + i)) tsin.(n8 + i) in
    x.(j2).re <- xre2;
    x.(j2).im <- xim2
  done;
  (* FFT *)
  fft_rec x 0 ctx.fft.cos_tab ctx.fft.n;
  (* Post-rotation *)
  for i = 0 to n8 - 1 do
    let i1, r0 = cmul x.(n8 - i - 1).re x.(n8 - i - 1).im 
                   (-.tsin.(n8 - i - 1)) (-.tcos.(n8 - i - 1)) in
    let i0, r1 = cmul x.(n8 + i).re x.(n8 + i).im 
                   (-.tsin.(n8 + i)) (-.tcos.(n8 + i)) in
    x.(n8 - i - 1).re <- r0;
    x.(n8 - i - 1).im <- i0;
    x.(n8 + i).re <- r1;
    x.(n8 + i).im <- i1
  done;
  (* Extract output: n/2 coefficients from n/4 complex values *)
  let output = Array.make n2 0.0 in
  for i = 0 to n4 - 1 do
    output.(2 * i) <- x.(i).re;
    output.(2 * i + 1) <- x.(i).im
  done;
  output

(* Inverse MDCT half *)
let imdct_half ctx input =
  let n = ctx.mdct_size in
  let n2 = n lsr 1 in
  let n4 = n lsr 2 in
  let n8 = n lsr 3 in
  let tcos = ctx.tcos in
  let tsin = ctx.tsin in
  let z = Array.init ctx.fft.n (fun _ -> complex_zero ()) in
  (* Pre-rotation *)
  let in1 = ref 0 in
  let in2 = ref (n2 - 1) in
  for k = 0 to n4 - 1 do
    let j = ctx.fft.revtab.(k) in
    let zre, zim = cmul input.(!in2) input.(!in1) tcos.(k) tsin.(k) in
    z.(j).re <- zre;
    z.(j).im <- zim;
    in1 := !in1 + 2;
    in2 := !in2 - 2
  done;
  (* FFT *)
  fft_rec z 0 ctx.fft.cos_tab ctx.fft.n;
  (* Post-rotation + reordering *)
  for k = 0 to n8 - 1 do
    let r0, i1 = cmul z.(n8 - k - 1).im z.(n8 - k - 1).re 
                   tsin.(n8 - k - 1) tcos.(n8 - k - 1) in
    let r1, i0 = cmul z.(n8 + k).im z.(n8 + k).re 
                   tsin.(n8 + k) tcos.(n8 + k) in
    z.(n8 - k - 1).re <- r0;
    z.(n8 - k - 1).im <- i0;
    z.(n8 + k).re <- r1;
    z.(n8 + k).im <- i1
  done;
  (* Return output: n/2 samples *)
  let output = Array.make n2 0.0 in
  for i = 0 to n4 - 1 do
    output.(2 * i) <- z.(i).re;
    output.(2 * i + 1) <- z.(i).im
  done;
  output

(* Full IMDCT *)
let imdct_calc ctx input =
  let n = ctx.mdct_size in
  let n2 = n lsr 1 in
  let n4 = n lsr 2 in
  let half = imdct_half ctx input in
  let output = Array.make n 0.0 in
  (* Copy first half *)
  for i = 0 to n2 - 1 do
    output.(n4 + i) <- half.(i)
  done;
  (* Mirror *)
  for k = 0 to n4 - 1 do
    output.(k) <- -.output.(n2 - k - 1);
    output.(n - k - 1) <- output.(n2 + k)
  done;
  output

(* Convenience wrappers for list interface *)
let mdct_transform_fast data =
  let n = List.length data in
  if n = 0 then []
  else
    let rec find_nbits n acc =
      if n <= 1 then acc
      else find_nbits (n lsr 1) (acc + 1)
    in
    let nbits = find_nbits n 0 in
    if (1 lsl nbits) <> n || nbits < 4 then
      (* Fall back to reference for non-power-of-2 or too small *)
      []
    else
      match mdct_init nbits false 1.0 with
      | None -> []
      | Some ctx ->
        let input = Array.of_list data in
        let output = mdct_calc ctx input in
        Array.to_list output

let imdct_transform_fast data =
  let m = List.length data in
  if m = 0 then []
  else
    let n = 2 * m in
    let rec find_nbits n acc =
      if n <= 1 then acc
      else find_nbits (n lsr 1) (acc + 1)
    in
    let nbits = find_nbits n 0 in
    if (1 lsl nbits) <> n || nbits < 4 then
      []
    else
      match mdct_init nbits true (1.0 /. float_of_int m) with
      | None -> []
      | Some ctx ->
        let input = Array.of_list data in
        let output = imdct_calc ctx input in
        Array.to_list output

