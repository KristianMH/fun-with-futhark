import "lib/github.com/diku-dk/segmented/segmented"
import "util"
import "lib/github.com/diku-dk/sorts/radix_sort"

local
def pairwise op (a1, b1, c1, d1) (a2, b2, c2, d2) =
  (a1 `op` a2, b1 `op` b2, c1 `op` c2, d1 `op` d2)

-- steps "twice" e.g. two bits at a time
local
def radix_sort_step [n] 't
                    (get_bit: i32 -> t -> i32)
                    (digit_n: i32)
                    --  (segment_offsets: [k]i64)
                    --  (segment_idxs: [n]i32)
                    (segment_offsets: [n]i64)
                    -- [n]index of last entry within each segment
                    (segment_last_entries: [n]i64)
                    (segment_flags: [n]bool)
                    (xs: [n]t) : [n]t =
  let get_bit' x = get_bit (digit_n + 1) x * 2 + get_bit digit_n x
  let bins = map get_bit' xs
  let flags =
    map (\x ->
           ( i64.bool (x == 0)
           , i64.bool (x == 1)
           , i64.bool (x == 2)
           , i64.bool (x == 3)
           ))
        bins
  let offsets = segmented_scan (pairwise (+)) (0, 0, 0, 0) segment_flags flags
  let f bin (a, b, c, d) i seg_offset =
    let (na, nb, nc, _nd) = offsets[i]
    in (-1) + seg_offset
       + a * (i64.bool (bin == 0))
       + na * (i64.bool (bin > 0))
       + b * (i64.bool (bin == 1))
       + nb * (i64.bool (bin > 1))
       + c * (i64.bool (bin == 2))
       + nc * (i64.bool (bin > 2))
       + d * (i64.bool (bin == 3))
  let is = map4 f bins offsets segment_last_entries segment_offsets
  in scatter (copy xs) is xs

-- steps only once. Based on the original implementation of radix sort
local
def radix_sort_step_once [n] [k] 't
                         (get_bit: i32 -> t -> i32)
                         (digit_n: i32)
                         (segment_offsets: [k]i64)
                         (segment_idxs: [n]i32)
                         (flags: [n]bool)
                         (xs: [n]t) : [n]t =
  let bits = map (get_bit digit_n) xs |> map i64.i32
  let bits_neg = map (1 -) bits
  -- find the segment offsets for entries which do not have bit 'b' set
  -- uses segmented_reduce from utils instead of segmented library
  -- to fix compiler warning with size!
  let offs_seg_neg = segmented_reduce (+) 0 flags bits_neg k
  -- finds the offset for start of entries in the irregular array
  -- which do have bit 'b' set
  let offs_seg = map2 (+) segment_offsets offs_seg_neg
  -- find indicies of entries which do not have bit 'b' set within each segment
  let idxs0_seg_non_offset = segmented_scan (+) 0 flags bits_neg
  let idxs0_seg_non_flagged =
    map2 (\i x -> x + segment_offsets[i]) segment_idxs idxs0_seg_non_offset
  -- bits_neg is used a flag vector (0/1)* idxs
  let idxs0_seg = map2 (*) bits_neg idxs0_seg_non_flagged
  -- find indicies of entries which have bit 'b' set within each segment
  let idxs1_seg_non_offset = segmented_scan (+) 0 flags bits
  let idxs1_seg_non_flagged =
    map2 (\i x -> x + offs_seg[i]) segment_idxs idxs1_seg_non_offset
  let idxs1_seg = map2 (*) bits idxs1_seg_non_flagged
  -- merge the two sets of indicies, flag vector ensures non-overlapping
  let idxs2_seg = map2 (+) idxs0_seg idxs1_seg
  -- off by one correction
  let idxs_seg = map (\x -> x - 1) idxs2_seg
  -- scatter the array
  in scatter (copy xs) idxs_seg xs

local
def segmented_radix_sort_old [n] [k] 't
                             (num_bits: i32)
                             (get_bit: i32 -> t -> i32)
                             (segments: [k]i64)
                             (xs: [n]t) : [n]t =
  let sum_segs = reduce (+) 0 segments
  let flags = mkFlagArray segments false true n
  let seg_offsets = exclusive_scan (+) 0 segments
  let segment_idxs = replicated_iota flags
  in if n == 0 || k == 0
     then xs
     else if n != sum_segs
     then xs
     else loop xs for i < num_bits do
            radix_sort_step_once get_bit i seg_offsets segment_idxs flags xs

local
def segmented_radix_sort [n] [k] 't
                         (num_bits: i32)
                         (get_bit: i32 -> t -> i32)
                         (segments: [k]i64)
                         (xs: [n]t) : [n]t =
  let iters = if n == 0 then 0 else (num_bits + 2 - 1) / 2
  let flags = mkFlagArray segments false true n
  let segment_last_entry = scan (+) 0 segments |> map (\x -> x - 1)
  let seg_offsets = exclusive_scan (+) 0 segments
  let temp = replicated_iota flags
  let segment_offsets = map (\i -> seg_offsets[i]) temp
  let segment_last_entries = map (\i -> segment_last_entry[i]) temp
  in loop xs for i < iters do
       radix_sort_step get_bit (i * 2) segment_offsets segment_last_entries flags xs

-- simple version. does full radix sort, then an iteration to calculate write offsets
-- results in an additional scatter operation. Should use less memory though.
local
def segmented_radix_sort_naive 't [n] [k]
                               (num_bits: i32)
                               (get_bit: i32 -> t -> i32)
                               (segments: [k]i64)
                               (xs: [n]t) : [n]t =
  let flags = mkFlagArray segments false true n
  let segment_idxs = replicated_iota flags
  let zipped = zip segment_idxs xs
  let (sorted_segment_idxs, sorted_data) = radix_sort_by_key (.1) num_bits get_bit zipped |> unzip
  let segment_offsets = exclusive_scan (+) 0 segments
  let (write, _) =
    loop (write_idxs, segment_offsets) = (replicate n 0, copy segment_offsets)
    for i < length segment_idxs do
      let segment_idx = sorted_segment_idxs[i]
      in ( write_idxs with [i] = segment_offsets[segment_idx]
         , segment_offsets with [segment_idx] = segment_offsets[segment_idx] + 1
         )
  in scatter (copy xs) write sorted_data

def segmented_radix_sort_int 't [n] [k]
                             (num_bits: i32)
                             (get_bit: i32 -> t -> i32)
                             (segments: [k]i64)
                             (xs: [n]t) : [n]t =
  let get_bit' i x =
    -- Flip the most significant bit.
    let b = get_bit i x
    in if i == num_bits - 1 then b ^ 1 else b
  in segmented_radix_sort num_bits get_bit' segments xs

def segmented_radix_sort_int_old 't [n] [k]
                             (num_bits: i32)
                             (get_bit: i32 -> t -> i32)
                             (segments: [k]i64)
                             (xs: [n]t) : [n]t =
  let get_bit' i x =
    -- Flip the most significant bit.
    let b = get_bit i x
    in if i == num_bits - 1 then b ^ 1 else b
  in segmented_radix_sort_old num_bits get_bit' segments xs

def segmented_radix_sort_int_naive 't [n] [k]
                                   (num_bits: i32)
                                   (get_bit: i32 -> t -> i32)
                                   (segments: [k]i64)
                                   (xs: [n]t) : [n]t =
  let get_bit' i x =
    -- Flip the most significant bit.
    let b = get_bit i x
    in if i == num_bits - 1 then b ^ 1 else b
  in segmented_radix_sort_naive num_bits get_bit' segments xs

def segmented_radix_sort_float 't [n] [k]
                               (num_bits: i32)
                               (get_bit: i32 -> t -> i32)
                               (segments: [k]i64)
                               (xs: [n]t) : [n]t =
  let get_bit' i x =
    -- We flip the bit returned if:
    --
    -- 0) the most significant bit is set (this makes more negative
    --    numbers sort before less negative numbers), or
    --
    -- 1) we are asked for the most significant bit (this makes
    --    negative numbers sort before positive numbers).
    let b = get_bit i x
    in if get_bit (num_bits - 1) x == 1 || i == num_bits - 1
       then b ^ 1
       else b
  in segmented_radix_sort num_bits get_bit' segments xs

def segmented_radix_sort_float_old 't [n] [k]
                               (num_bits: i32)
                               (get_bit: i32 -> t -> i32)
                               (segments: [k]i64)
                               (xs: [n]t) : [n]t =
  let get_bit' i x =
    -- We flip the bit returned if:
    --
    -- 0) the most significant bit is set (this makes more negative
    --    numbers sort before less negative numbers), or
    --
    -- 1) we are asked for the most significant bit (this makes
    --    negative numbers sort before positive numbers).
    let b = get_bit i x
    in if get_bit (num_bits - 1) x == 1 || i == num_bits - 1
       then b ^ 1
       else b
  in segmented_radix_sort_old num_bits get_bit' segments xs

def segmented_radix_sort_float_naive 't [n] [k]
                                     (num_bits: i32)
                                     (get_bit: i32 -> t -> i32)
                                     (segments: [k]i64)
                                     (xs: [n]t) : [n]t =
  let get_bit' i x =
    -- We flip the bit returned if:
    --
    -- 0) the most significant bit is set (this makes more negative
    --    numbers sort before less negative numbers), or
    --
    -- 1) we are asked for the most significant bit (this makes
    --    negative numbers sort before positive numbers).
    let b = get_bit i x
    in if get_bit (num_bits - 1) x == 1 || i == num_bits - 1
       then b ^ 1
       else b
  in segmented_radix_sort_naive num_bits get_bit' segments xs
