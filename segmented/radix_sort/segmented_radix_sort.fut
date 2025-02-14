import "lib/github.com/diku-dk/segmented/segmented"
import "util"
import "lib/github.com/diku-dk/sorts/radix_sort"

local
def radix_sort_step [n] [k] 't
                    (get_bit: i32 -> t -> i32)
                    (digit_n: i32)
                    (segment_offsets: [k]i32)
                    (segment_idxs: [n]i32)
                    (flags: [n]bool)
                    (xs: [n]t) : [n]t =
  let bits = map (get_bit digit_n) xs
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

  in scatter (copy xs) (map i64.i32 idxs_seg) xs

def segmented_radix_sort [n] [k] 't
                         (num_bits: i32)
                         (get_bit: i32 -> t -> i32)
                         (segments: [k]i32)
                         (xs: [n]t) : [n]t =
  let sum_segs = reduce (+) 0 segments |> i64.i32
  let flags = mkFlagArray segments false true n
  let seg_offsets = exclusive_scan (+) 0 segments
  let segment_idxs = replicated_iota flags
  in if n == 0 || k == 0
     then xs
     else if n != sum_segs
     then xs
     else loop xs for i < num_bits do
            radix_sort_step get_bit i seg_offsets segment_idxs flags xs



def segmented_radix_sort_naive [n] [k]
                         (num_bits: i32)
                         (get_bit: i32 -> i32 -> i32)
                         (segments: [k]i32)
                         (xs: [n]i32) : [n]i32 =
  let flags = mkFlagArray segments false true n
  let segment_idxs = replicated_iota flags
  let zipped = zip segment_idxs xs
  let (sorted_segment_idxs, sorted_data) = radix_sort_int_by_key (.1) num_bits get_bit zipped |> unzip
  let segment_offsets = exclusive_scan (+) 0 segments
  -- HOW TO MAKE THE FOLLOWING PARALLEL ? maybee implement segments offsets in Radix sort steps! ?
  -- naive sequential scan could potential be parallel scan ? 
  let (write, _) =
    loop (write_idxs, segment_offsets) = (replicate n 0, copy segment_offsets)
    for i < length segment_idxs do
      let segment_idx = sorted_segment_idxs[i]
      in ( write_idxs with [i] = segment_offsets[segment_idx]
         , segment_offsets with [segment_idx] = segment_offsets[segment_idx] + 1
         )
  let write = map i64.i32 write
  in scatter (copy xs) write sorted_data
