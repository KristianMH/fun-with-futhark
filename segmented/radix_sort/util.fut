import "lib/github.com/diku-dk/segmented/segmented"

-- can also be implemented with branching function!
def exclusive_scan 'a [n] (op: a -> a -> a) (ne: a) (as: [n]a) : [n]a =
  let xs = scan op ne as |> rotate (-1)
  let xs[0] = ne
  in xs

-- makes segmented flag array from shape array
def mkFlagArray 't [m]
                (shp: [m]i32)
                (zero: t)
                (flag_val: t)
                (r: i64) : [r]t =
  let shp_ind = exclusive_scan (+) 0 shp |> map i64.i32
  let vals = replicate m flag_val
  in scatter (replicate r zero) shp_ind vals

-- | Segmented reduction. Similar to segment library but extended with size parameter
def segmented_reduce [n] 't
                     (op: t -> t -> t)
                     (ne: t)
                     (flags: [n]bool)
                     (as: [n]t)
                     (r: i64) : [r]t =
  -- Compute segmented scan.  Then we just have to fish out the end of
  -- each segment.
  let as' = segmented_scan op ne flags as
  -- Find the segment ends.
  let segment_ends = rotate 1 flags
  -- Find the offset for each segment end.
  let segment_end_offsets = segment_ends |> map i64.bool |> scan (+) 0
  -- let num_segments = if n > 0 then last segment_end_offsets else 0
  -- Make room for the final result.  The specific value we write here
  -- does not matter; they will all be overwritten by the segment
  -- ends.
  let scratch = replicate r ne
  -- Compute where to write each element of as'.  Only segment ends
  -- are written.
  let index i f = if f then i - 1 else -1
  in scatter scratch (map2 index segment_end_offsets segment_ends) as'

def replicated_iota [n] (flags: [n]bool) : [n]i32 =
  map i32.bool flags |> scan (+) 0 |> map (\x -> x - 1)
