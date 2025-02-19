import "../segmented_radix_sort"
import "../util"
local def wrapper_i32 = segmented_radix_sort_int i32.num_bits i32.get_bit

local
def verify_order [n] [m] 't (gte: t -> t -> bool) (shp: [m]i64) (arr: [n]t) : bool =
  let end_idxs = scan (+) 0 shp
  let stat_idxs = rotate (-1) end_idxs with [0] = 0
  in all (\(i, j) ->
            let slice = arr[i:j]
            let last_one =
              if (length slice) > 1
              then slice[(length slice) - 1] `gte` slice[(length slice) - 2]
              else true
            -- reduce with max operator to find max index. check it is the
            -- or do sequential check ?
            let iter_range = length slice - 1
            let rest =
              if iter_range == 0
              then true
              else loop acc = false
                   for i < iter_range do
                     if i == 0
                     then true
                     else acc && (slice[i] `gte` slice[i - 1])
            in rest && last_one)
         (zip stat_idxs end_idxs)

-- ==
-- entry: test_segmented_radix_sort
-- input { [5i64, 3i64, 1i64, 6i64] [9,5,56,3,2,436,94,6845,5835, 43,46,32,47,9,876] }
-- output {[2,3,5,9,56, 94, 436, 6845,5835, 9, 32, 43, 46, 47, 876] }
entry test_segmented_radix_sort = wrapper_i32

-- ==
-- entry: test_segmented_radix_sort_1
-- input { [4i64, 2i64, 3i64, 1i64] [15, 3, 8, 6, 1, 9, 2, 7, 4, 5] }
-- output {[ 3, 6, 8, 15, 1, 9, 2,4,7, 5] }
entry test_segmented_radix_sort_1 = wrapper_i32

-- ==
-- entry: test_segmented_radix_sort_2
-- input { [3i64, 3i64, 3i64] [10, 20, 30, 5, 15, 25, 1, 11, 21] }
-- output {[10, 20, 30, 5, 15, 25, 1, 11, 21] }
entry test_segmented_radix_sort_2 = wrapper_i32

-- ==
-- entry: test_segmented_radix_sort_3
-- input { [2i64, 2i64, 2i64, 2i64] [4, 2, 8, 6, 1, 3, 7, 5] }
-- output {[2, 4, 6, 8, 1, 3, 5, 7] }
entry test_segmented_radix_sort_3 = wrapper_i32

-- ==
-- entry: test_segmented_radix_sort_4
-- input { [1i64, 1i64, 1i64, 1i64, 1i64] [5, 4, 3, 2, 1] }
-- output {[5, 4, 3, 2, 1] }
entry test_segmented_radix_sort_4 = wrapper_i32

-- ==
-- entry: test_segmented_radix_sort_5
-- input { [5i64] [9, 8, 7, 6, 5] }
-- output {[5, 6, 7, 8, 9] }
entry test_segmented_radix_sort_5 = wrapper_i32

-- ==
-- entry: test_segmented_radix_sort_6
-- input { [1i64, 1i64, 1i64] [14324, 432, 4532]}
-- output {[14324, 432, 4532]}
entry test_segmented_radix_sort_6 = wrapper_i32

-- ==
-- entry: test_segmented_radix_sort_random_int
-- random input { i32 [1000]i32}
-- random input { i32 [10000]i32}
-- random input { i32 [100000]i32}
-- output { true }
entry test_segmented_radix_sort_random_int seed xs =
  let shp = random_segments (length xs) seed
  let sorted = wrapper_i32 shp xs
  in verify_order (>=) shp sorted
-- ==
-- entry: test_segmented_radix_sort_random_int_naive
-- random input { i32 [1000]i32}
-- random input { i32 [10000]i32}
-- random input { i32 [100000]i32}
-- output { true }
entry test_segmented_radix_sort_random_int_naive seed xs =
  let shp = random_segments (length xs) seed
  let sorted = segmented_radix_sort_int_naive i32.num_bits i32.get_bit shp xs
  in verify_order (>=) shp sorted


-- ==
-- entry: test_segmented_radix_sort_random_float
-- random input { i32 [1000]f32}
-- random input { i32 [10000]f32}
-- random input { i32 [100000]f32}
-- output { true }
entry test_segmented_radix_sort_random_float seed xs =
  let shp = random_segments (length xs) seed
  let sorted = segmented_radix_sort_float f32.num_bits f32.get_bit shp xs
  in verify_order (>=) shp sorted

-- ==
-- entry: test_segmented_radix_sort_random_float_naive
-- random input { i32 [1000]f32}
-- random input { i32 [10000]f32}
-- random input { i32 [100000]f32}
-- output { true }
entry test_segmented_radix_sort_random_float_naive seed xs =
  let shp = random_segments (length xs) seed
  let sorted = segmented_radix_sort_float_naive f32.num_bits f32.get_bit shp xs
  in verify_order (>=) shp sorted