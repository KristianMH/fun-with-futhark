import "../segmented_radix_sort"
import "../util"

local def wrapper_i32 = segmented_radix_sort_int i32.num_bits i32.get_bit
local def wrapper_i32_old = segmented_radix_sort_int_old i32.num_bits i32.get_bit
local def wrapper_i32_naive = segmented_radix_sort_int_naive i32.num_bits i32.get_bit
local def wrapper_f32 = segmented_radix_sort_float f32.num_bits f32.get_bit
local def wrapper_f32_old = segmented_radix_sort_float_old f32.num_bits f32.get_bit
local def wrapper_f32_naive = segmented_radix_sort_float_naive f32.num_bits f32.get_bit
-- == 
-- entry: bench_segmented_radix_sort_int
-- random input { i32 [1000]i32}
-- random input { i32 [10000]i32}
-- random input { i32 [100000]i32}
-- random input { i32 [1000000]i32}
-- random input { i32 [10000000]i32}
entry bench_segmented_radix_sort_int seed xs = 
  let shp = random_segments (length xs) seed
  in wrapper_i32 shp xs

-- == 
-- entry: bench_segmented_radix_sort_int_old
-- random input { i32 [1000]i32}
-- random input { i32 [10000]i32}
-- random input { i32 [100000]i32}
-- random input { i32 [1000000]i32}
-- random input { i32 [10000000]i32}
entry bench_segmented_radix_sort_int_old seed xs = 
  let shp = random_segments (length xs) seed
  in wrapper_i32_old shp xs



-- == 
-- entry: bench_segmented_radix_sort_int_naive
-- random input { i32 [1000]i32}
-- random input { i32 [10000]i32}
-- random input { i32 [100000]i32}
entry bench_segmented_radix_sort_int_naive seed xs = 
  let shp = random_segments (length xs) seed
  in wrapper_i32_naive shp xs

-- == 
-- entry: bench_segmented_radix_sort_float
-- random input { i32 [1000]f32}
-- random input { i32 [10000]f32}
-- random input { i32 [100000]f32}
-- random input { i32 [1000000]f32}
-- random input { i32 [10000000]f32}
entry bench_segmented_radix_sort_float seed xs = 
  let shp = random_segments (length xs) seed
  in wrapper_f32 shp xs

-- == 
-- entry: bench_segmented_radix_sort_float_old
-- random input { i32 [1000]f32}
-- random input { i32 [10000]f32}
-- random input { i32 [100000]f32}
-- random input { i32 [1000000]f32}
-- random input { i32 [10000000]f32}
entry bench_segmented_radix_sort_float_old seed xs = 
  let shp = random_segments (length xs) seed
  in wrapper_f32_old shp xs

-- == 
-- entry: bench_segmented_radix_sort_float_naive
-- random input { i32 [1000]f32}
-- random input { i32 [10000]f32}
-- random input { i32 [100000]f32}
entry bench_segmented_radix_sort_float_naive seed xs = 
  let shp = random_segments (length xs) seed
  in wrapper_f32_naive shp xs