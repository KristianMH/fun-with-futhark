import "segmented_radix_sort"

local def wrapper_i32 = segmented_radix_sort_naive i32.num_bits i32.get_bit

-- ==
-- entry: test_segmented_radix_sort
-- input { [5, 3, 1, 6] [9,5,56,3,2,436,94,6845,5835, 43,46,32,47,9,876] }
-- output {[2,3,5,9,56, 94, 436, 6845,5835, 9, 32, 43, 46, 47, 876] }
entry test_segmented_radix_sort = wrapper_i32

-- ==
-- entry: test_segmented_radix_sort_1
-- input { [4, 2, 3, 1] [15, 3, 8, 6, 1, 9, 2, 7, 4, 5] }
-- output {[ 3, 6, 8, 15, 1, 9, 2,4,7, 5] }
entry test_segmented_radix_sort_1 = wrapper_i32

-- ==
-- entry: test_segmented_radix_sort_2
-- input { [3, 3, 3] [10, 20, 30, 5, 15, 25, 1, 11, 21] }
-- output {[10, 20, 30, 5, 15, 25, 1, 11, 21] }
entry test_segmented_radix_sort_2 = wrapper_i32

-- ==
-- entry: test_segmented_radix_sort_3
-- input { [2, 2, 2, 2] [4, 2, 8, 6, 1, 3, 7, 5] }
-- output {[2, 4, 6, 8, 1, 3, 5, 7] }
entry test_segmented_radix_sort_3 = wrapper_i32

-- ==
-- entry: test_segmented_radix_sort_4
-- input { [1, 1, 1, 1, 1] [5, 4, 3, 2, 1] }
-- output {[5, 4, 3, 2, 1] }
entry test_segmented_radix_sort_4 = wrapper_i32

-- ==
-- entry: test_segmented_radix_sort_5
-- input { [5] [9, 8, 7, 6, 5] }
-- output {[5, 6, 7, 8, 9] }
entry test_segmented_radix_sort_5 = wrapper_i32

-- ==
-- entry: test_segmented_radix_sort_6
-- input { [100, 100, 100] [1, 1, 1]}
-- output {[1, 1, 1]}
entry test_segmented_radix_sort_6 = wrapper_i32
