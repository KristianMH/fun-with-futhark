import "util"

local def check_segments (arr: []i32) (target: i32) =
    reduce (+) 0 arr == target

-- ==
-- entry: test_random_segments
-- input {1000 10}
-- output {true}
entry test_random_segments x y = 
    check_segments (random_segments x y) x
     

-- ==
-- entry: test_random_segments_1
-- input {1 12}
-- output {true}
entry test_random_segments_1 x y = 
    check_segments (random_segments x y) x
