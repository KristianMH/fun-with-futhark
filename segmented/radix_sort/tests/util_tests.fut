import "../util"

local
def check_segments (arr: []i64) (target: i64) =
  reduce (+) 0 arr == target

-- ==
-- entry: test_random_segments
-- input {1000i64 10}
-- output {true}
entry test_random_segments x y =
  check_segments (random_segments x y) x

-- ==
-- entry: test_random_segments_1
-- input {1i64 12}
-- output {true}
entry test_random_segments_1 x y =
  check_segments (random_segments x y) x
