import "lib/github.com/diku-dk/cpprandom/shuffle"
import "lib/github.com/diku-dk/cpprandom/random"
module shuffle = mk_shuffle pcg32

-- Generate k random indices for initial centroids
def pick_random_indices (rng: shuffle.rng) (k: i64) (n: i64) : [k]i32 =
  let (_, idxs) = shuffle.shuffle rng (iota n)
  in take k idxs |> map i32.i64

-- Calculate Euclidean distance between two points
-- to multiple dist functions support!
-- transform into amodule ? !
def dist [m] (x: [m]f32) (y: [m]f32) : f32 =
  let diff = map2 (-) x y
  let sqr = map (** 2) diff
  in f32.sqrt (reduce (+) 0 sqr)

-- Find index of nearest centroid for a point
def nearest_centroid [k] [m] (centroids: [k][m]f32) (point: [m]f32) : i64 =
  let distances = map (dist point) centroids
  let (_, min_idx) =
    reduce_comm (\(d1, i1) (d2, i2) ->
                   if d1 < d2 then (d1, i1) else (d2, i2))
                (f32.inf, -1)
                (zip distances (iota k))
  in min_idx

def kmeans [n] [m] (points: [n][m]f32) (k: i64) (max_iter: i32) : [k][m]f32 =
  -- Initialize RNG
  -- to do pass seed to main!
  let rng = pcg32.rng_from_seed [123]
  -- Pick k random points as initial centroids
  let init_indices = pick_random_indices rng k n
  let centroids = map (\i -> points[i]) init_indices
  -- Iterate until convergence or max iterations
  -- to do: add convergence check
  in loop (centroids) for _i < max_iter do
       -- Assign points to nearest centroids
       let assignments = map (nearest_centroid centroids) points
       let clustersizes = hist (+) 0 k assignments (replicate n 1i32)
       let summedClusters = hist (map2 (+)) (replicate m 0) k assignments points
       let new_centroids = map2 (\c m -> map (/ (f32.i32) m) c) summedClusters clustersizes
       in new_centroids

def main [n] [m] (points: [n][m]f32) (k: i64) : [k][m]f32 =
  kmeans points k 100
