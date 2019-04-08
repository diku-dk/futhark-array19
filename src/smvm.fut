import "segm"

type csr 't = {row_off: []i32, col_idx: []i32, vals: []t}

-- Sparse matrix-vector multiplication by expanding each row into an
-- irregular number of multiplications, followed by a segmented
-- reduction with + and 0.

let smvm ({row_off,col_idx,vals} : csr f32)
         (v:[]f32) : []f32 = unsafe
  let rows = map (\i -> (i,
                        row_off[i],
                        row_off[i+1]-row_off[i]))
                 (iota(length row_off - 1))
  let sz r = r.3
  let get r i = vals[r.2+i] * v[col_idx[r.2+i]]
  in expand_reduce sz get (+) 0f32 rows

-- let m : [](i32,i32,i32) =
--   [(0,0,1),(0,1,2),(0,3,11),
--    (1,1,3),(1,2,4),
--    (2,1,5),(2,2,6),(2,3,7),
--    (3,3,8),
--    (4,3,9),(4,4,10)]

let m_csr : csr f32 =
  {row_off=[0,3,5,8,9,11],          -- size 6
   col_idx=[0,1,3,1,2,1,2,3,3,3,4], -- size 11
   vals=map f32.i32 [1,2,11,3,4,5,6,7,8,9,10]}  -- size 11

let v : []f32 = map f32.i32 [3,1,2,6,5]

let main (_ : i32) : []i32 =   -- [71,11,59,48,104]
  map (i32.f32) <| smvm m_csr (copy v)

import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/diku-dk/sorts/merge_sort"
module rng = xorshift128plus
module ID = uniform_int_distribution i32 rng
module RD = uniform_real_distribution f32 rng
type rng = rng.rng

-- genrow n d rng generate an array of size d with sorted entries
-- between 0 and (n-1) and no duplicates.

let genrow (n:i32) (d:i32) rng : [d]i32 =  -- d << n
  let rngs = rng.split_rng n rng
  let (_, vals) = unzip <| map (RD.rand (0f32,1f32)) rngs
  let vals_idx = zip vals (iota (length vals))
  let sorted = merge_sort_by_key (.1) (<=) vals_idx
  let idxs = (map (.2) sorted)[:d]
  in merge_sort (<=) idxs


entry gen_square_csr_and_vector (seed:i32) (rows: i32) (pct:i32) : ([]i32, []i32, []f32,[]f32) =
  let rng = rng.rng_from_seed [seed]
  -- vector
  let rngs = rng.split_rng rows rng
  let (rngs,vector) = unzip <| map (RD.rand (0f32,1f32)) rngs
  let rng = rng.join_rng rngs
  -- 100x100 10% sparse => 1000 elements => avg 1000/100=10 elements pr row;
  let elems_total = f32.i32(rows*rows*pct) / 100f32
  let elems_row = i32.f32(elems_total / f32.i32 rows)
  let row_off = map (\i -> i*elems_row) (iota (rows+1))
  -- values
  let rngs = rng.split_rng row_off[length(row_off)-1] rng
  let (rngs, vals) = unzip <| map (RD.rand (0f32,1f32)) rngs
  let rng = rng.join_rng rngs
  -- col_off
  let rngs = rng.split_rng rows rng
  let col_idx = map (genrow rows elems_row) rngs
                |> flatten
  in (row_off, col_idx, vals, vector)


entry test_smvm (row_off:[]i32) (col_idx:[]i32) (vals:[]f32) (v:[]f32) : f32 =
   let csr = {row_off,col_idx,vals}
   let res = smvm csr v
   in reduce (+) 0f32 res

let dotprod xs ys = map2 (*) xs ys |> f32.sum

let matmul [n][m][k] (xss: [n][m]f32) (yss: [m][k]f32) =
  map (\xs -> map (dotprod xs) (transpose yss)) xss

let mvm [n][m] (xss: [n][m]f32) (v: [m]f32) : [n]f32 =
  map (\xs -> dotprod xs v) xss

entry test_dense (n:i32) : f32 =
  let m = copy (replicate n (replicate n 0.9f32))
  let v = copy (replicate n 0.8f32)
  in reduce (+) 0f32 (mvm m v)
