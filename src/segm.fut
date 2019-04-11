--
-- Library of segmented operations, including expand and friends
--

let segm_scan [n] 't (op: t -> t -> t) (ne: t)
              (flags: [n]bool)
              (as: [n]t) : [n]t =
  zip flags as
  |> scan (\(x_flag,x) (y_flag,y) ->
            (x_flag || y_flag,
             if y_flag then y else x `op` y))
     (false, ne)
  |> unzip |> (.2)

let repl_iota [n] (reps:[n]i32) : []i32 =
  let s1 = scan (+) 0 reps
  let s2 = map (\i -> if i==0 then 0
                     else unsafe s1[i-1]) (iota n)
  let tmp = scatter (replicate (reduce (+) 0 reps) 0)
                    s2 (iota n)
  let flags = map (>0) tmp
  in segm_scan (+) 0 flags tmp

let segm_iota [n] (flags:[n]bool) : [n]i32 =
  let iotas = segm_scan (+) 0 flags (replicate n 1)
  in map (\x -> x-1) iotas

let segm_reduce [n] 't (op: t -> t -> t) (ne: t)
                            (flags: [n]bool) (as: [n]t) =
  let as' = segm_scan op ne flags as
  let segm_ends = rotate 1 flags
  let segm_end_offsets = segm_ends |> map i32.bool |> scan (+) 0
  let num_segm = if n > 1 then segm_end_offsets[n-1] else 0
  let scratch = replicate num_segm ne
  let index i f = if f then i-1 else -1
  in scatter scratch (map2 index segm_end_offsets segm_ends) as'

let expand 'a 'b (sz: a -> i32) (get: a -> i32 -> b)
                 (arr:[]a) : []b =
  let szs = map sz arr
  let idxs = repl_iota szs
  let iotas = segm_iota (map2 (!=) idxs
                              (rotate (-1) idxs))
  in map2 (\i j -> get (unsafe arr[i]) j)
                       idxs iotas

let expand_pad 'a 'b [n] (sz: a -> i32) (get: a -> i32 -> b) (z:b)
                         (arr:[n]a) : []b =
  let szs = map sz arr
  let msz = reduce i32.max 0 szs
  in flatten <|
     map2 (\a s ->
  	     map (\j -> if j > s then z
			else get a j
		 ) (iota msz)
          ) arr szs

let expand_with_flags 'a 'b
                      (sz: a -> i32) (get: a -> i32 -> b)
                      (arr:[]a) : ([]b, []bool)  =
  let szs = map sz arr
  let idxs = repl_iota szs
  let flags = map2 (!=) idxs (rotate (-1) idxs)
  let vals = map2 (\i j -> get (unsafe arr[i]) j)
                  idxs (segm_iota flags)
  in (vals, flags)

let expand_reduce 'a 'b [n]
                  (sz: a -> i32) (get: a -> i32 -> b)
                  (f: b -> b -> b) (ne:b)
                  (arr:[n]a) : *[n]b =
  let (vals, flags) = expand_with_flags sz get arr
  in segm_reduce f ne flags vals


let expand_reduce_keep 'a 'b [n]
                       (sz: a -> i32) (get: a -> i32 -> b)
                       (f: b -> b -> b) (ne:b)
                       (as:[n]a) : *[n]b =
  let ias = zip (iota n) as
  let (is, bs) = unzip <| expand (sz <-< (.2))
                                 (\(i,a) j -> (i,get a j))
                                 ias
  let dst = replicate n ne
  in reduce_by_index dst f ne is bs
