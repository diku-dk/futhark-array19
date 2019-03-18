import "lib/github.com/diku-dk/segmented/segmented"
import "lib/github.com/athas/matte/colour"
import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/diku-dk/sorts/radix_sort"

-- Drawing lines
type color = argb.colour
type point0 = (i32,i32)
type point = {p:point0,z:i32,color:color}
type line = {p1:point0,p2:point0,z:i32,color:color}

let red_by_index [m][n] 'a (vs0:*[m]a) (f:a->a->a) (ne:a) (is:[n]i32) (vs:[n]a) : *[m]a =
  let xs = zip is vs
  let xs = radix_sort_by_key (.1) i32.num_bits i32.get_bit xs
  let is = map (.1) xs
  let fs = map2 (!=) is (rotate (-1) is)
  let (is,vs) = unzip <| segmented_reduce (\(i1,v1) (i2,v2) -> (i32.max i1 i2,f v1 v2)) (-1,ne) fs xs
  in scatter vs0 is vs

-- Write to grid
let mk_grid [n] (h:i32) (w:i32) (xs:[n]i32) (ys:[n]i32)
                (vs:[n]{z:i32,color:color}) : [h][w]i32 =
  let is = map2 (\x y -> w*y+x) xs ys
  let grid0 = replicate (h*w) (0,argb.white)
  let f (z1,c1) (z2,c2) = if z1 > z2 then (z1,c1) else (z2,c2)
  let cs = map (\v -> (v.z,v.color)) vs
  let grid = reduce_by_index grid0 f (0,argb.white) is cs
  let grid = map (.2) grid
  --let grid = scatter (replicate (h*w) 0) is (map (.color) vs)
  in unflatten h w grid

let max = i32.max
let abs = i32.abs

let compare (v1:i32) (v2:i32) : i32 =
  if v2 > v1 then 1 else if v1 > v2 then -1 else 0

let slope ((x1,y1):point0) ((x2,y2):point0) : f32 =
  if x2==x1 then if y2>y1 then r32(1) else r32(-1)
                 else r32(y2-y1) / r32(abs(x2-x1))

-- Parallel flattened algorithm for turning lines into
-- points, using expansion.

let points_in_line (l:line) : i32 =
  let (x1,y1) = l.p1
  let (x2,y2) = l.p2
  in i32.(1 + max (abs(x2-x1)) (abs(y2-y1)))

let get_point_in_line ({p1,p2,z,color}:line) (i:i32) : point =
  if i32.abs(p1.1-p2.1) > i32.abs(p1.2-p2.2)
  then let dir = compare (p1.1) (p2.1)
       let sl = slope p1 p2
       in {p=(p1.1+dir*i,
	      p1.2+i32.f32(f32.round(sl*r32 i))),
	   z,
	   color}
  else let dir = compare (p1.2) (p2.2)
       let sl = slope (p1.2,p1.1) (p2.2,p2.1)
          in {p=(p1.1+i32.f32(f32.round(sl*r32 i)),
		 p1.2+i*dir),
	      z,
	      color}

let drawlines [n] (h:i32) (w:i32)
                  (lines:[n]line) :[h][w]i32 =
  let ps = expand points_in_line get_point_in_line lines
  let xs = map (.p.1) ps
  let ys = map (.p.2) ps
  let vs = map (\p -> {z=p.z,color=p.color}) ps
  in mk_grid h w xs ys vs

type triangle = (point0,point0,point0,i32,color)

-- Parallel flattened algorithm for turning triangles into
-- lines, using expansion.

let bubble (a:point0) (b:point0) =
  if b.2 < a.2 then (b,a) else (a,b)

let normalize ((p,q,r,z,c): triangle) : triangle =
  let (p,q) = bubble p q
  let (q,r) = bubble q r
  let (p,q) = bubble p q
  in (p,q,r,z,c)

let lines_in_triangle ((p,_,r,_,_):triangle) : i32 =
  r.2 - p.2 + 1

let dxdy (a:point0) (b:point0) : f32 =
  let dx = b.1 - a.1
  let dy = b.2 - a.2
  in if dy == 0 then f32.i32 0
     else f32.i32 dx f32./ f32.i32 dy

let get_line_in_triangle ((p,q,r,z,c):triangle) (i:i32) =
  let y = p.2 + i
  in if i <= q.2 - p.2 then     -- upper half
       let sl1 = dxdy p q
       let sl2 = dxdy p r
       let x1 = p.1 + i32.f32(f32.round(sl1 * f32.i32 i))
       let x2 = p.1 + i32.f32(f32.round(sl2 * f32.i32 i))
       in {p1=(x1,y),p2=(x2,y),z,color=c}
     else                       -- lower half
       let sl1 = dxdy r p
       let sl2 = dxdy r q
       let dy = (r.2 - p.2) - i
       let x1 = r.1 - i32.f32(f32.round(sl1 * f32.i32 dy))
       let x2 = r.1 - i32.f32(f32.round(sl2 * f32.i32 dy))
       in {p1=(x1,y),p2=(x2,y),z,color=c}

let lines_of_triangles (xs:[]triangle) : []line =
  expand lines_in_triangle get_line_in_triangle
         (map normalize xs)

-- Pick an RNG engine and define random distributions for specific types.
module rng_engine = minstd_rand
--module rand_f32 = uniform_real_distribution f32 rng_engine
module rand_i32 = uniform_int_distribution i32 rng_engine

-- We can create a new RNG state with 'rng_engine.rng_from_seed [x]',
-- where 'x' is some seed.  We can split one RNG state into many with
-- 'rng_engine.split_rng'.
--
-- For an RNG state 'r', we can generate random integers that are
-- either 0 or 1 by calling 'rand_i8.rand (0i8, 1i8) r'.
--
-- For details, consult
-- https://futhark-lang.org/pkgs/github.com/diku-dk/cpprandom/1.1.4/doc/lib/github.com/diku-dk/cpprandom/random.html

--
-- The gui entries
--
type state = {width:i32,height:i32,triangles:[]triangle}

-- Create a new start state
entry start_state (seed: i32) (h: i32) (w: i32): state =
  let rng = rng_engine.rng_from_seed [seed]
  let rngs = rng_engine.split_rng 1000 rng
  let ts = map2 (\rng z ->
		  let maxsz = 200
		  let (rng,x1) = rand_i32.rand (0,w-maxsz) rng
		  let (rng,y1) = rand_i32.rand (0,h-maxsz) rng
		  let (rng,dx2) = rand_i32.rand (5,maxsz) rng
		  let (rng,dy2) = rand_i32.rand (5,maxsz) rng
		  let (rng,dx3) = rand_i32.rand (5,maxsz) rng
		  let (rng,dy3) = rand_i32.rand (5,maxsz) rng
		  let (x2,y2) = (x1+dx2,y1+dy2)
		  let (x3,y3) = (x1+dx3,y1+dy3)
		  let (_rng,c) = rand_i32.rand (0,256*256*256) rng
		  in ((x1,y1),(x2,y2),(x3,y3),z+1,c)) rngs (iota(length rngs))
  in {width=w,height=h,triangles=ts}

-- Take one step
entry step (vx: i32) (vy: i32) ({width,height,triangles}: state) : state =
  let okx x = x+vx > 0 && x+vx<width
  let oky y = y+vy > 0 && y+vy<height
  let zmax = reduce (i32.max) 0 (map (.4) triangles)
  let nextz z =
    if z >= zmax then 0 else z+1
  let trans_triangle (t:triangle) : triangle =
    let ((x1,y1),(x2,y2),(x3,y3),z,color) = t
    in if okx x1 && okx x2 && okx x3 && oky y1 && oky y2 && oky y3 then
         ((x1+vx,y1+vy),(x2+vx,y2+vy),(x3+vx,y3+vy),nextz z,color)
       else (\(p1,p2,p3,z,c) -> (p1,p2,p3,nextz z,c)) t
  in {width,height,triangles=map trans_triangle triangles}

-- | Turn lines into an array of pixel values, ready to be
-- blitted to the screen.
entry render ({width,height,triangles}:state): [][]color =
  lines_of_triangles triangles |> drawlines height width
