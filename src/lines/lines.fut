import "lib/github.com/diku-dk/segmented/segmented"
import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/athas/matte/colour"

-- Drawing lines
type point = (i32,i32)
type line = (point,point)
type state = {width:i32,height:i32,lines:[]line}

-- Write to grid
let mk_grid [n] (h:i32) (w:i32) (xs:[n]i32)
                (ys:[n]i32) : [h][w]i32 =
  let is = map2 (\x y -> if 0<=y && y<h && 0<=x && x<w
			 then w*y+x else -1) xs ys
  let flatgrid = replicate (h*w) 0
  let ones = map (\_ -> 1) is
  in unflatten h w (scatter (copy flatgrid) is ones)

let compare (v1:i32) (v2:i32) : i32 =
  if v2 > v1 then 1 else if v1 > v2 then -1 else 0

let slope ((x1,y1):point) ((x2,y2):point) : f32 =
  if x2==x1 then if y2>y1 then r32(1) else r32(-1)
                 else r32(y2-y1) / r32(i32.abs(x2-x1))

-- Parallel flattened algorithm for turning lines into
-- points, using expansion.
let points_in_line ((x1,y1),(x2,y2)) =
  i32.(1 + max (abs(x2-x1)) (abs(y2-y1)))

let get_point_in_line ((p1,p2):line) (i:i32) =
  if i32.abs(p1.1-p2.1) > i32.abs(p1.2-p2.2)
  then let dir = compare (p1.1) (p2.1)
       let sl = slope p1 p2
       in (p1.1+dir*i,
           p1.2+i32.f32(f32.round(sl*r32 i)))
    else let dir = compare (p1.2) (p2.2)
         let sl = slope (p1.2,p1.1) (p2.2,p2.1)
         in (p1.1+i32.f32(f32.round(sl*r32 i)),
             p1.2+i*dir)

let drawlines [n] (h:i32) (w:i32) (lines:[n]line) :[h][w]i32 =
  let (xs,ys) = expand points_in_line get_point_in_line lines
              |> unzip
  in mk_grid h w xs ys

-- Pick an RNG engine and define random distributions for specific types.
--
-- We can create a new RNG state with 'rng_engine.rng_from_seed [x]',
-- where 'x' is some seed.  We can split one RNG state into many with
-- 'rng_engine.split_rng'.
--
-- For an RNG state 'r', we can generate random integers between 1 and 8
-- by calling 'rand_i32.rand (1,6) r'.
--
module rng_engine = minstd_rand
module rand_i32 = uniform_int_distribution i32 rng_engine

--
-- The gui entries
--

-- Create a new start state
entry start_state (seed: i32) (h: i32) (w: i32): state =
  let rng = rng_engine.rng_from_seed [seed]
  let rngs = rng_engine.split_rng 1000 rng
  let lines = map (\rng ->
		     let (rng,x1) = rand_i32.rand (0,w/3-1) rng
		     let (rng,y1) = rand_i32.rand (0,h/3-1) rng
		     let (rng,x2) = rand_i32.rand (0,w/3-1) rng
		     let (_rng,y2) = rand_i32.rand (0,h/3-1) rng
		     in ((x1+w/3,y1+h/3),(x2+w/3,y2+h/3))) rngs
  in {width=w,height=h,lines}

-- Take one step
entry step (vx: i32) (vy: i32) ({width,height,lines}: state) : state =
  let trans_line ((x1,y1),(x2,y2)) =
    ((x1+vx,y1+vy),(x2+vx,y2+vy))
  in {width,height,lines=map trans_line lines}

-- Turn lines into an array of pixel values, ready to be
-- blitted to the screen.
entry render ({width,height,lines}:state): [][]argb.colour =
  let grid = drawlines height width lines
  let toColor i =
    if i == 0 then argb.(bright <| light blue)
    else argb.(bright <| light red)
  in map1 (map1 toColor) grid
