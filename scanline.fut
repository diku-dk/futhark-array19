import "lib/github.com/diku-dk/segmented/segmented"
import "types"

-- Based on Martin Elsman's https://github.com/melsman/canvas demo, modified to
-- fit within the bounds of this 3D rasterizer.

let z_inv (z: f32): f32 =
  1 / z

let slope (a: point_projected) (b: point_projected): slope =
  let dy = b.projected.y - a.projected.y
  in if dy == 0
     then {projected={x=0}, z=0, world=vec3.zero}
     else let dy' = r32 dy
          in {projected={x=r32 (b.projected.x - a.projected.x) / dy'},
              z=(z_inv b.z - z_inv a.z) / dy',
              world={x=(b.world.x - a.world.x) / dy',
                     y=(b.world.y - a.world.y) / dy',
                     z=(z_inv b.world.z - z_inv a.world.z) / dy'}}

let neg_slope (s: slope): slope =
  {projected={x= -s.projected.x}, z= -s.z, world={x= -s.world.x, y= -s.world.y, z= -s.world.z}}

let triangle_slopes ((p, q, r): triangle_projected): triangle_slopes =
  {n_lines=r.projected.y - p.projected.y + 1,
   y=p.projected.y,
   y_subtracted_p_y={q=q.projected.y - p.projected.y,
                     r=r.projected.y - p.projected.y},
   p={projected={x=p.projected.x},
      z=z_inv p.z,
      world={x=p.world.x, y=p.world.y, z=z_inv p.world.z}},
   r={projected={x=r.projected.x},
      z=z_inv r.z,
      world={x=r.world.x, y=r.world.y, z=z_inv r.world.z}},
   s1=slope p q,
   s2=slope p r,
   s3=neg_slope (slope q r)}

let get_line_in_triangle 'a
    ((t, aux): (triangle_slopes, a))
    (i: i64): (line, a) =
  let i = i32.i64 i
  let y = t.y + i
  let half (p: slope_point) (s1: slope) (s2: slope) (i': f32): (line, a) =
    let x1 = p.projected.x + t32 (f32.round (s1.projected.x * i'))
    let x2 = p.projected.x + t32 (f32.round (s2.projected.x * i'))
    let z1 = p.z + s1.z * i'
    let z2 = p.z + s2.z * i'
    let world_x1 = p.world.x + s1.world.x * i'
    let world_x2 = p.world.x + s2.world.x * i'
    let world_y1 = p.world.y + s1.world.y * i'
    let world_y2 = p.world.y + s2.world.y * i'
    let world_z1 = p.world.z + s1.world.z * i'
    let world_z2 = p.world.z + s2.world.z * i'
    let n_points = 1 + i32.abs (x2 - x1)
    let n_points' = r32 n_points
    let x = i32.sgn (x2 - x1)
    let z = (z2 - z1) / n_points'
    let world_x = (world_x2 - world_x1) / n_points'
    let world_y = (world_y2 - world_y1) / n_points'
    let world_z = (world_z2 - world_z1) / n_points'
    in ({n_points,
         y,
         leftmost = {projected={x=x1}, z=z1,
                     world={x=world_x1, y=world_y1, z=world_z1}},
         step = {projected={x=x}, z=z,
                 world={x=world_x, y=world_y, z=world_z}}},
         aux)
  in if i <= t.y_subtracted_p_y.q
     then half t.p t.s1 t.s2 (r32 i) -- upper half
     else half t.r (neg_slope t.s2) t.s3 (r32 (t.y_subtracted_p_y.r - i)) -- lower half

let lines_of_triangles 'a [n]
    (triangles: [n]triangle_slopes)
    (aux: [n]a): [](line, a) =
  expand (\(t, _) -> i64.i32 t.n_lines) get_line_in_triangle (zip triangles aux)

let points_in_line 'a ((line, _): (line, a)): i64 =
  i64.i32 line.n_points

let get_point_in_line 'a ((l, aux): (line, a)) (i: i64): (point_projected, a) =
  let i' = f32.i64 i
  in ({projected={x=l.leftmost.projected.x + l.step.projected.x * i32.i64 i,
                  y=l.y},
       z=l.leftmost.z + l.step.z * i',
       world={x=l.leftmost.world.x + l.step.world.x * i',
              y=l.leftmost.world.y + l.step.world.y * i',
              z=l.leftmost.world.z + l.step.world.z * i'}},
   aux)

let points_of_lines 'a (lines: [](line, a)): [](point_projected, a) =
  expand points_in_line get_point_in_line lines
