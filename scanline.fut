import "lib/github.com/diku-dk/segmented/segmented"
import "types"

-- Based on Martin Elsman's https://github.com/melsman/canvas demo, modified to
-- fit within the bounds of this 3D rasterizer.

-- fixme use 'lerp'

def z_inv (z: f32): f32 =
  1 / z

def slope (a: point_projected) (b: point_projected): slope =
  let dy = b.extra.projected.y - a.extra.projected.y
  in if dy == 0
     then {extra={x=0}, z=0, bary={u= -1, v= -1}}
     else let dy' = r32 dy
          in {extra={x=r32 (b.extra.projected.x - a.extra.projected.x) / dy'},
              z=(z_inv b.z - z_inv a.z) / dy',
              bary={u=(b.bary.u - a.bary.u) / dy',
                    v=(b.bary.v - a.bary.v) / dy'}}

def neg_slope (s: slope): slope =
  {extra={x= -s.extra.x}, z= -s.z,
   bary={u= -s.bary.u, v= -s.bary.v}}

def triangle_slopes ((p, q, r): triangle_projected): triangle_slopes =
  {n_lines=r.extra.projected.y - p.extra.projected.y + 1,
   y=p.extra.projected.y,
   y_subtracted_p_y={q=q.extra.projected.y - p.extra.projected.y,
                     r=r.extra.projected.y - p.extra.projected.y},
   p={extra={x=p.extra.projected.x, world={x=p.extra.world.x, y=p.extra.world.y, z=z_inv p.extra.world.z}},
      z=z_inv p.z,
      bary=p.bary},
   q={extra={x=q.extra.projected.x, world={x=q.extra.world.x, y=q.extra.world.y, z=z_inv q.extra.world.z}},
      z=z_inv q.z,
      bary=q.bary},
   r={extra={x=r.extra.projected.x, world={x=r.extra.world.x, y=r.extra.world.y, z=z_inv r.extra.world.z}},
      z=z_inv r.z,
      bary=r.bary},
   s1=slope p q,
   s2=slope p r,
   s3=neg_slope (slope q r)}

def get_line_in_triangle 'a
    ((t, aux): (triangle_slopes, a))
    (i: i64): (line, a) =
  let i = i32.i64 i
  let y = t.y + i
  let half (p: slope_point) (s1: slope) (s2: slope) (i': f32): (line, a) =
    let x1 = p.extra.x + t32 (f32.round (s1.extra.x * i'))
    let x2 = p.extra.x + t32 (f32.round (s2.extra.x * i'))
    let z1 = p.z + s1.z * i'
    let z2 = p.z + s2.z * i'
    let bary_u1 = p.bary.u + s1.bary.u * i'
    let bary_u2 = p.bary.u + s2.bary.u * i'
    let bary_v1 = p.bary.v + s1.bary.v * i'
    let bary_v2 = p.bary.v + s2.bary.v * i'
    let n_points = 1 + i32.abs (x2 - x1)
    let n_points' = r32 n_points
    let x = i32.sgn (x2 - x1)
    let z = (z2 - z1) / n_points'
    let bary_u = (bary_u2 - bary_u1) / n_points'
    let bary_v = (bary_v2 - bary_v1) / n_points'
    in ({n_points,
         y,
         leftmost = {extra={x=x1}, z=z1,
                     bary={u=bary_u1, v=bary_v1}},
         step = {extra={x=x}, z=z,
                 bary={u=bary_u, v=bary_v}}},
         aux)
  in if i <= t.y_subtracted_p_y.q
     then half t.p t.s1 t.s2 (r32 i) -- upper half
     else half t.r (neg_slope t.s2) t.s3 (r32 (t.y_subtracted_p_y.r - i)) -- lower half

def lines_of_triangles 'a [n]
    (triangles: [n]triangle_slopes)
    (aux: [n]a): [](line, a) =
  expand (\(t, _) -> i64.i32 t.n_lines) get_line_in_triangle (zip triangles aux)

def points_in_line 'a ((line, _): (line, a)): i64 =
  i64.i32 line.n_points

def get_point_in_line 'a ((l, aux): (line, a)) (i: i64): (point_projected_final, a) =
  let i' = f32.i64 i
  in ({extra={x=l.leftmost.extra.x + l.step.extra.x * i32.i64 i,
                  y=l.y},
       z=l.leftmost.z + l.step.z * i',
       bary={u=l.leftmost.bary.u + l.step.bary.u * i',
             v=l.leftmost.bary.v + l.step.bary.v * i'}},
   aux)

def points_of_lines 'a (lines: [](line, a)): [](point_projected_final, a) =
  expand points_in_line get_point_in_line lines
