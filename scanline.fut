import "lib/github.com/diku-dk/segmented/segmented"
import "types"

-- Based on Martin Elsman's https://github.com/melsman/canvas demo, modified to
-- fit within the bounds of this 3D rasterizer.

let bubble
    (a: point_projected)
    (b: point_projected): (point_projected, point_projected) =
  if b.y < a.y then (b, a) else (a, b)

let normalize ((p, q, r): triangle_projected): triangle_projected =
  let (p, q) = bubble p q
  let (q, r) = bubble q r
  let (p, q) = bubble p q
  in (p, q, r)

let lines_in_triangle ((p, _, r): (triangle_projected)): i64 =
  i64.i32 (r.y - p.y + 1)

let dy (a: point_projected) (b: point_projected): slope =
  let dy = b.y - a.y
  in if dy == 0
     then {x=0, z=0, x_orig=0, y_orig=0, z_orig=0}
     else let dx = r32 (b.x - a.x)
          let dz = (1 / b.z) - (1 / a.z)
          let dx_orig = b.x_orig - a.x_orig
          let dy_orig = b.y_orig - a.y_orig
          let dz_orig = b.z_orig - a.z_orig
          let dy' = r32 dy
          in {x=dx / dy', z=dz / dy', x_orig=dx_orig / dy', y_orig=dy_orig / dy', z_orig=dz_orig / dy'}

let triangle_slopes ((p, q, r): triangle_projected): triangle_slopes =
  {p_y=p.y,
   y_subtracted_p_y={q=q.y - p.y, r=r.y - p.y},
   p={x=p.x, z=1 / p.z, x_orig=p.x_orig, y_orig=p.y_orig, z_orig=p.z_orig},
   r={x=r.x, z=1 / r.z, x_orig=r.x_orig, y_orig=r.y_orig, z_orig=r.z_orig},
   s1=(let s = dy p q in s),
   s2=(let s = dy p r in s),
   s3=(let s = dy q r in {x= -s.x, z= -s.z, x_orig= -s.x_orig, y_orig= -s.y_orig, z_orig= -s.z_orig})}

let get_line_in_triangle 'a
    (((_, t), aux): (triangle_slopes_with_amount, a))
    (i: i64): (line, a) =
  let i = i32.i64 i
  let y = t.p_y + i
  let half p s1 s2 i' =
    let x1 = p.x + t32 (f32.round (s1.x * i'))
    let x2 = p.x + t32 (f32.round (s2.x * i'))
    let z1 = p.z + s1.z * i'
    let z2 = p.z + s2.z * i'
    let x_orig1 = p.x_orig + s1.x_orig * i'
    let x_orig2 = p.x_orig + s2.x_orig * i'
    let y_orig1 = p.y_orig + s1.y_orig * i'
    let y_orig2 = p.y_orig + s2.y_orig * i'
    let z_orig1 = p.z_orig + s1.z_orig * i'
    let z_orig2 = p.z_orig + s2.z_orig * i'
    let fn = r32 (1 + i32.abs (x2 - x1))
    let z = (z2 - z1) / fn
    let x_orig = (x_orig2 - x_orig1) / fn
    let y_orig = (y_orig2 - y_orig1) / fn
    let z_orig = (z_orig2 - z_orig1) / fn
    in ({y, x = i32.sgn (x2 - x1), x1, z, z1, x_orig1, y_orig1, z_orig1, n_points = (1 + i32.abs (x2 - x1)), x_orig, y_orig, z_orig}, aux)
  in if i <= t.y_subtracted_p_y.q
     then half t.p t.s1 t.s2 (r32 i) -- upper half
     else half t.r {x= -t.s2.x, z= -t.s2.z, x_orig= -t.s2.x_orig, y_orig= -t.s2.y_orig, z_orig= -t.s2.z_orig} t.s3 (r32 (t.y_subtracted_p_y.r - i)) -- lower half

let prepare_triangles [n] (triangles: [n]triangle_projected): [n]triangle_slopes_with_amount =
  map normalize triangles
  |> map (\triangle -> (lines_in_triangle triangle, triangle_slopes triangle))

-- Call this variant if your triangle data has already been prepared for the rasterizer.
let lines_of_triangles_prepared 'a [n]
    (triangles: [n](i64, triangle_slopes))
    (aux: [n]a): [](line, a) =
  expand (\((n, _), _) -> n) get_line_in_triangle (zip triangles aux)

let lines_of_triangles 'a [n]
    (triangles: [n]triangle_projected)
    (aux: [n]a): [](line, a) =
  lines_of_triangles_prepared (prepare_triangles triangles) aux

let points_in_line 'a ((line, _): (line, a)): i64 =
  i64.i32 line.n_points

let get_point_in_line 'a (({y, x, n_points=_, x1, z, z1, x_orig1, y_orig1, z_orig1, x_orig, y_orig, z_orig}, aux): (line, a)) (i: i64): (point_2d_with_interpolation, a) =
  let i' = f32.i64 i in
  ({x=x1 + x * i32.i64 i,
    y,
    z=z1 + z * i',
    x_orig=x_orig1 + x_orig * i',
    y_orig=y_orig1 + y_orig * i',
    z_orig=z_orig1 + z_orig * i'},
   aux)

let points_of_lines 'a (lines: [](line, a)): [](point_2d_with_interpolation, a) =
  expand points_in_line get_point_in_line lines
