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

let dxdz_dy (a: point_projected) (b: point_projected): slope =
  let dy = b.y - a.y
  in if dy == 0
     then {x=0, z=0}
     else let dx = r32 (b.x - a.x)
          let dz = b.z - a.z
          let dy' = r32 dy
          in {x=dx / dy', z=dz / dy'}

let triangle_slopes ((p, q, r): triangle_projected): triangle_slopes =
  {p_y=p.y,
   y_subtracted_p_y={q=q.y - p.y, r=r.y - p.y},
   p={x=p.x, z=p.z},
   r={x=r.x, z=r.z},
   s1=dxdz_dy p q,
   s2=dxdz_dy p r,
   s3=(let {x, z} = dxdz_dy q r in {x= -x, z= -z})}

let get_line_in_triangle 'a
    (((_, t), aux): (triangle_slopes_with_amount, a))
    (i: i64): (line, a) =
  let i = i32.i64 i
  let y = t.p_y + i
  let half p_x' p_z' (xsl1', zsl1') (xsl2', zsl2') i' =
    let x1 = p_x' + t32 (f32.round (xsl1' * i'))
    let x2 = p_x' + t32 (f32.round (xsl2' * i'))
    let z1 = p_z' + zsl1' * i'
    let z2 = p_z' + zsl2' * i'
    in ({y, x1, x2, z1, z2}, aux)
  in if i <= t.y_subtracted_p_y.q
     then half t.p.x t.p.z (t.s1.x, t.s1.z) (t.s2.x, t.s2.z) (r32 i) -- upper half
     else half t.r.x t.r.z (-t.s2.x, -t.s2.z) (t.s3.x, t.s3.z) (r32 (t.y_subtracted_p_y.r - i)) -- lower half

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

let points_in_line 'a (({y=_, x1, x2, z1=_, z2=_}, _): (line, a)): i64 =
  i64.i32 (1 + i32.abs (x2 - x1))

let get_point_in_line 'a (({y, x1, x2, z1, z2}, aux): (line, a)) (i: i64): (point_2d_with_z, a) =
  ({x=x1 + i32.sgn (x2 - x1) * i32.i64 i,
    y,
    z=z1 + f32.sgn (z2 - z1) * f32.i64 i},
   aux)

let points_of_lines 'a (lines: [](line, a)): [](point_2d_with_z, a) =
  expand points_in_line get_point_in_line lines
