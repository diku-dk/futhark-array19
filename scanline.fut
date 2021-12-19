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

let dxdz_dy (a: point_projected) (b: point_projected): (f32, f32) =
  let dy = b.y - a.y
  in if dy == 0
     then (0, 0)
     else let dx = r32 (b.x - a.x)
          let dz = b.z - a.z
          let dy' = r32 dy
          in (dx / dy', dz / dy')

let triangle_slopes ((p, q, r): triangle_projected): (i32, i32, i32, i32, f32, i32, f32, (f32, f32), (f32, f32), (f32, f32)) =
  (p.y, q.y - p.y, r.y - p.y, p.x, p.z, r.x, r.z, dxdz_dy p q, dxdz_dy p r, let (a, b) = dxdz_dy q r in (-a, -b))

let get_line_in_triangle 'a
    ((_, (p_y, q_y_minus_p_y, r_y_minus_p_y, p_x, p_z, r_x, r_z, (xsl1, zsl1), (xsl2, zsl2), (xsl3, zsl3)), aux): (i64, (i32, i32, i32, i32, f32, i32, f32, (f32, f32), (f32, f32), (f32, f32)), a))
    (i: i64): (line, a) =
  let i = i32.i64 i
  let y = p_y + i
  let half p_x' p_z' (xsl1', zsl1') (xsl2', zsl2') i' =
    let x1 = p_x' + t32 (f32.round (xsl1' * i'))
    let x2 = p_x' + t32 (f32.round (xsl2' * i'))
    let z1 = p_z' + zsl1' * i'
    let z2 = p_z' + zsl2' * i'
    in ({y, x1, x2, z1, z2}, aux)
  in if i <= q_y_minus_p_y
     then half p_x p_z (xsl1, zsl1) (xsl2, zsl2) (r32 i) -- upper half
     else half r_x r_z (-xsl2, -zsl2) (xsl3, zsl3) (r32 (r_y_minus_p_y - i)) -- lower half

let lines_of_triangles 'a [n]
    (triangles: [n]triangle_projected)
    (aux: [n]a): [](line, a) =
  -- FIXME PRECALC
  let triangles' = map normalize triangles
  in expand (\(n, _, _) -> n) get_line_in_triangle
            (map2 (\(triangle: triangle_projected) aux' -> (lines_in_triangle triangle, triangle_slopes triangle, aux')) triangles' aux)

let points_in_line 'a (({y=_, x1, x2, z1=_, z2=_}, _): (line, a)): i64 =
  i64.i32 (1 + i32.abs (x2 - x1))

let get_point_in_line 'a (({y, x1, x2, z1, z2}, aux): (line, a)) (i: i64): (point_2d_with_z, a) =
  ({x=x1 + i32.sgn (x2 - x1) * i32.i64 i,
    y,
    z=z1 + f32.sgn (z2 - z1) * f32.i64 i},
   aux)

let points_of_lines 'a (lines: [](line, a)): [](point_2d_with_z, a) =
  expand points_in_line get_point_in_line lines
