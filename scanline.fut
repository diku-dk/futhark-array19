import "lib/github.com/diku-dk/segmented/segmented"
import "types"

-- All of this is heavily copied from Martin Elsman's
-- https://github.com/melsman/canvas demo, and only slightly modified to fit
-- within the bounds of this 3D rasterizer.

let bubble
    (a: point_projected)
    (b: point_projected): (point_projected, point_projected) =
  if b.y < a.y then (b, a) else (a, b)

let normalize ((p, q, r): triangle_projected): triangle_projected =
  let (p, q) = bubble p q
  let (q, r) = bubble q r
  let (p, q) = bubble p q
  in (p, q, r)

let lines_in_triangle 'a (((p, _, r), _): (triangle_projected, a)): i64 =
  i64.i32 (r.y - p.y + 1)

let dxdy (a: point_projected) (b: point_projected): f32 =
  let dx = b.x - a.x
  let dy = b.y - a.y
  in if dy == 0 then r32 0
     else r32 dx f32./ r32 dy

let dzdy (a: point_projected) (b: point_projected): f32 =
  let dz = b.z - a.z
  let dy = b.y - a.y
  in if dy == 0 then r32 0
     else dz f32./ r32 dy

let get_line_in_triangle 'a
    (((p, q, r), aux): (triangle_projected, a))
    (i: i64): (line, a) =
  let i = i32.i64 i
  let y = p.y + i
  in if i <= q.y - p.y then     -- upper half
     let xsl1 = dxdy p q
     let xsl2 = dxdy p r
     let x1 = p.x + t32 (f32.round (xsl1 * r32 i))
     let x2 = p.x + t32 (f32.round (xsl2 * r32 i))
     let zsl1 = dzdy p q
     let zsl2 = dzdy p r
     let z1 = p.z + zsl1 * r32 i
     let z2 = p.z + zsl2 * r32 i
     in ({y, x1, x2, z1, z2}, aux)
     else                       -- lower half
     let xsl1 = dxdy r p
     let xsl2 = dxdy r q
     let dy = (r.y - p.y) - i
     let x1 = r.x - t32 (f32.round (xsl1 * r32 dy))
     let x2 = r.x - t32 (f32.round (xsl2 * r32 dy))
     let zsl1 = dzdy r p
     let zsl2 = dzdy r q
     let z1 = r.z - zsl1 * r32 dy
     let z2 = r.z - zsl2 * r32 dy
     in ({y, x1, x2, z1, z2}, aux)

let lines_of_triangles 'a [n]
    (triangles: [n]triangle_projected)
    (aux: [n]a): [](line, a) =
  let triangles' = map normalize triangles
  in expand lines_in_triangle get_line_in_triangle
            (zip triangles' aux)

let points_in_line 'a (({y=_, x1, x2, z1=_, z2=_}, _): (line, a)): i64 =
  i64.i32 (i32.(1 + abs (x2 - x1)))

let get_point_in_line 'a (({y, x1, x2, z1, z2}, aux): (line, a)) (i: i64): (point_2d_with_z, a) =
  ({x=x1 + i32.sgn (x2 - x1) * i32.i64 i,
    y,
    z=z1 + f32.sgn (z2 - z1) * f32.i64 i},
   aux)

let points_of_lines 'a (lines: [](line, a)): [](point_2d_with_z, a) =
  expand points_in_line get_point_in_line lines
