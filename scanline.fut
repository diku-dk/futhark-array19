import "lib/github.com/diku-dk/segmented/segmented"
import "types"

-- All of this is heavily copied from Martin Elsman's
-- https://github.com/melsman/canvas demo, and only slightly modified to fit
-- within the bounds of this 3D rasterizer.
--
-- FIXME: Also find the z values.  We currently do this in a separate pass in a
-- pretty inefficient way.

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

let get_line_in_triangle 'a
    (((p, q, r), aux): (triangle_projected, a))
    (i: i64): (line, a) =
  let i = i32.i64 i
  let y = p.y + i
  in if i <= q.y - p.y then     -- upper half
     let sl1 = dxdy p q
     let sl2 = dxdy p r
     let x1 = p.x + t32 (f32.round (sl1 * r32 i))
     let x2 = p.x + t32 (f32.round (sl2 * r32 i))
     in (({x=x1, y}, {x=x2, y}), aux)
     else                       -- lower half
     let sl1 = dxdy r p
     let sl2 = dxdy r q
     let dy = (r.y - p.y) - i
     let x1 = r.x - t32 (f32.round (sl1 * r32 dy))
     let x2 = r.x - t32 (f32.round (sl2 * r32 dy))
     in (({x=x1, y}, {x=x2, y}), aux)

let lines_of_triangles 'a [n]
    (triangles: [n]triangle_projected)
    (aux: [n]a): [](line, a) =
  let triangles' = map normalize triangles
  in expand lines_in_triangle get_line_in_triangle
            (zip triangles' aux)

let swap ({x, y}: point_2d): point_2d = {x=y, y=x}

let compare (v1: i32) (v2: i32): i32 =
  if v2 > v1 then 1 else if v1 > v2 then -1 else 0

let slo ({x=x1, y=y1}: point_2d) ({x=x2, y=y2}: point_2d): f32 =
  if x2 == x1 then if y2 > y1 then r32 1 else r32 (-1)
  else r32 (y2 - y1) / r32 (i32.abs (x2 - x1))

let points_in_line 'a ((({x=x1, y=y1}, {x=x2, y=y2}), _): (line, a)): i64 =
  i64.i32 (i32.(1 + max (abs (x2 - x1)) (abs (y2 - y1))))

let get_point_in_line 'a (((p1, p2), aux): (line, a)) (i: i64): (point_2d, a) =
  let i = i32.i64 i in
  if i32.abs (p1.x - p2.x) > i32.abs (p1.y - p2.y)
  then let dir = compare p1.x p2.x
       let sl = slo p1 p2
       in ({x=p1.x + dir * i,
            y=p1.y + t32 (sl * r32 i)}, aux)
  else let dir = compare (p1.y) (p2.y)
       let sl = slo (swap p1) (swap p2)
       in ({x=p1.x + t32 (sl * r32 i),
            y=p1.y + i * dir}, aux)

let points_of_lines 'a (lines: [](line, a)): [](point_2d, a) =
  expand points_in_line get_point_in_line lines
