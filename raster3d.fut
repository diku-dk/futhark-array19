import "lib/github.com/athas/matte/colour"
import "types"
import "scanline"
import "hsv"

let bubble_point
    (a: point_projected)
    (b: point_projected): (point_projected, point_projected) =
  if b.projected.y < a.projected.y then (b, a) else (a, b)

let normalize_triangle_points ((p, q, r): triangle_projected): triangle_projected =
  let (p, q) = bubble_point p q
  let (q, r) = bubble_point q r
  let (p, q) = bubble_point p q
  in (p, q, r)

let prepare_triangles [n] (triangles: [n]triangle_projected): [n]triangle_slopes =
  map normalize_triangle_points triangles
  |> map (\triangle -> triangle_slopes triangle)

let rotate_point
  (angle: vec3.vector)
  (origo: vec3.vector)
  (p: vec3.vector)
  : vec3.vector =
  let (x0, y0, z0) = (p.x - origo.x, p.y - origo.y, p.z - origo.z)

  let (sin_x, cos_x) = (f32.sin angle.x, f32.cos angle.x)
  let (sin_y, cos_y) = (f32.sin angle.y, f32.cos angle.y)
  let (sin_z, cos_z) = (f32.sin angle.z, f32.cos angle.z)

  -- X axis.
  let (x1, y1, z1) = (x0,
                      y0 * cos_x - z0 * sin_x,
                      y0 * sin_x + z0 * cos_x)
  -- Y axis.
  let (x2, y2, z2) = (z1 * sin_y + x1 * cos_y,
                      y1,
                      z1 * cos_y - x1 * sin_y)
  -- Z axis.
  let (x3, y3, z3) = (x2 * cos_z - y2 * sin_z,
                      x2 * sin_z + y2 * cos_z,
                      z2)

  let (x', y', z') = (origo.x + x3, origo.y + y3, origo.z + z3)
  in {x=x', y=y', z=z'}

-- | Translate and rotate all points relative to the camera.
let camera_normalize_point ({position=pc, orientation=po}: camera) (p: vec3.vector): vec3.vector =
  let p' = p vec3.+ {x= -pc.x, y= -pc.y, z= -pc.z}
  let p'' = rotate_point {x= -po.x, y= -po.y, z= -po.z} {x=0.0, y=0.0, z=0.0} p'
  in p''

let camera_normalize_triangle
    (camera: camera)
    ((p0, p1, p2): triangle): triangle =
  (camera_normalize_point camera p0,
   camera_normalize_point camera p1,
   camera_normalize_point camera p2)

let project_triangle
    (h: i64) (w: i64)
    (view_dist: f32)
    (triangle_unnormalized: triangle)
    (triangle: triangle): triangle_projected =

  let project_point ({x, y, z}: vec3.vector): point_2d =
    let z_ratio = if z >= 0.0
                  then (view_dist + z) / view_dist
                  else 1.0 / ((view_dist - z) / view_dist)
    let x_projected = x / z_ratio + f32.i64 w / 2.0
    let y_projected = y / z_ratio + f32.i64 h / 2.0
    in {x=t32 x_projected, y=t32 y_projected}

  let ({x=x0, y=y0, z=z0}, {x=x1, y=y1, z=z1}, {x=x2, y=y2, z=z2}) = triangle
  let ({x=ux0, y=uy0, z=uz0}, {x=ux1, y=uy1, z=uz1}, {x=ux2, y=uy2, z=uz2}) = triangle_unnormalized
  let {x=xp0, y=yp0} = project_point {x=x0, y=y0, z=z0}
  let {x=xp1, y=yp1} = project_point {x=x1, y=y1, z=z1}
  let {x=xp2, y=yp2} = project_point {x=x2, y=y2, z=z2}

  in ({projected={x=xp0, y=yp0}, world={x=ux0, y=uy0, z=uz0}, z=z0},
      {projected={x=xp1, y=yp1}, world={x=ux1, y=uy1, z=uz1}, z=z1},
      {projected={x=xp2, y=yp2}, world={x=ux2, y=uy2, z=uz2}, z=z2})

-- | Render triangles using expand and reduce_by_index.
let render_projected_triangles [n]
    (h: i64)
    (w: i64)
    (triangles_prepared: [n]triangle_slopes)
    (colours: [n]argb.colour): [h][w]argb.colour =
  -- Store the triangle indices along the found lines and points.
  let aux = 0..<n
  let lines = lines_of_triangles triangles_prepared aux
  let points = points_of_lines lines
  let points' = filter (\(p, _) ->
                          p.projected.x >= 0 && p.projected.x < i32.i64 w && p.projected.y >=0 && p.projected.y < i32.i64 h) points
  let indices = map (\(p, _) -> i64.i32 p.projected.y * w + i64.i32 p.projected.x) points'
  let points'' = map (\(p, aux) -> (p.projected.y * i32.i64 w + p.projected.x, z_inv p.z, p.world.x, p.world.y, z_inv p.world.z, aux)) points'
  let empty = (-1, -f32.inf, -f32.inf, -f32.inf, -f32.inf, -1)

  let update (loca, z_a, x_orig_a, y_orig_a, z_orig_a, ia) (locb, z_b, x_orig_b, y_orig_b, z_orig_b, ib) =
    if ia == -1
    then (locb, z_b, x_orig_b, y_orig_b, z_orig_b, ib)
    else if ib == -1
    then (loca, z_a, x_orig_a, y_orig_a, z_orig_a, ia)
    else if (z_a >= 0 && z_a < z_b) || z_b < 0
    then (loca, z_a, x_orig_a, y_orig_a, z_orig_a, ia)
    else (locb, z_b, x_orig_b, y_orig_b, z_orig_b, ib)

  -- FIXME: Generalize drawing system to pick one of these in a smart way.
  let pixel_color_orig (_loc, _z, _x_orig, _y_orig, _z_orig, i): argb.colour =
    if i == -1
    then argb.white
    else colours[i]

  -- Experiment: Visualize depth buffer
  let pixel_depth z =
    if z < 0
    then 1
    else z / 100000 -- FIXME: don't use constants

  let pixel_color_depth_buffer (_loc, z, _x_orig, _y_orig, _z_orig, _i): argb.colour =
    argb.gray (pixel_depth z)

  -- Experiment: Visualize height
  let pixel_color_y (_loc, z, _x_orig, y_orig, _z_orig, _i): argb.colour =
    let f = (y_orig + 4000) / 8000 -- FIXME: don't use constants
    in hsv_to_rgb ((360 * f) % 360, 1 - pixel_depth z, 0.5)

  let pixel_color = pixel_color_y

  let pixels = replicate (h * w) empty
  let pixels' = reduce_by_index pixels update empty indices points''
  let pixels'' = map pixel_color pixels'
  in unflatten h w pixels''

let project_triangles_in_view
    (h: i64)
    (w: i64)
    (view_dist: f32)
    (draw_dist: f32)
    (camera: camera)
    (triangles_coloured: [](triangle_coloured argb.colour)): [](triangle_slopes, argb.colour) =
  let triangles = map (.triangle) triangles_coloured
  let triangles_camera_normalized = map (camera_normalize_triangle camera) triangles
  let triangles_projected = map2 (project_triangle h w view_dist)
                                 triangles triangles_camera_normalized

  let close_enough_dist (p: point_projected): bool =
    0.0 <= p.z && p.z < draw_dist

  let close_enough_fully_out_of_frame
      ((p0, p1, p2): triangle_projected): bool =
    (p0.projected.x < 0 && p1.projected.x < 0 && p2.projected.x < 0) ||
    (p0.projected.x >= i32.i64 w && p1.projected.x >= i32.i64 w && p2.projected.x >= i32.i64 w) ||
    (p0.projected.y < 0 && p1.projected.y < 0 && p2.projected.y < 0) ||
    (p0.projected.y >= i32.i64 h && p1.projected.y >= i32.i64 h && p2.projected.y >= i32.i64 h)

  let close_enough (triangle: triangle_projected): bool =
    (close_enough_dist triangle.0 ||
     close_enough_dist triangle.1 ||
     close_enough_dist triangle.2) &&
    ! (close_enough_fully_out_of_frame triangle)

  let colours = map (.colour) triangles_coloured
  let (triangles_projected', colours') = unzip (filter (close_enough <-< (.0)) (zip triangles_projected colours))
  let triangles_slopes = prepare_triangles triangles_projected'
  in zip triangles_slopes colours'
