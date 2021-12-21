import "lib/github.com/athas/matte/colour"
import "types"
import "scanline"
import "hsv"

let prepare_triangles [n] (triangles: [n]triangle_projected): [n]triangle_slopes_with_amount =
  prepare_triangles triangles

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

let rotate_point_invert
  (angle: vec3.vector)
  (origo: vec3.vector)
  (p: vec3.vector)
  : vec3.vector =
  let (x0, y0, z0) = (p.x - origo.x, p.y - origo.y, p.z - origo.z)

  let (sin_x, cos_x) = (f32.sin angle.x, f32.cos angle.x)
  let (sin_y, cos_y) = (f32.sin angle.y, f32.cos angle.y)
  let (sin_z, cos_z) = (f32.sin angle.z, f32.cos angle.z)

  -- Z axis.
  let (x1, y1, z1) = (x0 * cos_z - y0 * sin_z,
                      x0 * sin_z + y0 * cos_z,
                      z0)
  -- Y axis.
  let (x2, y2, z2) = (z1 * sin_y + x1 * cos_y,
                      y1,
                      z1 * cos_y - x1 * sin_y)
  -- X axis.
  let (x3, y3, z3) = (x2,
                      y2 * cos_x - z2 * sin_x,
                      y2 * sin_x + z2 * cos_x)

  let (x', y', z') = (origo.x + x3, origo.y + y3, origo.z + z3)
  in {x=x', y=y', z=z'}

-- | Translate and rotate all points relative to the camera.
let camera_normalize_point ({position=pc, orientation=po}: camera) (p: vec3.vector): vec3.vector =
  let p' = p vec3.+ {x= -pc.x, y= -pc.y, z= -pc.z}
  let p'' = rotate_point {x= -po.x, y= -po.y, z= -po.z} {x=0.0, y=0.0, z=0.0} p'
  in p''

let camera_normalize_point_invert ({position=pc, orientation=po}: camera) (p: vec3.vector): vec3.vector =
  let p' = rotate_point_invert {x= po.x, y= po.y, z= po.z} {x=0.0, y=0.0, z=0.0} p
  let p'' = p' vec3.- {x= -pc.x, y= -pc.y, z= -pc.z}
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


  -- let bubble
  --     (a: (i32, f32))
  --     (b: (i32, f32)): ((i32, f32), (i32, f32)) =
  --   if b.0 < a.0 then (b, a) else (a, b)

  -- let normalize ((p, q, r): ((i32, f32), (i32, f32), (i32, f32))): ((i32, f32), (i32, f32), (i32, f32)) =
  --   let (p, q) = bubble p q
  --   let (q, r) = bubble q r
  --   let (p, q) = bubble p q
  -- in (p, q, r)

  -- let ((_, yo0), (_, yo1), (_, yo2)) = normalize ((yp0, 0), (yp1, 100), (yp2, 200))

  in ({x=xp0, y=yp0, x_orig=ux0, y_orig=uy0, z_orig=uz0, z=z0},
      {x=xp1, y=yp1, x_orig=ux1, y_orig=uy1, z_orig=uz1, z=z1},
      {x=xp2, y=yp2, x_orig=ux2, y_orig=uy2, z_orig=uz2, z=z2})

type point_barycentric = vec3.vector

let barycentric_coordinates
    ({x, y}: point_2d)
    (triangle: triangle_projected): point_barycentric =
  let ({x=xp0, y=yp0, z=_, x_orig=_, y_orig=_, z_orig=_}, {x=xp1, y=yp1, z=_, x_orig=_, y_orig=_, z_orig=_}, {x=xp2, y=yp2, z=_, x_orig=_, y_orig=_, z_orig=_}) = triangle
  let factor = (yp1 - yp2) * (xp0 - xp2) + (xp2 - xp1) * (yp0 - yp2)
  in if factor != 0 -- Avoid division by zero.
     then let a = ((yp1 - yp2) * (x - xp2) + (xp2 - xp1) * (y - yp2))
          let b = ((yp2 - yp0) * (x - xp2) + (xp0 - xp2) * (y - yp2))
          let factor' = r32 factor
          let an = r32 a / factor'
          let bn = r32 b / factor'
          let cn = 1.0 - an - bn
          in {x=an, y=bn, z=cn}
     else {x= -1.0, y= -1.0, z= -1.0}

let interpolate_z
    (triangle: triangle_projected)
    ({x=an, y=bn, z=cn}: point_barycentric): f32 =
  let ({x=_, y=_, z=z0, x_orig=_, y_orig=_, z_orig=_}, {x=_, y=_, z=z1, x_orig=_, y_orig=_, z_orig=_}, {x=_, y=_, z=z2, x_orig=_, y_orig=_, z_orig=_}) = triangle
  in 1 / (an * 1 / z0 + bn * 1 / z1 + cn * 1 / z2)


-- | Render triangles using expand and reduce_by_index.
let render_projected_triangles [n]
    (h: i64)
    (w: i64)
    (camera: camera)
    (triangles_projected: [n]triangle_projected)
    (triangles_prepared: [n]triangle_slopes_with_amount)
    (colours: [n]argb.colour): [h][w]argb.colour =
  -- Store the triangle indices along the found lines and points.
  let aux = 0..<n
  let lines = lines_of_triangles_prepared triangles_prepared aux
  let points = points_of_lines lines
  let points' = filter (\({x, y, z=_, x_orig=_, y_orig=_, z_orig=_}, _) -> x >= 0 && x < i32.i64 w && y >=0 && y < i32.i64 h) points
  let indices = map (\({x, y, z=_, x_orig=_, y_orig=_, z_orig=_}, _) -> i64.i32 y * w + i64.i32 x) points'
  let points'' = map (\({x, y, z, x_orig, y_orig, z_orig}, aux) -> (y * i32.i64 w + x, 1 / z, x_orig, y_orig, z_orig, aux)) points'
  let empty = (-1, -f32.inf, -f32.inf, -f32.inf, -f32.inf, -1)

  let update (loca, z_a, x_orig_a, y_orig_a, z_orig_a, ia) (locb, z_b, x_orig_b, y_orig_b, z_orig_b, ib) =
    if ia == -1
    then (locb, z_b, x_orig_b, y_orig_b, z_orig_b, ib)
    else if ib == -1
    then (loca, z_a, x_orig_a, y_orig_a, z_orig_a, ia)
    else let w = i32.i64 w
         let (pa, pb) = ({y=loca / w, x=loca % w}, {y=locb / w, x=locb % w})
         -- XXX: We really should get rid of this and just calculate the z
         -- values when doing scanline rasterization.
         let (ta, tb) = (triangles_projected[ia], triangles_projected[ib])
         let (bary_a, bary_b) = (barycentric_coordinates pa ta,
                                 barycentric_coordinates pb tb)
--         let (z_a, z_b) = (interpolate_z ta bary_a, interpolate_z tb bary_b)
         in if (z_a >= 0 && z_a < z_b) || z_b < 0
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
  let pixel_color_y (loc, z, _x_orig, y_orig, z_orig, i): argb.colour =
    -- let base = camera_normalize_point_invert camera {x=x_orig, y=y_orig, z=z_orig}

    let z_a = if i == -1
              then -1 else
              let w = i32.i64 w
              let pa = {y=loc / w, x=loc % w}
              let ta = triangles_projected[i]
              let bary_a = barycentric_coordinates pa ta
              in interpolate_z ta bary_a

--    let f = z / 1000 -- (z + 4000) / 8000
    let f = (y_orig + 4000) / 8000
    in hsv_to_rgb ((360 * f) % 360, 1 - pixel_depth z, 0.5) -- FIXME: don't use constants

  let pixel_color = pixel_color_y

  let pixels = replicate (h * w) empty
  let pixels' = reduce_by_index pixels update empty indices points''
  let pixels'' = map pixel_color pixels'
  in unflatten h w pixels''

let find_triangles_in_view
    (h: i64)
    (w: i64)
    (view_dist: f32)
    (draw_dist: f32)
    (camera: camera)
    (triangles_coloured: [](triangle_coloured argb.colour)): [](triangle_projected, triangle_slopes_with_amount, argb.colour) =
  let triangles = map (.triangle) triangles_coloured
  let triangles_camera_normalized = map (camera_normalize_triangle camera) triangles
  let triangles_projected = map2 (project_triangle h w view_dist)
                                triangles triangles_camera_normalized

  let close_enough_dist ({x=_, y=_, z, x_orig=_, y_orig=_, z_orig=_}: point_projected): bool =
    0.0 <= z && z < draw_dist

  let close_enough_fully_out_of_frame
      ((p0, p1, p2): triangle_projected): bool =
    (p0.x < 0 && p1.x < 0 && p2.x < 0) ||
    (p0.x >= i32.i64 w && p1.x >= i32.i64 w && p2.x >= i32.i64 w) ||
    (p0.y < 0 && p1.y < 0 && p2.y < 0) ||
    (p0.y >= i32.i64 h && p1.y >= i32.i64 h && p2.y >= i32.i64 h)

  let close_enough (triangle: triangle_projected): bool =
    (close_enough_dist triangle.0 ||
     close_enough_dist triangle.1 ||
     close_enough_dist triangle.2) &&
    ! (close_enough_fully_out_of_frame triangle)

  let colours = map (.colour) triangles_coloured
  let (triangles_projected', colours') = unzip (filter (close_enough <-< (.0)) (zip triangles_projected colours))
  let triangles_slopes = prepare_triangles triangles_projected'
  in zip3 triangles_projected' triangles_slopes colours'

let render_triangles_in_view_prepared
    (h: i64)
    (w: i64)
    (camera: camera)
    (triangles_in_view: [](triangle_projected, triangle_slopes_with_amount, argb.colour)) =
  let (triangles_projected, triangles_slopes, colours) = unzip3 triangles_in_view
  in render_projected_triangles h w camera triangles_projected triangles_slopes colours

-- let render_triangles_in_view
--     (h: i64)
--     (w: i64)
--     (camera: camera)
--     (triangles_in_view: [](triangle_projected, argb.colour)) =
--   let (triangles_projected, colours) = unzip triangles_in_view
--   in render_triangles_in_view_prepared h w camera triangles_projected (zip (prepare_triangles triangles_projected) colours)
