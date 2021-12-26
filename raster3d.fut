import "lib/github.com/athas/matte/colour"
import "types"
import "scanline"
import "hsv"

def bubble_point
    (a: point_projected)
    (b: point_projected): (point_projected, point_projected) =
  if b.projected.y < a.projected.y then (b, a) else (a, b)

def normalize_triangle_points ((p, q, r): triangle_projected): triangle_projected =
  let (p, q) = bubble_point p q
  let (q, r) = bubble_point q r
  let (p, q) = bubble_point p q
  in (p, q, r)

def prepare_triangles [n] (triangles: [n]triangle_projected): [n]triangle_slopes =
  map normalize_triangle_points triangles
  |> map (\triangle -> triangle_slopes triangle)

def rotate_point
  (angle: vec3.vector)
  (origo: vec3.vector)
  (p: vec3.vector)
  : vec3.vector =
  let (x, y, z) = (p.x - origo.x, p.y - origo.y, p.z - origo.z)

  let (sin_x, cos_x) = (f32.sin angle.x, f32.cos angle.x)
  let (sin_y, cos_y) = (f32.sin angle.y, f32.cos angle.y)
  let (sin_z, cos_z) = (f32.sin angle.z, f32.cos angle.z)

  -- Y axis.
  let (x, y, z) = (z * sin_y + x * cos_y,
                   y,
                   z * cos_y - x * sin_y)
  -- Z axis.
  let (x, y, z) = (x * cos_z - y * sin_z,
                   x * sin_z + y * cos_z,
                   z)
  -- X axis.
  let (x, y, z) = (x,
                   y * cos_x - z * sin_x,
                   y * sin_x + z * cos_x)

  let (x, y, z) = (origo.x + x, origo.y + y, origo.z + z)
  in {x, y, z}

-- FIXME: Reduce code duplication.
def rotate_point_inv
  (angle: vec3.vector)
  (origo: vec3.vector)
  (p: vec3.vector)
  : vec3.vector =
  let (x, y, z) = (p.x - origo.x, p.y - origo.y, p.z - origo.z)

  let (sin_x, cos_x) = (f32.sin angle.x, f32.cos angle.x)
  let (sin_y, cos_y) = (f32.sin angle.y, f32.cos angle.y)
  let (sin_z, cos_z) = (f32.sin angle.z, f32.cos angle.z)

  -- X axis.
  let (x, y, z) = (x,
                   y * cos_x - z * sin_x,
                   y * sin_x + z * cos_x)
  -- Z axis.
  let (x, y, z) = (x * cos_z - y * sin_z,
                   x * sin_z + y * cos_z,
                   z)
  -- Y axis.
  let (x, y, z) = (z * sin_y + x * cos_y,
                   y,
                   z * cos_y - x * sin_y)

  let (x, y, z) = (origo.x + x, origo.y + y, origo.z + z)
  in {x, y, z}

-- | Translate and rotate all points relative to the camera.
def camera_normalize_triangle
    ({position=pc, orientation=po}: camera)
    ((p, q, r): triangle): triangle =

  let camera_normalize_point (p: vec3.vector): vec3.vector =
    vec3.(p - pc)
    |> rotate_point vec3.(zero - po) vec3.zero

  in (camera_normalize_point p,
      camera_normalize_point q,
      camera_normalize_point r)

def project_triangle
    (h: i64) (w: i64)
    (view_dist: f32)
    (camera: camera)
    (world: triangle): triangle_projected =

  let project_point ({x, y, z}: vec3.vector): point_2d =
    let z_ratio = if z >= 0.0
                  then (view_dist + z) / view_dist
                  else 1.0 / ((view_dist - z) / view_dist)
    let x_projected = x / z_ratio + f32.i64 w / 2.0
    let y_projected = y / z_ratio + f32.i64 h / 2.0
    in {x=t32 x_projected, y=t32 y_projected}

  let normalized = camera_normalize_triangle camera world
  in ({projected=project_point normalized.0, world=world.0, z=normalized.0.z},
      {projected=project_point normalized.1, world=world.1, z=normalized.1.z},
      {projected=project_point normalized.2, world=world.2, z=normalized.2.z})

-- | Project triangles currently visible from the camera.
def project_triangles_in_view
    (h: i64)
    (w: i64)
    (view_dist: f32)
    (draw_dist: f32)
    (camera: camera)
    (triangles_coloured: [](triangle_coloured argb.colour)): [](triangle_slopes, argb.colour) =

  let triangles = map (.triangle) triangles_coloured
  let triangles_projected = map (project_triangle h w view_dist camera) triangles

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
  let (triangles_projected', colours') =
    unzip (filter (close_enough <-< (.0)) (zip triangles_projected colours))
  let triangles_slopes = prepare_triangles triangles_projected'
  in zip triangles_slopes colours'

-- | Render projected triangles using expand and reduce_by_index.
def render_projected_triangles [n]
    (h: i64)
    (w: i64)
    (triangles_prepared: [n]triangle_slopes)
    (colours: [n]argb.colour): [h][w]argb.colour =
  -- Store the triangle indices along the found lines and points.
  let aux = 0..<n
  let lines = lines_of_triangles triangles_prepared aux
  let points = points_of_lines lines
  let points' = filter (\(p, _) ->
                          p.projected.x >= 0 && p.projected.x < i32.i64 w
                          && p.projected.y >=0 && p.projected.y < i32.i64 h) points
  let indices = map (\(p, _) -> i64.i32 p.projected.y * w + i64.i32 p.projected.x) points'
  let points'' = map (\(p, aux) -> ({projected={i=p.projected.y * i32.i64 w + p.projected.x}, z=z_inv p.z,
                                     world={x=p.world.x, y=p.world.y, z=z_inv p.world.z}}, aux)) points'
  let empty = ({projected={i= -1}, z= -f32.inf, world={x= -f32.inf, y= -f32.inf, z= -f32.inf}}, -1)

  let z_check ((a, aux_a): (point_projected_1d, i64))
              ((b, aux_b): (point_projected_1d, i64))
              : (point_projected_1d, i64) =
    if (a.z >= 0 && a.z < b.z) || b.z < 0
    then (a, aux_a)
    else (b, aux_b)

  -- FIXME: Generalize drawing system to pick one of these in a smart way.
  let pixel_color_orig ((p, _aux): (point_projected_1d, i64)): argb.colour =
    if p.projected.i == -1
    then argb.white
    else colours[p.projected.i]

  -- Experiment: Visualize depth buffer
  let pixel_depth (z: f32): f32 =
    if z < 0
    then 1
    else z / 100000 -- FIXME: don't use constants

  let pixel_color_depth_buffer ((p, _aux): (point_projected_1d, i64)): argb.colour =
    argb.gray (pixel_depth p.z)

  -- Experiment: Visualize height
  let pixel_color_y ((p, _aux): (point_projected_1d, i64)): argb.colour =
    let f = (p.world.y + 4000) / 8000 -- FIXME: don't use constants
    in hsv_to_rgb ((360 * f) % 360, 1 - pixel_depth p.z, 0.5)

  let pixel_color = pixel_color_y

  let pixels = replicate (h * w) empty
  -- FIXME: Use reduce_by_index_2d instead.
  let pixels' = reduce_by_index pixels z_check empty indices points''
  let pixels'' = map pixel_color pixels'
  in unflatten h w pixels''
