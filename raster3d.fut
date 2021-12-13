import "lib/github.com/athas/matte/colour"
import "types"
import "scanline"

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
let normalize_triangle
    ({position=pc, orientation=po}: camera)
    ((p0, p1, p2): triangle): triangle =

  let normalize_point (p: vec3.vector): vec3.vector =
    let p' = {x= -pc.x, y= -pc.y, z= -pc.z} vec3.+ p
    let p'' = rotate_point {x= -po.x, y= -po.y, z= -po.z} {x=0.0, y=0.0, z=0.0} p'
    in p''

  in (normalize_point p0,
      normalize_point p1,
      normalize_point p2)

let project_triangle
    (h: i64) (w: i64)
    (view_dist: f32)
    (triangle: triangle): triangle_projected =

  let project_point ({x, y, z}: vec3.vector): point_2d =
    let z_ratio = if z >= 0.0
                  then (view_dist + z) / view_dist
                  else 1.0 / ((view_dist - z) / view_dist)
    let x_projected = x / z_ratio + f32.i64 w / 2.0
    let y_projected = y / z_ratio + f32.i64 h / 2.0
    in {x=t32 x_projected, y=t32 y_projected}

  let ({x=x0, y=y0, z=z0}, {x=x1, y=y1, z=z1}, {x=x2, y=y2, z=z2}) = triangle
  let {x=xp0, y=yp0} = project_point {x=x0, y=y0, z=z0}
  let {x=xp1, y=yp1} = project_point {x=x1, y=y1, z=z1}
  let {x=xp2, y=yp2} = project_point {x=x2, y=y2, z=z2}
  in ({x=xp0, y=yp0, z=z0}, {x=xp1, y=yp1, z=z1}, {x=xp2, y=yp2, z=z2})

-- | Render triangles using expand and reduce_by_index.
let render_projected_triangles [n]
    (h: i64)
    (w: i64)
    (triangles_projected: [n]triangle_projected)
    (colours: [n]argb.colour): [h][w]argb.colour =
  -- Store the triangle indices along the found lines and points.
  let aux = 0..<n
  let lines = lines_of_triangles triangles_projected aux
  let points = points_of_lines lines
  let points' = filter (\({x, y, z=_}, _) -> x >= 0 && x < i32.i64 w && y >=0 && y < i32.i64 h) points
  let indices = map (\({x, y, z=_}, _) -> i64.i32 y * w + i64.i32 x) points'
  let points'' = map (\({x, y, z}, aux) -> (y * i32.i64 w + x, z, aux)) points'
  let empty = (-1, -f32.inf, -1)

  let update (loca, z_a, ia) (locb, z_b, ib) =
    if ia == -1
    then (locb, z_b, ib)
    else if ib == -1
    then (loca, z_a, ia)
    else if (z_a >= 0 && z_a < z_b) || z_b < 0
            then (loca, z_a, ia)
            else (locb, z_b, ib)

  let pixel_color (_loc, _z, i): argb.colour =
    if i == -1
    then argb.white
    else colours[i]

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
    (triangles_coloured: [](triangle_coloured argb.colour)): [](triangle_projected, argb.colour) =
  let triangles = map (.triangle) triangles_coloured
  let triangles_normalized = map (normalize_triangle camera) triangles
  let triangles_projected = map (project_triangle h w view_dist)
                                triangles_normalized

  let close_enough_dist ({x=_, y=_, z}: point_projected): bool =
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
  in filter (close_enough <-< (.0)) (zip triangles_projected colours)

let render_triangles_in_view
    (h: i64)
    (w: i64)
    (triangles_in_view: [](triangle_projected, argb.colour)) =
  let (triangles_projected, colours) = unzip triangles_in_view
  in render_projected_triangles h w triangles_projected colours

let render_triangles_in_view'
    (h: i64)
    (w: i64)
    (view_dist: f32)
    (draw_dist: f32)
    (camera: camera)
    (triangles_coloured: [](triangle_coloured argb.colour)): [h][w]argb.colour =
  let triangles_in_view = find_triangles_in_view h w view_dist draw_dist
                                                 camera triangles_coloured
  let (triangles_projected, colours) = unzip triangles_in_view
  in render_projected_triangles h w triangles_projected colours
