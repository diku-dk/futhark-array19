import "lib/github.com/diku-dk/lys/lys"
import "types"
import "raster3d"
import "terrain"
import "quaternion"

module quaternion = mk_quaternion f32

-- x roll/bank
-- y pitch/heading
-- z yaw/attitude

def euler_to_quaternion ({x, y, z}: vec3.vector): quaternion.quaternion =
  let c1 = f32.cos (y / 2)
  let s1 = f32.sin (y / 2)
  let c2 = f32.cos (z / 2)
  let s2 = f32.sin (z / 2)
  let c3 = f32.cos (x / 2)
  let s3 = f32.sin (x / 2)
  let a = c1 * c2 * c3 - s1 * s2 * s3
  let b = s1 * s2 * c3 + c1 * c2 * s3
  let c = s1 * c2 * c3 + c1 * s2 * s3
  let d = c1 * s2 * c3 - s1 * c2 * s3
  in quaternion.mk a b c d

-- FIXME: edge cases okay?
def quaternion_to_euler (q: quaternion.quaternion): vec3.vector =
  let sqa = q.a * q.a
  let sqb = q.b * q.b
  let sqc = q.c * q.c
  let sqd = q.d * q.d
  let unit = sqa + sqb + sqc + sqd
  let test = q.b * q.c + q.d * q.a
  in if test > 0.499 * unit -- singularity at north pole
     then {x=0, y=2 * f32.atan2 q.b q.a, z=f32.pi / 2}
     else if test < -0.499 * unit -- singularity at south pole
     then {x=0, y= -2 * f32.atan2 q.b q.a, z= -f32.pi / 2}
     else {x=f32.atan2 (2 * q.b * q.a - 2 * q.c * q.d) (-sqb + sqc - sqd + sqa),
           y=f32.atan2 (2 * q.c * q.a - 2 * q.b * q.d) (sqb - sqc - sqd + sqa),
           z=f32.asin (2 * test / unit)}

type keys_state = {shift: bool, alt: bool, down: bool, up: bool, left: bool, right: bool,
                   pagedown: bool, pageup: bool, minus: bool, plus: bool}

type text_content = (i32, i64, i64, f32, f32, f32, f32, f32, f32, f32, f32)
module lys: lys with text_content = text_content = {
  type~ state = {h: i64, w: i64,
                 view_dist: f32, -- another way of expressing the FOV
                 draw_dist: f32,
                 camera: camera,
                 is_still: bool,
                 triangles_coloured: [](triangle_coloured argb.colour),
                 triangles_in_view: [](triangle_slopes, argb.colour),
                 keys: keys_state}

  type text_content = text_content

  def text_format () = "FPS: %d\nTriangles (before culling): %d\nTriangles (after culling): %d\nPosition: (%.1f, %.1f, %.1f)\nOrientation: (%.1f, %.1f, %.1f)\nView distance (FOV): %.1f\nDraw distance: %.1f"

  def text_content (fps: f32) (s: state): text_content =
    (t32 fps, length s.triangles_coloured, length s.triangles_in_view,
     s.camera.position.x, s.camera.position.y, s.camera.position.z,
     s.camera.orientation.x, s.camera.orientation.y, s.camera.orientation.z,
     s.view_dist, s.draw_dist)

  def text_colour = const argb.blue

  def init (terrain_seed: u32) (h: i64) (w: i64): state =
    let view_dist = 600
    let draw_dist = 100000
    let camera = {position={x=150000, y= -4000, z=100000},
                  orientation=vec3.zero}

    let triangles_coloured = generate_terrain 1000 1000 300 100000 64 3 (i32.u32 terrain_seed)
    let triangles_in_view = project_triangles_in_view h w view_dist draw_dist
                                                      camera triangles_coloured
    in {w, h,
        view_dist, draw_dist, camera, is_still=false,
        triangles_coloured, triangles_in_view,
        keys={shift=false, alt=false, down=false, up=false, left=false, right=false,
              pagedown=false, pageup=false, minus=false, plus=false}}

  def render (s: state) =
    let (triangles_slopes, colours) = unzip s.triangles_in_view
    in render_projected_triangles s.h s.w triangles_slopes colours

  def step_camera (move_factor: f32) (keys: keys_state) (camera: camera) =
    let move_camera op (camera: camera): camera =
      let v = rotate_point_inv camera.orientation vec3.zero {x=0, y=0, z=op 0 (5 * move_factor)}
      in camera with position = camera.position vec3.+ v

    let turn_camera (turn: vec3.vector) (camera: camera): camera =
      let q = euler_to_quaternion camera.orientation
      let q_rotation = euler_to_quaternion turn
      let q' = quaternion.(q * q_rotation)
      in camera with orientation = quaternion_to_euler q'

    let turn_camera_y op = turn_camera {x=0, y=op 0 (0.005 * move_factor), z=0}
    let turn_camera_z op = turn_camera {x=0, y=0, z=op 0 (0.005 * move_factor)}
    let turn_camera_x op = turn_camera {x=op 0 (0.005 * move_factor), y=0, z=0}

    let pick dir kind (camera, changes) =
      dir (camera, changes) (\op -> kind (\k -> k op camera))
    let changed f op = (f op, true)
    let alt_kind y n f = if keys.alt
                         then f y
                         else f n
    let minus_plus m p current f = if m
                                   then changed f (-)
                                   else if p
                                   then changed f (+)
                                   else current
    let just k f = f k

    in id (camera, false)
       |> pick (minus_plus keys.down keys.up) (alt_kind turn_camera_x move_camera)
       |> pick (minus_plus keys.left keys.right) (alt_kind turn_camera_z turn_camera_y)

  def step td (s: state) =
    let move_factor = 200 * td * if s.keys.shift then 6 else 1
    let (camera', camera_changes) = step_camera move_factor s.keys s.camera
    let draw_dist' = if s.keys.pageup
                     then s.draw_dist + 5 * move_factor
                     else if s.keys.pagedown
                     then s.draw_dist - 5 * move_factor
                     else s.draw_dist
   in s with camera = camera'
        with draw_dist = draw_dist'
        with is_still = !camera_changes
        with triangles_in_view = if camera_changes || !s.is_still
                                 then project_triangles_in_view s.h s.w s.view_dist s.draw_dist
                                                                camera' s.triangles_coloured
                                 else s.triangles_in_view

  def resize (h: i64) (w: i64) (s: state) =
    s with h = h with w = w

  def keychange k pressed (keys: keys_state): keys_state =
    if k == SDLK_LSHIFT
    then keys with shift = pressed
    else if k == SDLK_RSHIFT
    then keys with shift = pressed
    else if k == SDLK_LALT
    then keys with alt = pressed
    else if k == SDLK_RALT
    then keys with alt = pressed
    else if k == SDLK_DOWN
    then keys with down = pressed
    else if k == SDLK_UP
    then keys with up = pressed
    else if k == SDLK_LEFT
    then keys with left = pressed
    else if k == SDLK_RIGHT
    then keys with right = pressed
    else if k == SDLK_PAGEDOWN
    then keys with pagedown = pressed
    else if k == SDLK_PAGEUP
    then keys with pageup = pressed
    else keys

  def event (e: event) (s: state) =
    match e
    case #step td -> step td s
    case #wheel _ -> s
    case #mouse _ -> s
    case #keydown {key} -> s with keys = keychange key true s.keys
    case #keyup {key} -> s with keys = keychange key false s.keys

  def grab_mouse = false
}

-- ==
-- entry: benchmark
-- compiled input { 800 600 }
entry benchmark (w: i64) (h: i64) =
  lys.init 0 h w |> lys.render
