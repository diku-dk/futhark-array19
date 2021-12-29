import "lib/github.com/diku-dk/lys/lys"
import "types"
import "raster3d"
import "terrain"
import "quaternion"
import "quaternion_euler"

type keys_state = {shift: bool, alt: bool, ctrl: bool, down: bool, up: bool, left: bool, right: bool,
                   pagedown: bool, pageup: bool, space: bool}

type navigation = #mouse | #keyboard

type text_content = (i32, i64, i64, f32, f32, f32, f32, f32, f32, f32, f32, i32)
module lys: lys with text_content = text_content = {
  type~ state = {h: i64, w: i64,
                 view_dist: f32, -- another way of expressing the FOV
                 draw_dist: f32,
                 camera: camera,
                 is_still: bool,
                 triangles_coloured: [](triangle_coloured argb.colour),
                 triangles_in_view: [](triangle_slopes, argb.colour),
                 keys: keys_state,
                 navigation: navigation}

  type text_content = text_content

  def text_format () = "FPS: %d\n"
                       ++ "Triangles (before culling): %d\n"
                       ++ "Triangles (after culling): %d\n"
                       ++ "Position: (%.1f, %.1f, %.1f)\n"
                       ++ "Orientation: (%.1f, %.1f, %.1f)\n"
                       ++ "View distance (FOV): %.1f\n"
                       ++ "Draw distance: %.1f\n"
                       ++ "Navigation: %[Mouse|Keyboard]"

  def text_content (fps: f32) (s: state): text_content =
    (t32 fps, length s.triangles_coloured, length s.triangles_in_view,
     s.camera.position.x, s.camera.position.y, s.camera.position.z,
     s.camera.orientation.x, s.camera.orientation.y, s.camera.orientation.z,
     s.view_dist, s.draw_dist,
     (match s.navigation
      case #mouse -> 0
      case #keyboard -> 1))

  def text_colour = const argb.black

  def grab_mouse = true

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
        keys={shift=false, alt=false, ctrl=false, down=false, up=false, left=false, right=false,
              pagedown=false, pageup=false, space=false},
       navigation=#mouse}

  def resize (h: i64) (w: i64) (s: state) =
    s with h = h with w = w

  def render (s: state) =
    let (triangles_slopes, colours) = unzip s.triangles_in_view
    in render_projected_triangles s.h s.w triangles_slopes colours

  def get_move_factor (td: f32) (s: state): f32 =
    200 * td * if s.keys.shift then 6 else 1

  module camera = {
    def move (move_factor: f32) op (camera: camera): camera =
      let v = rotate_point_inv camera.orientation vec3.zero {x=0, y=0, z=op (f32.i32 0) (5 * move_factor)}
      in camera with position = camera.position vec3.+ v

    def turn (turn: vec3.vector) (camera: camera): camera =
      let q = euler_to_quaternion camera.orientation
      let q_rotation = euler_to_quaternion turn
      let q' = quaternion.(q * q_rotation)
      in camera with orientation = quaternion_to_euler q'

    def turn_y (move_factor: f32) op = turn {x=0, y=op (f32.i32 0) (0.005 * move_factor), z=0}
    def turn_z (move_factor: f32) op = turn {x=0, y=0, z=op (f32.i32 0) (0.005 * move_factor)}
    def turn_x (move_factor: f32) op = turn {x=op (f32.i32 0) (0.005 * move_factor), y=0, z=0}

    def step (navigation: navigation) (move_factor: f32) (keys: keys_state) (camera: camera) =
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

      let when pred action orig = if pred
                                  then action orig
                                  else orig

      let mouse_actions = when keys.space (\(camera, _) -> (move move_factor (+) camera, true))

      let keyboard_actions =
        pick (minus_plus keys.down keys.up) (alt_kind (turn_x move_factor) (move move_factor))
             >-> pick (minus_plus keys.left keys.right) (alt_kind (turn_z move_factor) (turn_y move_factor))

      let use actions =
        id (camera, false)
        |> actions
        |> mouse_actions

      in match navigation
         case #mouse -> use mouse_actions
       case #keyboard -> use keyboard_actions
  }

  def step (td: f32) (s: state) =
    let move_factor = get_move_factor td s
    let (camera', camera_changes) = camera.step s.navigation move_factor s.keys s.camera
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

  def mouse ((x, y): (i32, i32)) (s: state): state =
    match s.navigation
    case #mouse ->
      s with camera = (if s.keys.ctrl
                       then camera.turn_y (get_move_factor (r32 x / 100) s) (+) s.camera -- fixme td
                       else camera.turn_z (get_move_factor (r32 x / 100) s) (+) s.camera)
                      |> camera.turn_x (get_move_factor (-r32 y / 100) s) (+)
        with is_still = false
    case #keyboard -> s

  def keychange (navigation: navigation) (k: i32) (pressed: bool) (keys: keys_state): keys_state =
    let cond (elem: i32) (action_then: () -> keys_state) (action_else: () -> keys_state) (): keys_state =
      if k == elem
      then action_then ()
      else action_else ()

    let common_controls =
      cond SDLK_LSHIFT (\() -> keys with shift = pressed)
           <-< cond SDLK_RSHIFT (\() -> keys with shift = pressed)
           <-< cond SDLK_PAGEDOWN (\() -> keys with pagedown = pressed)
           <-< cond SDLK_PAGEUP (\() -> keys with pageup = pressed)

    let mouse_controls =
      cond SDLK_LCTRL (\() -> keys with ctrl = pressed)
           <-< cond SDLK_RCTRL (\() -> keys with ctrl = pressed)
           <-< cond SDLK_SPACE (\() -> keys with space = pressed)

    let keyboard_controls =
      cond SDLK_LALT (\() -> keys with alt = pressed)
      <-< cond SDLK_RALT (\() -> keys with alt = pressed)
      <-< cond SDLK_DOWN (\() -> keys with down = pressed)
      <-< cond SDLK_UP (\() -> keys with up = pressed)
      <-< cond SDLK_LEFT (\() -> keys with left = pressed)
      <-< cond SDLK_RIGHT (\() -> keys with right = pressed)

    let use controls = (common_controls <| controls <| const keys) ()
    in match navigation
       case #mouse -> use mouse_controls
       case #keyboard -> use keyboard_controls

  def keydown (key: i32) (s: state): state =
    if key == SDLK_TAB
    then s with navigation = match s.navigation
                             case #mouse -> #keyboard
                             case #keyboard -> #mouse
    else s with keys = keychange s.navigation key true s.keys

  def keyup (key: i32) (s: state): state =
    s with keys = keychange s.navigation key false s.keys

  def event (e: event) (s: state): state =
    match e
    case #step td -> step td s
    case #wheel _ -> s
    case #mouse {buttons=_, x, y} -> mouse (x, y) s
    case #keydown {key} -> keydown key s
    case #keyup {key} -> keyup key s
}
