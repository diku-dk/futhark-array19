import "lib/github.com/diku-dk/lys/lys"
import "types"
import "raster3d"
import "terrain"
import "quaternion"
import "quaternion_euler"
import "hsv"

type keys_state = {shift: bool, alt: bool, ctrl: bool, down: bool, up: bool, left: bool, right: bool,
                   pagedown: bool, pageup: bool, space: bool}

type navigation = #mouse | #keyboard

type pixel_color_approach = #by_triangle
                          | #by_depth
                          | #by_height

def n_pixel_color_approaches = 3

def pixel_color_id (approach: pixel_color_approach): i32 =
  match approach
  case #by_triangle -> 0
  case #by_depth -> 1
  case #by_height -> 2

def pixel_color_approach (i: i32): pixel_color_approach =
  match i
  case 0 -> #by_triangle
  case 1 -> #by_depth
  case 2 -> #by_height
  case _ -> assert false #by_triangle

type text_content = (i32, i64, i64, f32, f32, f32, f32, f32, f32, f32, f32, i32, i32)
module lys: lys with text_content = text_content = {
  type~ state = {h: i64, w: i64,
                 view_dist: f32, -- another way of expressing the FOV
                 draw_dist: f32,
                 camera: camera,
                 is_still: bool,
                 triangles_coloured: [](triangle_coloured argb.colour),
                 triangles_in_view: [](triangle_slopes, argb.colour),
                 keys: keys_state,
                 navigation: navigation,
                 pixel_color_approach: pixel_color_approach}

  type text_content = text_content

  def text_format () = "FPS: %d\n"
                       ++ "Triangles (before culling): %d\n"
                       ++ "Triangles (after culling): %d\n"
                       ++ "Position: (%.1f, %.1f, %.1f)\n"
                       ++ "Orientation: (%.1f, %.1f, %.1f)\n"
                       ++ "View distance (FOV): %.1f\n"
                       ++ "Draw distance: %.1f\n"
                       ++ "Navigation: %[Mouse|Keyboard]\n"
                       ++ "Pixel color: By %[triangle|depth|height]"

  def text_content (fps: f32) (s: state): text_content =
    (t32 fps, length s.triangles_coloured, length s.triangles_in_view,
     s.camera.position.x, s.camera.position.y, s.camera.position.z,
     s.camera.orientation.x, s.camera.orientation.y, s.camera.orientation.z,
     s.view_dist, s.draw_dist,
     (match s.navigation
      case #mouse -> 0
      case #keyboard -> 1),
     pixel_color_id s.pixel_color_approach)

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
       navigation=#mouse, pixel_color_approach=#by_triangle}

  def resize (h: i64) (w: i64) (s: state) =
    s with h = h with w = w

  module pixel_color = {
    def pixel_depth (draw_dist: f32) (z: f32): f32 =
      if z < 0
      then 1
      else z / draw_dist

    module by_triangle = {
      type aux = argb.colour
      def empty_aux = argb.white
      def triangles_aux colors = colors
      def pixel_color ((_p, color): (point_projected_1d, argb.colour)): argb.colour = color
    }

    module by_depth = {
      type aux = ()
      def empty_aux = ()
      def triangles_aux [n] (_: [n]triangle_slopes): [n]() = replicate n ()
      def pixel_color (draw_dist: f32) ((p, _aux): (point_projected_1d, ())): argb.colour =
        argb.gray (pixel_depth draw_dist p.z)
    }

    module by_height = {
      type aux = ()
      def empty_aux = ()
      def triangles_aux [n] (_: [n]triangle_slopes): [n]() = replicate n ()
      def pixel_color (draw_dist: f32) ((p, _aux): (point_projected_1d, ())): argb.colour =
        let f = (p.world.y + 4000) / 8000 -- FIXME: don't use constants
        in hsv_to_rgb ((360 * f) % 360, 1 - pixel_depth draw_dist p.z, 0.5)
    }
  }

  def render (s: state) =
    let (triangles_slopes, colours) = unzip s.triangles_in_view
    in match s.pixel_color_approach
       case #by_triangle -> render_projected_triangles
                            s.h s.w triangles_slopes
                            pixel_color.by_triangle.pixel_color
                            (pixel_color.by_triangle.triangles_aux colours)
                            pixel_color.by_triangle.empty_aux
       case #by_depth -> render_projected_triangles
                         s.h s.w triangles_slopes
                         (pixel_color.by_depth.pixel_color s.draw_dist)
                         (pixel_color.by_depth.triangles_aux triangles_slopes)
                         pixel_color.by_depth.empty_aux
       case #by_height -> render_projected_triangles
                          s.h s.w triangles_slopes
                          (pixel_color.by_height.pixel_color s.draw_dist)
                          (pixel_color.by_height.triangles_aux triangles_slopes)
                          pixel_color.by_height.empty_aux

  def get_speed (delta: f32) (shift: bool): f32 =
    delta * if shift then 6 else 1

  module camera = {
    def move (speed: f32) (op: f32 -> f32) (camera: camera): camera =
      let v = rotate_point_inv camera.orientation vec3.zero {x=0, y=0, z=op (5 * speed)}
      in camera with position = camera.position vec3.+ v

    def turn (turn: vec3.vector) (camera: camera): camera =
      let q = euler_to_quaternion camera.orientation
      let q_rotation = euler_to_quaternion turn
      let q' = quaternion.(q * q_rotation)
      in camera with orientation = quaternion_to_euler q'

    def turn_speed = 0.005f32
    def turn_y (speed: f32) (op: f32 -> f32) = turn {x=0, y=op (turn_speed * speed), z=0}
    def turn_z (speed: f32) (op: f32 -> f32) = turn {x=0, y=0, z=op (turn_speed * speed)}
    def turn_x (speed: f32) (op: f32 -> f32) = turn {x=op (turn_speed * speed), y=0, z=0}

    def step (navigation: navigation) (speed: f32) (keys: keys_state) (camera: camera): (camera, bool) =
      let pick dir kind (camera, changes) =
        dir (camera, changes) (\op -> kind (\k -> k op camera))

      let changed f op = (f op, true)

      let alt_kind y n f = if keys.alt
                           then f y
                           else f n

      let minus_plus m p current f = if m
                                     then changed f f32.neg
                                     else if p
                                     then changed f id
                                     else current

      let when pred action orig = if pred
                                  then action orig
                                  else orig

      let mouse_actions = when keys.space (\(camera, _) -> (move speed (id) camera, true))

      let keyboard_actions =
        pick (minus_plus keys.down keys.up) (alt_kind (turn_x speed) (move speed))
             >-> pick (minus_plus keys.left keys.right) (alt_kind (turn_z speed) (turn_y speed))

      in match navigation
         case #mouse -> mouse_actions (camera, false)
         case #keyboard -> keyboard_actions (camera, false)
  }

  def step (td: f32) (s: state): state =
    let factor = 200
    let speed = get_speed (td * factor) s.keys.shift
    let (camera', camera_changes) = camera.step s.navigation speed s.keys s.camera
    let draw_dist' = if s.keys.pageup
                     then s.draw_dist + 5 * speed
                     else if s.keys.pagedown
                     then s.draw_dist - 5 * speed
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
      let factor = 1
      in s with camera = (if s.keys.ctrl
                          then camera.turn_y (get_speed (r32 x * factor) s.keys.shift) id s.camera
                          else camera.turn_z (get_speed (r32 x * factor) s.keys.shift) id s.camera)
                         |> camera.turn_x (get_speed (-r32 y * factor) s.keys.shift) id
        with is_still = false
    case #keyboard -> s

  module keyboard = {
    def change (navigation: navigation) (k: i32) (pressed: bool) (keys: keys_state): keys_state =
      let cond (elem: i32) (action_then: () -> keys_state) (action_else: () -> keys_state) (): keys_state =
        if k == elem
        then action_then ()
        else action_else ()

      let common_controls =
        cond SDLK_LSHIFT (\() -> keys with shift = pressed)
             <-< cond SDLK_RSHIFT (\() -> keys with shift = pressed)
             <-< cond SDLK_LCTRL (\() -> keys with ctrl = pressed)
             <-< cond SDLK_RCTRL (\() -> keys with ctrl = pressed)
             <-< cond SDLK_PAGEDOWN (\() -> keys with pagedown = pressed)
             <-< cond SDLK_PAGEUP (\() -> keys with pageup = pressed)

      let mouse_controls =
        cond SDLK_SPACE (\() -> keys with space = pressed)

      let keyboard_controls =
        cond SDLK_LALT (\() -> keys with alt = pressed)
             <-< cond SDLK_RALT (\() -> keys with alt = pressed)
             <-< cond SDLK_DOWN (\() -> keys with down = pressed)
             <-< cond SDLK_UP (\() -> keys with up = pressed)
             <-< cond SDLK_LEFT (\() -> keys with left = pressed)
             <-< cond SDLK_RIGHT (\() -> keys with right = pressed)

      let use controls = (common_controls <| controls <| const keys) ()
      in if !pressed -- Allow releasing pressed keys
         then use (mouse_controls >-> keyboard_controls)
         else match navigation
              case #mouse -> use mouse_controls
              case #keyboard -> use keyboard_controls

    def down (key: i32) (s: state): state =
      if key == SDLK_TAB
      then if s.keys.ctrl
           then s with pixel_color_approach = pixel_color_approach ((pixel_color_id s.pixel_color_approach + 1)
                                                                    % n_pixel_color_approaches)
           else s with navigation = match s.navigation
                                    case #mouse -> #keyboard
                                    case #keyboard -> #mouse
      else s with keys = change s.navigation key true s.keys

    def up (key: i32) (s: state): state =
      s with keys = change s.navigation key false s.keys
  }

  def event (e: event) (s: state): state =
    match e
    case #step td -> step td s
    case #wheel _ -> s
    case #mouse {buttons=_, x, y} -> mouse (x, y) s
    case #keydown {key} -> keyboard.down key s
    case #keyup {key} -> keyboard.up key s
}
