import "lib/github.com/diku-dk/lys/lys"
import "types"
import "raster3d"
import "terrain"

type keys_state = {shift: bool, down: bool, up: bool, left: bool, right: bool,
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

  let text_format () = "FPS: %d\nTriangles (before culling): %d\nTriangles (after culling): %d\nPosition: (%.1f, %.1f, %.1f)\nOrientation: (%.1f, %.1f, %.1f)\nView distance (FOV): %.1f\nDraw distance: %.1f"

  let text_content (fps: f32) (s: state): text_content =
    (t32 fps, length s.triangles_coloured, length s.triangles_in_view,
     s.camera.position.x, s.camera.position.y, s.camera.position.z,
     s.camera.orientation.x, s.camera.orientation.y, s.camera.orientation.z,
     s.view_dist, s.draw_dist)

  let text_colour = const argb.blue

  let init (terrain_seed: u32) (h: i64) (w: i64): state =
    let view_dist = 600
    let draw_dist = 100000
    let camera = {position={x=150000, y= -4000, z=100000},
                  orientation={x=0, y=0, z=0}}

    let triangles_coloured = generate_terrain 1000 1000 300 100000 64 3 (i32.u32 terrain_seed)
    let triangles_in_view = project_triangles_in_view h w view_dist draw_dist
                                                      camera triangles_coloured
    in {w, h,
        view_dist, draw_dist, camera, is_still=false,
        triangles_coloured, triangles_in_view,
        keys={shift=false, down=false, up=false, left=false, right=false,
              pagedown=false, pageup=false, minus=false, plus=false}}

  let render (s: state) =
    let (triangles_slopes, colours) = unzip s.triangles_in_view
    in render_projected_triangles s.h s.w triangles_slopes colours

  let step_camera (move_factor: f32) (keys: keys_state) (camera0: camera) =
    let move_camera op (camera : camera) =
      let point = camera.position with z = op camera.position.z (5 * move_factor)
      in camera with position = rotate_point camera.orientation camera.position point

    let turn_camera op (camera : camera) =
      camera with orientation.y = op camera.orientation.y (0.005 * move_factor)

    let elevate_camera op (camera : camera) =
      camera with position.y = op camera.position.y (5 * move_factor)

    let (camera1, changes) =
      if keys.down
      then (move_camera (-) camera0, true)
      else if keys.up
      then (move_camera (+) camera0, true)
      else (camera0, false)
    let (camera2, changes) =
      if keys.left
      then (turn_camera (-) camera1, true)
      else if keys.right
      then (turn_camera (+) camera1, true)
      else (camera1, changes)
    let (camera3, changes) =
      if keys.pagedown
      then (elevate_camera (+) camera2, true)
      else if keys.pageup
      then (elevate_camera (-) camera2, true)
      else (camera2, changes)
    in (camera3, changes)

  let step td (s: state) =
    let move_factor = 200 * td * if s.keys.shift then 6 else 1
    let (camera', camera_changes) = step_camera move_factor s.keys s.camera
    let draw_dist' = if s.keys.plus
                     then s.draw_dist + 5 * move_factor
                     else if s.keys.minus
                     then s.draw_dist - 5 * move_factor
                     else s.draw_dist
   in s with camera = camera'
        with draw_dist = draw_dist'
        with is_still = !camera_changes
        with triangles_in_view = if camera_changes || !s.is_still
                                 then project_triangles_in_view s.h s.w s.view_dist s.draw_dist
                                                                camera' s.triangles_coloured
                                 else s.triangles_in_view

  let resize (h: i64) (w: i64) (s: state) =
    s with h = h with w = w

  let keychange k pressed (keys: keys_state): keys_state =
    if k == SDLK_LSHIFT
    then keys with shift = pressed
    else if k == SDLK_RSHIFT
    then keys with shift = pressed
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
    else if k == SDLK_MINUS
    then keys with minus = pressed
    else if k == SDLK_KP_MINUS
    then keys with minus = pressed
    else if k == SDLK_PLUS
    then keys with plus = pressed
    else if k == SDLK_KP_PLUS
    then keys with plus = pressed
    else keys

  let event (e: event) (s: state) =
    match e
    case #step td -> step td s
    case #wheel _ -> s
    case #mouse _ -> s
    case #keydown {key} -> s with keys = keychange key true s.keys
    case #keyup {key} -> s with keys = keychange key false s.keys

  let grab_mouse = false
}

-- ==
-- entry: benchmark
-- compiled input { 800 600 }
entry benchmark (w: i64) (h: i64) =
  lys.init 0 h w |> lys.render
