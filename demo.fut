import "lib/github.com/diku-dk/lys/lys"
import "types"
import "raster3d"
import "terrain"

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
                  orientation={x=0.1, y=0.2, z=0.2}} --vec3.zero}

    let triangles_coloured = generate_terrain 1000 1000 300 100000 64 3 (i32.u32 terrain_seed)
    let triangles_in_view = project_triangles_in_view h w view_dist draw_dist
                                                      camera triangles_coloured
    in {w, h,
        view_dist, draw_dist, camera, is_still=false,
        triangles_coloured, triangles_in_view,
        keys={shift=false, alt=false, down=false, up=false, left=false, right=false,
              pagedown=false, pageup=false, minus=false, plus=false}}

  let render (s: state) =
    let (triangles_slopes, colours) = unzip s.triangles_in_view
    in render_projected_triangles s.h s.w triangles_slopes colours

  let step_camera (move_factor: f32) (keys: keys_state) (camera: camera) =
    let move_camera op (camera: camera): camera =
      let point = camera.position with z = op camera.position.z (5 * move_factor)
      in camera with position = rotate_point camera.orientation camera.position point

    let turn_camera_y op (camera: camera): camera =
      camera with orientation.y = op camera.orientation.y (0.005 * move_factor)

    let turn_camera_z op (camera: camera): camera =
      -- let z' = op camera.orientation.z (0.005 * move_factor)
      -- in camera with orientation = rotate_point {x=camera.orientation.x, y=camera.orientation.y, z=0} vec3.zero {x=0, y=0, z=z'}
      camera with orientation.z = op camera.orientation.z (0.005 * move_factor)

    -- let cartesian_to_spherical ({x, y, z}: vec3.vector): vec3.vector =
    --   {x=if y == 0 || z == 0 then 0 else f32.atan2 y z,
    --    y=if x == 0 || z == 0 then 0 else f32.atan2 x z,
    --    z=if y == 0 || x == 0 then 0 else f32.atan2 y x}
    --   -- let s = f32.sqrt (x**2 + y**2 + z**2)
    --   -- in {x=s,
    --   --     y=if s == 0 then 0 else f32.acos (z / s),
    --   --     z=if x == 0 then 0 else f32.atan (y / x)}

    -- let spherical_to_cartesian ({x, y, z}: vec3.vector): vec3.vector =
    --   {x=f32.sin y * f32.cos z,
    --    y=f32.cos x * f32.sin z,
    --    z=f32.sin y * f32.cos x}
    --   -- {x=x * f32.sin y * f32.cos z,
    --   --  y=x * f32.sin y * f32.sin z,
    --   --  z=x * f32.cos y}

    let turn_camera_x (op: f32 -> f32 -> f32) (camera: camera): camera =
      camera with orientation.x = op camera.orientation.x (0.005 * move_factor)


      -- let v = spherical_to_cartesian camera.orientation
      --         -- -- |> rotate_point {x=0, y= -camera.orientation.y, z=0} vec3.zero
      --         -- -- |> rotate_point {x=0, y=0, z= -camera.orientation.z} vec3.zero
      --         -- -- |> cartesian_to_spherical
      --         -- -- |> (\(v: vec3.vector) -> v with x = op v.x (0.005 * move_factor))
      --         -- -- |> spherical_to_cartesian
      --         -- -- |> rotate_point {x=0, y=0, z=camera.orientation.z} vec3.zero
      --         -- -- |> rotate_point {x=0, y=camera.orientation.y, z=0} vec3.zero
      --         |> cartesian_to_spherical
      -- in camera with orientation = v


      -- let v = spherical_to_cartesian {x=0.005 * move_factor, y=0, z=0}
      -- let v = {x=0, y=0.0001, z=1}
      -- let v = rotate_point {x=0, y=0, z=0} vec3.zero v
      -- let s = cartesian_to_spherical v
      -- in camera with orientation = camera.orientation vec3.+ s


--      let v = rotate_point {x=0.005 * move_factor, y=0, z=0} vec3.zero v
--      let v = rotate_point {x=0, y=0, z=camera.orientation.z} vec3.zero v




      -- let u = {x=0, y=0, z=1}
      -- let u = rotate_point {x=0, y=camera.orientation.y, z=camera.orientation.z} vec3.zero u
      -- let u = rotate_point {x=0.005 * move_factor, y=0, z=0} vec3.zero u
      -- let d = convert u
      -- -- let d = {x=if u.y == 0 || u.z == 0 then 0 else f32.atan2 u.y u.z,
      -- --          y=0,
      -- --          z=if u.y == 0 || u.x == 0 then 0 else f32.atan2 u.y u.x} -- f32.atan2 u.y u.z

      -- in camera with orientation = d -- camera.orientation vec3.+ d


--       let x' = op camera.orientation.x (0.005 * move_factor)
-- --      let (x, z) = (f32.cos camera.orientation.z * x, f32.sin camera.orientation.z * x)
-- --      let (x, z) = (f32.sin camera.orientation.z * x, f32.cos camera.orientation.z * x)

--       let v = rotate_point {x=0, y=camera.orientation.y, z=camera.orientation.z} vec3.zero {x=0, y=f32.sin x', z=f32.cos x'}
--       in camera with orientation = {x=f32.atan2 v.y v.z, y=0, z=f32.atan2 v.x v.y} --y=f32.atan2 v.z v.x, z=f32.atan2
   --                                v.x v.y}

      -- let (sin_x, cos_x) = (f32.sin 0, f32.cos 0)
      -- let (sin_y, cos_y) = (f32.sin camera.orientation.y, f32.cos camera.orientation.y)
      -- let (sin_z, cos_z) = (f32.sin camera.orientation.z, f32.cos camera.orientation.z)

      -- -- X axis.
      -- let (x1, y1, z1) = (x0,
      --                     y0 * cos_x - z0 * sin_x,
      --                     y0 * sin_x + z0 * cos_x)

      -- -- Y axis.
      -- let (x2, y2, z2) = (z1 * sin_y + x1 * cos_y,
      --                     y1,
      --                     z1 * cos_y - x1 * sin_y)
      -- -- Z axis.
      -- let (x3, y3, z3) = (x2 * cos_z - y2 * sin_z,
      --                     x2 * sin_z + y2 * cos_z,
      --                     z2)

      -- in camera with orientation = {x=x3, y=y3, z=z3}

    let elevate_camera op (camera: camera): camera =
      camera with position.y = op camera.position.y (5 * move_factor)

    let (camera, changes) =
      if keys.down
      then (if keys.alt then turn_camera_x (-) camera else move_camera (-) camera, true)
      else if keys.up
      then (if keys.alt then turn_camera_x (+) camera else move_camera (+) camera, true)
      else (camera, false)
    let (camera, changes) =
      if keys.left
      then (if keys.alt then turn_camera_z (-) camera else turn_camera_y (-) camera, true)
      else if keys.right
      then (if keys.alt then turn_camera_z (+) camera else turn_camera_y (+) camera, true)
      else (camera, changes)
    let (camera, changes) =
      if keys.pagedown
      then (elevate_camera (+) camera, true)
      else if keys.pageup
      then (elevate_camera (-) camera, true)
      else (camera, changes)
    in (camera, changes)

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
