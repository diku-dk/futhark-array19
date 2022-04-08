import "lib/github.com/athas/vector/vspace"
import "lib/github.com/athas/matte/colour"
import "quaternion"
import "quaternion_euler"

module vec3 = mk_vspace_3d f32
module quaternion = mk_quaternion f32
module qe_conversions = mk_quaternion_euler_conversions f32

type point_2d = {x: i32, y: i32}

type base_component 'projected = {projected: projected, z: f32, world: vec3.vector, bary: vec3.vector}

type point_projected = base_component point_2d

type triangle = (vec3.vector, vec3.vector, vec3.vector)
type triangle_projected = (point_projected, point_projected, point_projected)

type slope_point = base_component {x: i32}
type slope = base_component {x: f32}
type triangle_slopes = {n_lines: i32,
                        y: i32,
                        y_subtracted_p_y: {q: i32, r: i32},
                        p: slope_point,
                        r: slope_point,
                        q: slope_point,
                        s1: slope,
                        s2: slope,
                        s3: slope}

type line_component = base_component {x: i32}
type line = {n_points: i32,
             y: i32,
             leftmost: line_component,
             step: line_component}

type point_projected_1d = base_component {i: i32}

type camera_base 'orientation = {position: vec3.vector, orientation: orientation}
type camera = camera_base vec3.vector -- Euler angles
type camera_quaternion = camera_base quaternion.quaternion

type trig = {sin: vec3.vector, cos: vec3.vector}

type^ pixel_color_function 'a = (point_projected_1d, a) -> argb.colour
