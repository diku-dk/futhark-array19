import "lib/github.com/athas/vector/vspace"

module vec3 = mk_vspace_3d f32
type point_2d = {x: i32, y: i32}

type point_projected = {projected: point_2d, z: f32, world: vec3.vector}

type triangle = (vec3.vector, vec3.vector, vec3.vector)
type triangle_coloured 'colour = {triangle: triangle, colour: colour}
type triangle_projected = (point_projected, point_projected, point_projected)

type slope_point = {projected: {x: i32}, z: f32, world: vec3.vector}
type slope = {projected: {x: f32}, z: f32, world: vec3.vector}
type triangle_slopes = {n_lines: i32,
                        p_y: i32,
                        y_subtracted_p_y: {q: i32, r: i32},
                        p: slope_point,
                        r: slope_point,
                        s1: slope,
                        s2: slope,
                        s3: slope}

type line_component = {projected: {x: i32},
                       z: f32,
                       world: vec3.vector}
type line = {n_points: i32,
             y: i32,
             leftmost: line_component,
             step: line_component}

             -- x1: i32,
             -- z1: f32,
             -- x_orig1: f32,
             -- y_orig1: f32,
             -- z_orig1: f32,

             -- x: i32,
             -- z: f32,
             -- x_orig: f32,
             -- y_orig: f32,
             -- z_orig: f32}

type camera = {position: vec3.vector, orientation: vec3.vector}
