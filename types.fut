import "lib/github.com/athas/vector/vspace"

module vec3 = mk_vspace_3d f32

type point_projected = {x: i32, y: i32, z: f32}
type point_2d = {x: i32, y: i32}
type point_2d_with_z = {x: i32, y: i32, z: f32}

type line = {y: i32, x1: i32, x2: i32, z1: f32, z2: f32}

type triangle = (vec3.vector, vec3.vector, vec3.vector)
type triangle_coloured 'colour = {triangle: triangle, colour: colour}
type triangle_projected = (point_projected, point_projected, point_projected)

type xz = {x: i32, z: f32}
type slope = {x: f32, z: f32}
type triangle_slopes = {p_y: i32,
                         y_subtracted_p_y: {q: i32, r: i32},
                         p: xz,
                         r: xz,
                         s1: slope,
                         s2: slope,
                         s3: slope}
type triangle_slopes_with_amount = (i64, triangle_slopes)

type camera = {position: vec3.vector, orientation: vec3.vector}
