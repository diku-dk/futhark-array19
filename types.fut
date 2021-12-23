import "lib/github.com/athas/vector/vspace"

module vec3 = mk_vspace_3d f32
type point_2d = {x: i32, y: i32}

type base_component 'projected = {projected: projected, z: f32, world: vec3.vector}

type point_projected = base_component point_2d

type triangle = (vec3.vector, vec3.vector, vec3.vector)
type triangle_coloured 'colour = {triangle: triangle, colour: colour}
type triangle_projected = (point_projected, point_projected, point_projected)

type slope_point = base_component {x: i32}
type slope = base_component {x: f32}
type triangle_slopes = {n_lines: i32,
                        y: i32,
                        y_subtracted_p_y: {q: i32, r: i32},
                        p: slope_point,
                        r: slope_point,
                        s1: slope,
                        s2: slope,
                        s3: slope}

type line_component = base_component {x: i32}
type line = {n_points: i32,
             y: i32,
             leftmost: line_component,
             step: line_component}

type point_projected_1d = base_component {i: i32}

type camera = {position: vec3.vector, orientation: vec3.vector}
