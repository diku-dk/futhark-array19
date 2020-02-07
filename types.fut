import "lib/github.com/athas/vector/vspace"
import "lib/github.com/athas/matte/colour"

module vec3 = mk_vspace_3d f32

type point_projected = {x: i32, y: i32, z: f32}
type point_2d = {x: i32, y: i32}
type point_barycentric = vec3.vector

type line = (point_2d, point_2d)

type triangle = (vec3.vector, vec3.vector, vec3.vector)
type triangle_coloured 'colour = {triangle: triangle, colour: colour}
type triangle_projected = (point_projected, point_projected, point_projected)

type camera = {position: vec3.vector, orientation: vec3.vector}
