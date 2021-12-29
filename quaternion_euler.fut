-- | Conversions between quaternions and Euler angles.

-- Based on
-- http://www.euclideanspace.com/maths/algebra/realNormedAlgebra/quaternions/index.htm

import "lib/github.com/athas/vector/vspace"
import "quaternion"

-- x roll/bank
-- y pitch/heading
-- z yaw/attitude

module mk_quaternion_euler_conversions (real: real) = {
  module vec3 = mk_vspace_3d f32
  module quaternion = mk_quaternion f32

  def euler_to_quaternion ({x, y, z}: vec3.vector): quaternion.quaternion =
    let (x', y', z') = ((x / 2, y / 2, z / 2))
    let c1 = f32.cos y'
    let s1 = f32.sin y'
    let c2 = f32.cos z'
    let s2 = f32.sin z'
    let c3 = f32.cos x'
    let s3 = f32.sin x'
    let a = c1 * c2 * c3 - s1 * s2 * s3
    let b = s1 * s2 * c3 + c1 * c2 * s3
    let c = s1 * c2 * c3 + c1 * s2 * s3
    let d = c1 * s2 * c3 - s1 * c2 * s3
    in quaternion.mk a b c d

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
}
