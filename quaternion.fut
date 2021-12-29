-- | Quaternions.
--
-- Divided into a module type for modules that implement quaternions,
-- and a parametric module that can construct such modules.

-- | The type of modules that implement a notion of quaternions.
-- Semantically, a quaternions. can be seen as a 4-tuple of numbers
-- (but this need not be the representation).
module type quaternion = {
  -- | The type of the components of the quaternion.
  type real
  -- | The type of quaternions.
  type quaternion

  -- | Construct a quaternion.
  val mk: real -> real -> real -> real -> quaternion

  val +: quaternion -> quaternion -> quaternion
  val -: quaternion -> quaternion -> quaternion
  val *: quaternion -> quaternion -> quaternion

  -- | Conjugate a quaternion.
  val conj: quaternion -> quaternion
  -- | The norm of a quaternion.
  val norm: quaternion -> real
  -- | The reciprocal of a quaternion.
  val rec: quaternion -> quaternion
}

-- | Given a module describing a number type, construct a module
-- implementing quaternions.
module mk_quaternion(T: real): (quaternion with real = T.t
                                           with quaternion = {a: T.t, b: T.t, c: T.t, d: T.t}) = {
  type real = T.t
  type quaternion = {a: T.t, b: T.t, c: T.t, d: T.t}

  def mk (a: real) (b: real) (c: real) (d: real): quaternion = {a, b, c, d}

  def (q1: quaternion) + (q2: quaternion): quaternion =
    T.({a=q1.a + q2.a, b=q1.b + q2.b, c=q1.c + q2.c, d=q1.d + q2.d})
  def (q1: quaternion) - (q2: quaternion): quaternion =
    T.({a=q1.a - q2.a, b=q1.b - q2.b, c=q1.c - q2.c, d=q1.d - q2.d})
  def (q1: quaternion) * (q2: quaternion): quaternion =
    T.({a=q1.a * q2.a - q1.b * q2.b - q1.c * q2.c - q1.d * q2.d,
        b=q1.b * q2.a + q1.a * q2.b + q1.c * q2.d - q1.d * q2.c,
        c=q1.a * q2.c - q1.b * q2.d + q1.c * q2.a + q1.d * q2.b,
        d=q1.a * q2.d + q1.b * q2.c - q1.c * q2.b + q1.d * q2.a})

  def conj ({a, b, c, d}: quaternion): quaternion =
    T.({a, b=neg b, c=neg c, d=neg d})
  def sq ({a, b, c, d}: quaternion): real =
    T.(a * a + b * b + c * c + d * d)
  def norm (q: quaternion): real =
    T.(sqrt (sq q))
  def rec (q: quaternion): quaternion =
    conj q * T.({a=recip (sq q), b=f32 0, c=f32 0, d=f32 0})
}
