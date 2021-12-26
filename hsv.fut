import "lib/github.com/athas/matte/colour"

type hsv = (f32, f32, f32)

def fmod (a: f32) (m: f32): f32 =
  a - r32 (t32 (a / m)) * m -- FIXME: Replace usage

def hsv_to_rgb ((h, s, v): hsv): argb.colour =
  let c = v * s
  let h' = h / 60.0
  let x = c * (1.0 - f32.abs (fmod h' 2.0 - 1.0))
  let (r0, g0, b0) = if 0.0 <= h' && h' < 1.0
                     then (c, x, 0.0)
                     else if 1.0 <= h' && h' < 2.0
                     then (x, c, 0.0)
                     else if 2.0 <= h' && h' < 3.0
                     then (0.0, c, x)
                     else if 3.0 <= h' && h' < 4.0
                     then (0.0, x, c)
                     else if 4.0 <= h' && h' < 5.0
                     then (x, 0.0, c)
                     else if 5.0 <= h' && h' < 6.0
                     then (c, 0.0, x)
                     else (0.0, 0.0, 0.0)
  let m = v - c
  let (r, g, b) = (r0 + m, g0 + m, b0 + m)
  in argb.from_rgba r g b 1.0
