import "lib/github.com/diku-dk/cpprandom/random"
import "types"

module rnge = xorshift128plus
module dist = uniform_real_distribution f32 rnge

type hsv = (f32, f32, f32)

let fmod (a: f32) (m: f32): f32 =
  a - r32 (t32 (a / m)) * m

let hsv_to_rgb ((h, s, v): hsv): argb.colour =
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

let hsv_average ((h0, s0, v0): hsv) ((h1, s1, v1): hsv): hsv =
  let (h0, h1) = if h0 < h1 then (h0, h1) else (h1, h0)
  let diff_a = h1 - h0
  let diff_b = h0 + 360.0 - h1
  let h = if diff_a < diff_b
          then h0 + diff_a / 2.0
          else fmod (h1 + diff_b / 2.0) 360.0
  let s = (s0 + s1) / 2.0
  let v = (v0 + v1) / 2.0
  in (h, s, v)


let mix (c1: argb.colour) (c2: argb.colour): argb.colour =
  let (r1,g1,b1,a1) = argb.to_rgba c1
  let (r2,g2,b2,a2) = argb.to_rgba c2

  in argb.from_rgba ((r1 + r2) / 2)
                    ((g1 + g2) / 2)
                    ((b1 + b2) / 2)
                    ((a1 + a2) / 2)

let mix8 (a: hsv) (b: hsv) (c: hsv) (d: hsv) (e: hsv) (f: hsv) (g: hsv) (h: hsv): hsv =
  let m = hsv_average
  in m (m (m a b) (m c d)) (m (m e f) (m g h))

-- | A very simple terrain generator.
let generate_terrain
  (depth: i32)
  (width: i32)
  (size: i32)
  (fluct: f32)
  (smooth_iterations_areas: i32)
  (smooth_iterations_colours: i32)
  (seed: i32): [](triangle_coloured argb.colour) =

  -- Generate points.
  let size_vert = r32 size / 2 ** 0.5
  let points =
    map (\i ->
           let indent = (i % 2) * (size / 2)
           in map (\j ->
                     {x=r32 (j * size + indent),
                      y=0,
                      z=r32 i * size_vert}
                  ) (0..<width)
        ) (0..<depth)

  -- Make random spikes.
  let rngs = rnge.rng_from_seed [seed]
             |> rnge.split_rng (depth * width)
             |> unflatten depth width
  let (rngs, points') =
    map2 (map2 (\rng (p: vec3.vector) ->
                  let (rng, y') = dist.rand (-fluct / 2, fluct / 2) rng
                  in (rng, p with y = y')
               )) rngs points |> map unzip |> unzip
  let rng = rnge.join_rng (flatten rngs)

  -- Smooth areas with a stencil.
  let points'' =
    iterate smooth_iterations_areas (\ps ->
      tabulate_2d depth width
                  (\i j ->
                     let p: vec3.vector = ps[i,j]
                     in if i >= 1 && i < depth - 1 && j >= 1 && j < width - 1
                        then p with y = (
                          ps[i-1,j-1].y + ps[i-1,j].y + ps[i-1,j+1].y +
                          ps[i,  j-1].y +               ps[i,  j+1].y +
                          ps[i+1,j-1].y + ps[i+1,j].y + ps[i+1,j+1].y) / 8
                        else p with y = 0
                  )) points'

  -- Make triangles.
  let triangles =
    let m = depth - 1
    let n = width - 1
    let n2 = n * 2
    in map3 (\row0 row1 i ->
               let (row0', row1') = if i % 2 == 0
                                    then (row0, row1)
                                    else (row1, row0)
               in flatten (map4 (\c0 c0t c1 c1t ->
                                   let tri0 = (c0, c0t, c1)
                                   let tri1 = (c1, c1t, c0t)
                                   in [tri0, tri1]
                                ) (row0'[:width-1] :> [n]vec3.vector) (row0'[1:] :> [n]vec3.vector) (row1'[:width-1] :> [n]vec3.vector) (row1'[1:] :> [n]vec3.vector)) :> [n2]triangle) (points''[:depth-1] :> [m][width]vec3.vector) (points''[1:] :> [m][width]vec3.vector) ((0..<depth-1) :> [m]i32)

  -- Colour triangles.
  let max_y = reduce f32.max (-fluct) (flatten triangles |> map (.0.y))
  let min_y = reduce f32.min fluct (flatten triangles |> map (.0.y))

  let (n_triangles0, n_triangles1) = (depth - 1, 2 * (width - 1))
  let triangles = triangles :> [n_triangles0][n_triangles1]triangle
  let rngs = rng
             |> rnge.split_rng (n_triangles0 * n_triangles1)
             |> unflatten n_triangles0 n_triangles1
  let triangles_coloured =
    map2 (map2 (\rng t ->
                  let h = 360.0 * ((t.0.y - min_y) / (max_y - min_y))
                  let (rng, h') = dist.rand (h - 30.0, h + 30.0) rng
                  let (rng, s) = dist.rand (0.5, 1.0) rng
                  let (_rng, v) = dist.rand (0.5, 1.0) rng
                  in {triangle=t, colour=(h', s, v)}
               )) rngs triangles

  -- Smooth the colours.
  let triangles_coloured' =
    iterate smooth_iterations_colours (\(tc: [n_triangles0][n_triangles1]triangle_coloured hsv) ->
      tabulate_2d n_triangles0 n_triangles1
                  (\i j ->
                     let t = tc[i,j]
                     in if i >= 1 && i < n_triangles0 - 1 && j >= 1 && j < n_triangles1 - 1
                        then t with colour = (mix8
                          tc[i-1,j-1].colour tc[i-1,j].colour tc[i-1,j+1].colour
                          tc[i,  j-1].colour                  tc[i,  j+1].colour
                          tc[i+1,j-1].colour tc[i+1,j].colour tc[i+1,j+1].colour)
                        else t
                  )) triangles_coloured

  let triangles_coloured'' = map (map (\t ->
                                         {triangle=t.triangle, colour=hsv_to_rgb t.colour}
                                      )) triangles_coloured'

  in flatten triangles_coloured''
