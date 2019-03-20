import "lib/github.com/diku-dk/cpprandom/random"

import "types"

module rng = xorshift128plus
module dist = uniform_real_distribution f32 rng

-- | A very simple terrain generator.
let generate_terrain
  (depth: i32)
  (width: i32)
  (size: i32)
  (fluct: f32)
  (seed: i32): []triangle_coloured =
  let size_vert = r32 size / 2 ** 0.5

  -- Generate points.
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
  let rngs = rng.rng_from_seed [seed]
             |> rng.split_rng (depth * width)
             |> unflatten depth width
  let (rngs, points') =
    map2 (map2 (\rng p ->
                  let (rng, y') = dist.rand (-fluct / 2, fluct / 2) rng
                  in (rng, p with y = y')
               )) rngs points |> map unzip |> unzip
  let rng = rng.join_rng (flatten rngs)

  -- Smooth areas with a stencil.
  let points'' =
    tabulate_2d depth width
                (\i j ->
                   let p = points'[i,j]
                   in if i >= 1 && i < depth - 1 && j >= 1 && j < width - 1
                      then p with y = unsafe (
                        points'[i-1,j-1].y + points'[i-1,j].y + points'[i-1,j+1].y +
                        points'[i,  j-1].y +                    points'[i,  j+1].y +
                        points'[i+1,j-1].y + points'[i+1,j].y + points'[i+1,j+1].y) / 8
                      else p
                )

  -- Make triangles.
  let triangles =
    map3 (\row0 row1 i ->
            let (row0', row1') = if i % 2 == 0
                                 then (row0, row1)
                                 else (row1, row0)
            in map4 (\c0 c0t c1 c1t ->
                       let tri0 = (c0, c0t, c1)
                       let tri1 = (c1, c1t, c0t)
                       in [tri0, tri1]
                    ) row0'[:width-1] row0'[1:] row1'[:width-1] row1'[1:]
               |> flatten
         ) points''[:depth-1] points''[1:] (0..<depth-1)

  -- Colour triangles.
  let n_triangles = (depth - 1, 2 * (width - 1))
  let rngs = rng
             |> rng.split_rng (n_triangles.1 * n_triangles.2)
             |> unflatten n_triangles.1 n_triangles.2
  let triangles_coloured =
    map2 (map2 (\rng t ->
                  let (rng, r) = dist.rand (0.3, 0.7) rng
                  let (rng, g) = dist.rand (0.3, 0.7) rng
                  let (_rng, b) = dist.rand (0.3, 0.7) rng
                  let c = argb.from_rgba r g b 1.0
                  in {triangle=t, colour=c}
               )) rngs triangles

  in flatten triangles_coloured
