import "segm"

let primes (n:i32) =
  (.1) <|
  loop (acc:[]i32,c) = ([],2) while c < n+1 do
    let c2 = if c < i32.f32(f32.sqrt(f32.i32(n+1)))
             then c*c
             else n+1
    let sz (p:i32) = (c2 - p) / p
    let get p i = (2+i)*p - c
    let sieves : []i32 = expand sz get acc -- computed non-primes offset by c
    let vs = replicate (c2-c) 1
    let vs = scatter vs sieves
                     (replicate (length sieves) 0)
    let new = filter (>0) <| map2 (*) vs (c..<c2)
    in (acc ++ new, c2)

let main(n:i32) : i32 =
  primes n |> length
