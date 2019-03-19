import "segm"

let primes (n:i32) : []i32 =
  (.1) <|
  loop (acc,c) = ([],2) while c < n+1 do
    let c2 = if c < i32.f32(f32.sqrt(f32.i32(n+1)))
             then c*c
             else n+1
    let is = map (+c) (iota(c2-c))
    let fs = map (\i ->
                   let xs = map (\p -> if i%p==0
                                       then 1
                                       else 0) acc
                   in reduce (+) 0 xs) is
    -- apply the sieve
   let new = filter (\i -> 0i32 == unsafe fs[i-c]) is
   in (acc ++ new, c2)

let main(n:i32) : i32 =
  primes n |> length
