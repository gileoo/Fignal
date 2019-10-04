module Akima

(** Implemented by Marcel Ritter following the equations in 

[Akima 1970] H. Akima, A New Method of Inteprolation and Smooth 
Curve Fitting Based on Local Procedures, Journ. of ACM, Vol 17, 1970.

The border intervals were simplified by using linear interpolation and
the the border tangent also at the first inner nodes.*)

let inline private linear x1 x2 y1 y2 x =
    let k = (y2-y1)/(x2-x1)
    k * (x - x1) + y1

let PrecomputeSlopes (X:float[]) (Y:float[]) =
    if X.Length <> Y.Length then failwith "Incompatible lengths!" else
    
    let M = 
        [|0 .. X.Length-2|]
        |> Array.map( fun i -> (Y.[i+1] - Y.[i]) / (X.[i+1] - X.[i]) )

    [|0 .. X.Length-1|]
    |> Array.map( fun i  ->
        // border cases
        if i <= 1 then 
            M.[0] 
        else if i >= X.Length - 2 then
            M.[M.Length-1]
        else
            // akima tangent
            if M.[i-2] = M.[i-1] then
                if M.[i] = M.[i+1] then
                    if M.[i-1] <> M.[i] then
                        0.5 * (M.[i-1] + M.[i])
                    else
                        M.[i]
                else
                  M.[i-1]             
            else    
                // eq. (1)
                ((abs (M.[i+1]-M.[i])) * M.[i-1] + (abs (M.[i-1] - M.[i-2])) * M.[i]) /
                ( abs (M.[i+1] - M.[i]) + abs (M.[i-1] - M.[i-2]))
        )

let private SlopeAt (X:float[]) (Y:float[]) i =
    if i < 0 || i >= X.Length - 1 then failwith "Index out of Bounds!" else

    let k (j) = 
        (Y.[j+1] - Y.[j]) / (X.[j+1] - X.[j])

    if i <= 1 then 
       k 0
    else if i >= X.Length - 2 then
         k (X.Length-2)
    else
        let m = [| i-2 .. i+1 |] |> Array.map( k )

        // akima tangent
        if m.[0] = m.[1] then
            if m.[2] = m.[3] then
                if m.[1] <> m.[2] then
                    0.5 * (m.[1] + m.[2])
                else
                    m.[2]
            else
              m.[1]             
        else // eq. (1)    
            ((abs (m.[3]-m.[2])) * m.[1] + (abs (m.[1] - m.[0])) * m.[2]) /
            ( abs (m.[3] - m.[2]) + abs (m.[1] - m.[0]))

let InterpolatePrecomputed (X:float[]) (Y:float[]) (slopes: float[]) x =
    if X.Length <> Y.Length || X.Length < 5 || Y.Length < 5 then failwith "Invalid length(s)!" else

    if x <= X.[1] then 
        linear X.[0] X.[1] Y.[0] Y.[1] x 
    else if x >= X.[X.Length-2] then
        linear X.[X.Length-2] X.[X.Length-1] Y.[X.Length-2] Y.[X.Length-1] x 
    else 
        let i = (X |> Array.findIndex( fun xx -> x < xx )) - 1
        let p0 = Y.[i]
        let p1 = slopes.[i]
        let dY = Y.[i+1] - Y.[i]
        let dX = X.[i+1] - X.[i]
        let p2 = (3.0 * dY / dX - 2.0 * slopes.[i] - slopes.[i+1]) / dX
        let p3 = (slopes.[i] + slopes.[i+1] - 2.0 * dY / dX ) / dX / dX

        let ix = x - X.[i]
        p0 + p1 * ix + p2 * ix * ix + p3 * ix * ix * ix 


let Interpolate (X:float[]) (Y:float[]) x =
    if X.Length <> Y.Length || X.Length < 5 || Y.Length < 5 then failwith "Invalid length(s)!" else

    if x <= X.[1] then 
        linear X.[0] X.[1] Y.[0] Y.[1] x 
    else if x >= X.[X.Length-2] then
        linear X.[X.Length-2] X.[X.Length-1] Y.[X.Length-2] Y.[X.Length-1] x 
    else 
        let i = (X |> Array.findIndex( fun xx -> x < xx )) - 1
        let p0 = Y.[i]
        let t1 = SlopeAt X Y i
        let t2 = SlopeAt X Y (i+1)
        let dY = Y.[i+1] - Y.[i]
        let dX = X.[i+1] - X.[i]
        let p2 = ( 3.0 * dY / dX - 2.0 * t1 - t2 ) / dX
        let p3 = (t1 + t2 - 2.0 * dY / dX ) / dX / dX

        let ix = x - X.[i]
        p0 + t1 * ix + p2 * ix * ix + p3 * ix * ix * ix 