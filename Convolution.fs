module Convolution

module Discrete =

let convolveDiscrete (x:float[]) (k:float[]) =
    Array.zeroCreate (x.Length + (k.Length-1))
    |> Array.mapi( fun i _ -> 
        k
        |> Array.mapi( fun j ke -> 
            let ix = i + j - (k.Length-1) 
            if ix >= 0 && ix < x.Length then
                ke * x.[ix]
            else
                0.0 )
        |> Array.sum )
