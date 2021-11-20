module Convolution


let convolveDiscrete (x:float[]) (k:float[]) =
    Array.zeroCreate (x.Length + (k.Length-1))
    |> Array.mapi( fun i _ -> 
        k
        |> Array.mapi( fun j ke -> 
            let xIdx = i + j - (k.Length-1) 
            if xIdx >= 0 && xIdx < x.Length then
                ke * x.[xIdx]
            else
                0.0 )
        |> Array.sum )
