module Convolution

module Discrete =

    let conv (x:float[]) (k:float[]) =
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


    let longDiv (x:float[]) (k:float[]) =
    
        let y = Array.zeroCreate x.Length
        y.[0] <- x.[0]

        [| 0 .. x.Length-2 |]
        |> Array.iter( fun i ->
            let d   = y.[i] / k.[0]
            [| 1 .. k.Length-1 |]
            |> Array.iter( fun j-> 
                if i+j < y.Length-1 then
                    y.[i+j] <- x.[i+j] - d * k.[j] ) 
            y.[i] <- d
            //printfn "i: %d, d: %.3f, x[i]: %.3f ,y=%A" i d x.[i] y
            )

        (y, x)
