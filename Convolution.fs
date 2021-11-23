module Convolution

module Discrete =

    let conv (x:float[]) (k:float[]) =
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

    let longDiv (x:float[]) (k:float[]) =

        let y = Array.zeroCreate x.Length
        [| 0 .. x.Length-1 |]
        |> Array.iter( fun i -> 
            let ks = k |> Array.splitAt (k.Length-i-1) |> fst
            printfn "i: %d, ks=%A" i ks
            let yd = (x |> Array.splitAt i) |> snd |> Array.mapi( fun j xx -> xx / ks.[j])
            y.[i] <- yd |> Array.min |> max 0.0
            [| i .. x.Length-1 |]
            |> Array.iter( fun ix -> x.[ix] <- x.[ix] - ks.[ix] * y.[i] ) 
            )

        (y, x)