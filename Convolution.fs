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

    let deConv (x:float[]) (k:float[]) =
        let y = Array.zeroCreate x.Length
        let r = Array.zeroCreate x.Length
        y.[0] <- x.[0]
        r.[0] <- 0.0

        [| 0 .. x.Length-2 |]
        |> Array.iter( fun i ->
            let d   = y.[i] / k.[0]
            [| 1 .. k.Length-1 |]
            |> Array.iter( fun j-> 
                if i+j < y.Length-1 then
                    y.[i+j] <- x.[i+j] - d * k.[j] ) 
            y.[i] <- d
            r.[i] <- 0.0
            //printfn "i: %d, d: %.3f, x[i]: %.3f ,y=%A" i d x.[i] y
            )

        r.[x.Length-1] <- -y.[x.Length-2]

        (y, r)


    let longDivNN (x:float[]) (k:float[]) =

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