module Kernels


let Bateman T1 T2 x =
    exp (-x/T2) - exp (-x/T1)


module Ranged =

    let ExpDiff (shift) (a) (T1) (T2) (t:float[]) =
    
        let ac = max 0.0 a
        let ampScaled = 
            if a > 0.0 then
                let mx = T1 * T2 * (log (T1/T2))  / (T1 - T2)
                let ma = exp (-mx/T2) - exp (-mx/T1) |> abs
                ac / ma
            else
                let meanDt = 
                    Array.zeroCreate (t.Length-1)
                    |> Array.mapi( fun i x -> t.[i+1] - t.[i] )
                    |> Array.average
                1.0 / (T2-T1) / meanDt

        if T1 > 0.0 then
            t
            |> Array.map( fun x -> x - shift)
            |> Array.map( Bateman T1 T2 )
            |> Array.map( (*) ampScaled )
            |> Array.map( max 0.0 )
        else
            t
            |> Array.map( fun x -> x - shift)
            |> Array.map( fun x -> ampScaled * ( exp (-x/T2) ) )
            |> Array.map( max 0.0 )
        