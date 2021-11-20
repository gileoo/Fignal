module Kernels


let Hat m s x =
    let xm = (x - m) / s
    if xm >= 0.0 && xm <= 1.0 then
        1.0 - xm
    else if xm >= -1.0 && xm < 0.0 then
        xm + 1.0
    else
        0.0

let Bateman T1 T2 x =
    exp (-x/T2) - exp (-x/T1)

let Gauss m s x =
    exp (-0.5 * ((x - m)/s) ** 2.0) / (sqrt(2.0 * System.Math.PI) * s)


module Ranged =

    let private meanDt (t:float[]) = 
        Array.zeroCreate (t.Length-1)
        |> Array.mapi( fun i x -> t.[i+1] - t.[i] )
        |> Array.average

    let ExpDiff (shift) (a) (T1) (T2) (t:float[]) =
    
        let ac = max 0.0 a
        let ampScaled = 
            if a > 0.0 then
                let mx = T1 * T2 * (log (T1/T2))  / (T1 - T2)
                let ma = exp (-mx/T2) - exp (-mx/T1) |> abs
                ac / ma
            else
                1.0 / (T2-T1) / (meanDt t)

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
        

    let ExpDiffGauss (shift) (a) (T1) (T2) (s) (t:float[]) =
        
        let mDt   = t |> meanDt
        let width = mDt * 4.0 *  s |> ceil
        let tk = [| 1.0 .. 2.0*width+1.0 |]
        let gkTmp = tk |> Array.map( Gauss (width + 1.0) (s*mDt) )
        let maxGk = gkTmp |> Array.max
        let gk = gkTmp |> Array.map( fun x -> x/maxGk * a )
        
        0.0


