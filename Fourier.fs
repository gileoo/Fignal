module Fourier


type Imaginary =
    {
        Re : float
        Im : float
    }
        
    member x.AbsSum() = 
        2.0 * (abs x.Re + abs x.Im)
        
    static member (/) (A:Imaginary, B:int) =
        {   Re = A.Re / float( B )
            Im = A.Im / float( B ) }
        
    static member Zero =
        {   Re = 0.0; Im = 0.0 }

// Simple DFT implementation following the decription of 
// http://paulbourke.net/miscellaneous/dft
let private discreteFT (doInverse:bool) (xn: Imaginary[]) =

    let N  = xn.Length
    let XK = Array.init N ( fun i -> Imaginary.Zero )
    let sgn = if doInverse then 1.0 else -1.0

    for i = 0 to N-1 do
        let dt = sgn * 2.0 * System.Math.PI * float(i) / float (N)

        for k = 0 to N-1 do 
            let cosK = cos( float( k ) * dt )
            let sinK = sin( float( k ) * dt )
            XK.[i] <- { 
                Re = XK.[i].Re + ( xn.[k].Re * cosK - xn.[k].Im * sinK )
                Im = XK.[i].Im + ( xn.[k].Re * sinK + xn.[k].Im * cosK ) }
    XK



let forwardDFT (xn: Imaginary[]) = 
    discreteFT true xn
    |> Array.map( fun x -> x / xn.Length )

    
let inverseDFT = discreteFT false

// following Numerical Recipes Chapter 12.2.1
// first not too much refactored
let private four1 (doInverse:bool) (d:float[]) =
    
    let isign = if doInverse then -1 else 1

    let N = d.Length/2

    // check power of 2 size
    if N < 2 || N &&& (N-1) <> 0 then
        let msg = sprintf "four1 requires a power of 2 size input but is: %d" d.Length
        failwith msg

    let nn = N <<< 1 

    let mutable j = 1

    let swapAI (a:float[]) ia ib   = 
        let tmp = a.[ib]
        a.[ib] <- a.[ia]
        a.[ia] <- tmp

    let swap = swapAI d

    [|1 .. 2 .. nn-1|]
    |> Array.iter( fun i -> 
        if j > i then // swap
            swap (j-1) (i-1)
            swap j i
        
        let mutable m = N

        while m >= 2 && j > m do
            j <- j - m
            m <- m >>> 1
        
        j <- j + m
       )

    let mutable mmax = 2

    while nn > mmax do
        let istep = mmax <<< 1
        let theta = float( isign ) * 2.0 * System.Math.PI / float( mmax )
        let temp = sin( 0.5 * theta)
        let wpr = -2.0 * temp * temp
        let wpi = sin(theta)
        let mutable wr = 1.0
        let mutable wi = 0.0
        let mutable wtemp = 0.0
        let mutable m = 1
        while m < mmax  do      // (for (m = 1; m < mmax; m += 2) )
            let mutable i = m
            while i <= nn do    // (for (i = m; i <= nn; i += istep) )
                j <- i + mmax
                let tempr = wr*d.[j-1] - wi*d.[j]
                let tempi = wr*d.[j] + wi*d.[j-1]
                d.[j-1] <- d.[i-1] - tempr
                d.[j]   <- d.[i] - tempi
                d.[i-1] <- d.[i-1] + tempr
                d.[i]   <- d.[i] + tempi
                i <- i + istep
            wtemp <- wr
            wr <- wr * wpr - wi * wpi + wr
            wi <- wi * wpr + wtemp * wpi + wi
            m <- m + 2
        mmax <- istep
  
    d
    
let forwardFFT (data:float[]) =
    four1 false data

let inverseFFT (data:float[]) =
    four1 true data
