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
