module Playground

open OxyPlot
open Helper
open Halton

let Akima () =
    let X  = [|0.0; 1.0; 2.0; 3.0; 4.5; 5.0; 6.0; 7.0; 8.0; 9.0; 10.0|]
    let Y  = [|10.0; 10.0; 11.0; 14.0; 10.0; 20.0; 10.5; 15.0; 50.0; 60.0; 85.0|]
    let XY = Y |> Array.zip X

    
    let XI  = [|0.0 .. 0.1 .. 10.0|] 
    
    // direct computation
    let YI  = XI |> Array.map( Akima.Interpolate X Y )
    let XYI = YI |> Array.zip XI

    // compute via precomputed slopes
    let TAkima = Akima.PrecomputeSlopes X Y
    let YI2 = XI |> Array.map( Akima.InterpolatePrecomputed X Y TAkima )

    // check equality the two methods
    YI 
    |> Array.zip YI2
    |> Array.iter( fun (a,b) -> if a <> b then printfn "Unmatching results: %f <> %f" a b )

    let model =  new PlotModel()

    model.Background <- OxyColor.FromRgb( 255uy, 255uy, 255uy )
    model.IsLegendVisible <- false

    let series = new Series.LineSeries()
    XYI |> Array.iter( fun (x,y) -> series.Points.Add( new DataPoint( x, y ) ) )
    model.Series.Add( series )

    let pointSeries = new Series.ScatterSeries()
    XY |> Array.iter( fun (x,y) -> pointSeries.Points.Add( new Series.ScatterPoint( x, y, 5.0 ) ) )
    model.Series.Add( pointSeries )

    showChartAndRun "Akima Interpolation" model |> ignore



let DFT () =
    let samMax   = 512
    let rangeMul = 1.0

    let xRe = 
        [|0 .. samMax|]
        |> Array.map( fun i -> float (i) / float(samMax)  )
        |> Array.map( (*) (2.0 * System.Math.PI * rangeMul) )

    let rand = System.Random()

    let yRe =
        xRe
        |> Array.map( fun x -> sin (x) + 0.3 * cos ( 19.0 * x) + 0.5  * sin (50.0*x) + 0.5 * rand.NextDouble())

    let wave = 
        xRe
        |> Array.mapi( fun i x -> (x, yRe.[i]) )

    let xn = 
        wave
        |> Array.map( fun (_,y) -> {Fourier.Imaginary.Re = y; Fourier.Imaginary.Im = 0.0} )

    let waveModel = createLineModel wave

    let fourier = Fourier.forwardDFT xn
    
    let fourierRe =
        fourier
        |> Array.mapi( fun i x -> (float(i) / rangeMul, x.AbsSum() ) )
        |> Array.splitAt (fourier.Length/2)
        |> fst

    let fourierModel = createLineModel fourierRe

    showChartAndRun "Wave Test" waveModel
    showChartAndRun "Fourier Test" fourierModel


let Halton() =
    
    let size = 4096

    Halton.test 2 size
    Halton.test 3 size

    let exportCppArray prefix (arr:(float*float)[]) =
        use sw = System.IO.File.CreateText( sprintf "./%s.hpp" prefix )
 
        sw.WriteLine (sprintf "#ifndef %s_HPP" (prefix.ToUpper())) 
        sw.WriteLine (sprintf "#define %s_HPP" (prefix.ToUpper())) 
        sw.WriteLine "\n\n#include <array>"
        sw.WriteLine (sprintf "\n\nstd::array<float,%d> %s = { " (2*arr.Length) prefix)
        sw.Write "    "

        arr
        |> Array.iteri( fun i x -> 
            if i < arr.Length-1 then
                sw.Write (sprintf "%f, %f, " (fst x) (snd x))
            else
                sw.Write (sprintf "%f, %f " (fst x) (snd x))
            if (i+1) % 10 = 0 then
                sw.Write "\n    "
            )

        sw.WriteLine "};\n#endif"

    let doHaltonPair size a b  =
        let haltonPts = 
            Halton.haltonSeq2 a b size
            |> Seq.toArray

        let splitAndOffset (arr:(float*float)[]) =
            let a, b = arr |> Array.splitAt( arr.Length / 2 )
            let scaleA    = a |> Array.map( fun x -> (0.5 * (fst x), snd x) )
            let scaleOffB = b |> Array.map( fun x -> (0.5 + 0.5 * (fst x), snd x) )

            scaleA |> Array.append scaleOffB

        let haltonDoubleX = splitAndOffset haltonPts         

        let pointsModel = createPointsModel haltonPts (sprintf "Halton%d%d" a b)
        showChartAndRun (sprintf "Halton%d%d" a b) pointsModel
        exportPDF "." (sprintf "Halton%d%d" a b) pointsModel
        exportCppArray (sprintf "halton%d%d" a b) haltonPts 

        let pointsModelDoubleX = createPointsModel haltonDoubleX (sprintf "Halton%d%d" a b)
        showChartAndRun (sprintf "Halton%d%d" a b) pointsModelDoubleX
        exportPDF "." (sprintf "Halton%d%d" a b) pointsModelDoubleX
        exportCppArray (sprintf "halton2X%d%d" a b) haltonDoubleX 

    doHaltonPair size 2 3
    doHaltonPair size 3 4
    doHaltonPair size 4 5
