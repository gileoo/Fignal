module Playground

open OxyPlot
open Helper
open Halton
open Center
open Wav
open Kernels

let Exp2Diff () =
    
    let X  =  [|-1.0 .. 0.01 .. 1.0|]

    let Y = 
        X
        |> Kernels.Ranged.ExpDiff 0.0 1.0 1.0 2.0 

    let XY = Y |> Array.zip X


    let model =  new PlotModel()

    model.Background <- OxyColor.FromRgb( 255uy, 255uy, 255uy )
    model.IsLegendVisible <- false

    let series = new Series.LineSeries()
    XY |> Array.iter( fun (x,y) -> series.Points.Add( new DataPoint( x, y ) ) )
    model.Series.Add( series )

    showChartAndRun "Exp2Diff Kernel" model |> ignore


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

let DFTWav() =
    let wav = Wav.readAllWav "E:/2020/Audio/MesoTest.wav"
    //printfn "%A" wav

    (*
    let graphLeft = 
        wav.left
        |> Array.mapi( fun i x -> float i, float x )

    let leftModel  = createLineModel graphLeft
    showChartAndRun (sprintf "Wav Left,  size: %d" wav.left.Length) leftModel
    *)

    let timer = System.Diagnostics.Stopwatch.StartNew()

    let nrSamples = 8 * 1024    
    let spectro = Wav.dFourierL nrSamples 32 wav

    printfn "\ndone in %.1f [secs]" timer.Elapsed.TotalSeconds
    
    let spectroModel = createHeatModel (-1.0) (float (spectro.GetLength(0)-1)) ( 1.0 ) spectro
    
    showChartAndRun "Spectro Test" spectroModel


let DFTWavCompact() =
    Wav.readAllWav "E:/2020/Audio/MesoTest.wav"
    |> Wav.dFourierL 8192 32
    |> createHeatModel (-1.0) (1.0) ( 1.0 ) 
    |> showChartAndRun "Spectro Test"



let Halton() =
    
    let size = 4096

    Halton.test 2 size
    Halton.test 3 size

    let exportCppArray prefix (arr:(float*float)[]) =
        use sw = System.IO.File.CreateText( sprintf "./%s.hpp" prefix )
 
        sw.WriteLine (sprintf "#ifndef %s_HPP" (prefix.ToUpper())) 
        sw.WriteLine (sprintf "#define %s_HPP" (prefix.ToUpper())) 
        sw.WriteLine "\n\n#include <vector>"
        sw.WriteLine (sprintf "\n\nstd::vector<float> %s = { " prefix)
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


    let toVec3 (h1) (h2) (radius) =
        let theta = 2.0 * System.Math.PI * h1
        let phi   = acos( 1.0 - 2.0 * h2 )

        let r_sin_phi = radius * sin( phi )

        (   r_sin_phi * cos( theta ),
            r_sin_phi * sin( theta ),
            radius * cos( phi ) ) 

    let exportCppVec3Array prefix (arr:(float*float)[]) =
        use sw = System.IO.File.CreateText( sprintf "./%s.hpp" prefix )
 
        sw.WriteLine (sprintf "#ifndef %s_HPP" (prefix.ToUpper())) 
        sw.WriteLine (sprintf "#define %s_HPP" (prefix.ToUpper())) 
        sw.WriteLine "\n\n#include <vector>"
        sw.WriteLine (sprintf "\n\nstd::vector<float> %s = { " prefix)
        sw.Write "    "

        arr
        |> Array.iteri( fun i x -> 
            let vx, vy, vz = toVec3 (fst x) (snd x) 1.0
            
            if i < arr.Length-1 then
                sw.Write (sprintf "%.15f, %.15f, %.15f, " vx vy vz)
            else
                sw.Write (sprintf "%.15f, %.15f, %.15f " vx vy vz)
            if (i+1) % 10 = 0 then
                sw.Write "\n    "
            )

        sw.WriteLine "};\n#endif"


    let doHaltonPair size a b  =
        let haltonPts = 
            Halton.haltonSeq2 a b size
            |> Seq.toArray

(*
        let splitAndOffset (arr:(float*float)[]) =
            let a, b = arr |> Array.splitAt( arr.Length / 2 )
            let scaleA    = a |> Array.map( fun x -> (0.5 * (fst x), snd x) )
            let scaleOffB = b |> Array.map( fun x -> (0.5 + 0.5 * (fst x), snd x) )

            scaleA |> Array.append scaleOffB

        let haltonDoubleX = splitAndOffset haltonPts         
*)
        let pointsModel = createPointsModel haltonPts (sprintf "Halton%d%d" a b)
        showChartAndRun (sprintf "Halton%d%d" a b) pointsModel
        exportPDF "." (sprintf "Halton%d%d" a b) pointsModel
        exportCppArray (sprintf "halton%d%d" a b) haltonPts 
        exportCppVec3Array (sprintf "haltonVec3%d%d" a b) haltonPts 

    let doRandomPair size seed =
        
        let rnd = System.Random( seed ) 
        
        let randomPts = 
            seq{
                for i = 0 to size-1 do            
                    yield ( rnd.NextDouble(), rnd.NextDouble() )
                }
            |> Seq.toArray

        let pointsModel = createPointsModel randomPts (sprintf "Random%d" seed)
        showChartAndRun (sprintf "Random%d" seed) pointsModel
        exportPDF "." (sprintf "Random%d" seed) pointsModel
        exportCppArray (sprintf "random%d" seed) randomPts
        exportCppVec3Array (sprintf "randomVec3%d" seed) randomPts 
 
(*
        let pointsModelDoubleX = createPointsModel haltonDoubleX (sprintf "Halton2X%d%d" a b)
        showChartAndRun (sprintf "Halton2X%d%d" a b) pointsModelDoubleX
        exportPDF "." (sprintf "Halton2X%d%d" a b) pointsModelDoubleX
        exportCppArray (sprintf "halton2X%d%d" a b) haltonDoubleX 
*)

    let doBlueNoiseTable cols samples =
        
        let blueNoise = BlueNoise.WangTileSet()
        
        blueNoise.Generate( cols, samples, 12 )

        let bluePts =
            seq{
                for t in blueNoise.tiles do
                    for d in t.distribution do
                        yield (float d.x, float d.y )
            }
            |> Seq.toArray

        let pointsModel = createPointsModel bluePts (sprintf "Blue%d%d" cols samples)
        showChartAndRun (sprintf "Blue%d%d" cols samples) pointsModel
        exportPDF "." (sprintf "blue%d%d" cols samples) pointsModel
        exportCppArray (sprintf "blue%d%d" cols samples) bluePts
        exportCppVec3Array (sprintf "blueVec3%d%d" cols samples) bluePts 


    let nr = 4096 // * 4
    
    doHaltonPair nr 2 3
    doHaltonPair nr 3 4
    doHaltonPair nr 4 5
    doHaltonPair nr 5 6

    doRandomPair size 0

    doBlueNoiseTable 1 nr 

let Harmonic() =

    let hMean = 
        [0.0 .. 10.0 ]
        |> Center.Mean.harmonic

    printfn "Harmonic mean of 0 to 10: %f" hMean
