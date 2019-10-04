module Playground

open OxyPlot
open Helper

let Akima () =
    let X = [|0.0; 1.0; 2.0; 3.0; 4.5; 5.0; 6.0; 7.0; 8.0; 9.0; 10.0|]
    let Y = [|10.0; 10.0; 11.0; 14.0; 10.0; 20.0; 10.5; 15.0; 50.0; 60.0; 85.0|]
    let XY = Y |> Array.zip X

    let TAkima = Akima.PrecomputeSlopes X Y

    let XI = [|0.0 .. 0.1 .. 10.0|] 
    let YI = XI |> Array.map( fun x ->
        let y = Akima.Interpolate X Y x // cubicIpol X Y TAkima x
        //printfn "%.2f: %.2f" x y 
        y )

    let XYI = YI |> Array.zip XI

    let model =  new PlotModel()

    model.Background <- OxyColor.FromRgb( 255uy, 255uy, 255uy )
    model.IsLegendVisible <- false

    let series = new Series.LineSeries()
    XYI |> Array.iter( fun x -> series.Points.Add( new DataPoint( fst x, snd x ) ) )
    model.Series.Add( series )

    let pointSeries = new Series.ScatterSeries()
    XY |> Array.iter( fun x -> pointSeries.Points.Add( new Series.ScatterPoint( fst x, snd x, 5.0 ) ) )
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