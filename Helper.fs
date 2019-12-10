﻿module Helper

open System.Windows.Forms
open OxyPlot

let showChartAndRun (title) (x:OxyPlot.PlotModel) =
    let plot = new OxyPlot.WindowsForms.PlotView(Model = x )
    plot.Size <- System.Drawing.Size( 700, 500 )
    plot.Dock <- DockStyle.Fill;
    plot.Show()


    let win = new System.Windows.Forms.Form()
    win.Text <- title
    win.FormBorderStyle <- System.Windows.Forms.FormBorderStyle.Sizable
    win.ClientSize <- plot.Size
    win.Controls.Add( plot )
    win.ShowDialog() |> ignore

let createLineModel ( xy : (float*float)[]) =
    let model =  new PlotModel()

    model.Background <- OxyColor.FromRgb( 255uy, 255uy, 255uy )
    model.IsLegendVisible <- false

    let series = new Series.LineSeries()
    xy |> Array.iter( fun x -> series.Points.Add( new DataPoint( fst x, snd x ) ) )
    model.Series.Add( series )

    model


let createPointsModel ( xy : (float*float)[]) =
    let model =  new PlotModel()

    model.Background <- OxyColor.FromRgb( 255uy, 255uy, 255uy )
    model.IsLegendVisible <- false

    let colAxis = 
        new Axes.LinearColorAxis( 
            Position = Axes.AxisPosition.Right, 
            Palette = OxyPalettes.Gray(256),
            Minimum= 0.0,
            Maximum= float xy.Length,
            Key="r" )

    let series = 
        new Series.ScatterSeries(
            MarkerType = MarkerType.Circle )

    xy |> Array.iteri( fun i x -> 
        series.Points.Add( 
            let lIdx = (float i) / (float xy.Length)
            new Series.ScatterPoint( fst x, snd x, 4.0 - 2.0 * lIdx, 0.5 * (float i) ) ) )


    model.DefaultFont <- "Times"
    model.DefaultFontSize <- 30.0

    model.Axes.Add( colAxis )
    model.Series.Add( series )

    model