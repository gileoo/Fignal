﻿open Helper

[<EntryPoint>]
let main argv = 

    Wav.readAllWav "E:/2020/Audio/MesoTest.wav"
    |> Wav.dFourierL 8192 32
    |> createHeatModel (-1.0) (1.0) ( 1.0 ) 
    |> showChartAndRun "Spectrogram"

    0 // return an integer exit code
