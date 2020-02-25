module Wav

open System.IO
open Fourier

// F#ified based on yoma's code at https://gist.github.com/yomakkkk/2290864

type Header =
    {
        riffID : byte[]    
        size : uint32 
        wavID      : byte[] 
        fmtID      : byte[] 
        fmtSize    : uint32   
        format     : uint16 
        channels   : uint16
        sampleRate : uint32   
        bytePerSec : uint32   
        blockSize  : uint16 
        bit        : uint16 
        dataID     : byte[] 
        dataSize   : uint32   
    }

type Data =
    {
        header : Header   // meta data
        left   : int16 [] // left channel
        right  : int16 [] // right channel
    }

/// Read in all samples from a wav file
let readAllWav( uri : string ) =
    use inFile = new FileStream( uri, FileMode.Open, FileAccess.Read )
    use reader = new BinaryReader( inFile )

    let header = 
        {
            riffID     = reader.ReadBytes(4)
            size       = reader.ReadUInt32()
            wavID      = reader.ReadBytes(4)
            fmtID      = reader.ReadBytes(4)
            fmtSize    = reader.ReadUInt32()
            format     = reader.ReadUInt16()
            channels   = reader.ReadUInt16()
            sampleRate = reader.ReadUInt32()
            bytePerSec = reader.ReadUInt32()
            blockSize  = reader.ReadUInt16()
            bit        = reader.ReadUInt16()
            dataID     = reader.ReadBytes(4)
            dataSize   = reader.ReadUInt32()
        }

    let N = int (header.dataSize / (uint32 header.blockSize))

    let lData, rData = 
        [| 1 .. N |]
        |> Array.map( fun x -> ( reader.ReadInt16(), reader.ReadInt16()  ) ) 
        |> Array.unzip

    {  header= header
       left= lData 
       right= rData }

/// Compute discrete fourier transform of left channel, output only the lower fraction.
let dFourierL (samples:int) (fraction:int) (dat:Data) =
    let samMax = samples 
    let rows   = samMax / fraction

    let cols = dat.left.Length / samMax

    let spectrogram = Array2D.zeroCreate cols rows

    dat.left
    |> Array.chunkBySize samMax
    |> Array.Parallel.iteri( fun c arr -> 
        printf "."
        if c < cols then
            let xn = 
                [|0 .. arr.Length-1|]
                |> Array.map( fun i -> {Fourier.Imaginary.Re = float (arr.[i]); Fourier.Imaginary.Im = 0.0} )
        
            let fourier = Fourier.forwardDFT xn

            fourier
            |> Array.iteri( fun i x -> 
                if i < rows then
                    spectrogram.[c, i] <- x.AbsSum() ) 
        )

    spectrogram
