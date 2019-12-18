module Center

module Mean =

    let harmonic (A:seq<float>) =
        let invSum = 
            A 
            |> Seq.sumBy( fun x -> 
                if x = 0.0 then 
                    0.0 
                else 
                    1.0 / x )
        
        printfn "invSum: %f" invSum

        (A |> Seq.length |> float) / invSum 

