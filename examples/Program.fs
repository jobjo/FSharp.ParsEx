namespace FSharp.ParsEx.Examples

module Main =
    
    open FSharp.ParsEx

    [<EntryPoint>]
    let main argv =
        while true do
            let s = System.Console.ReadLine()
            if s = "x" then
                exit 10
            else

                let rs = 
                    run Dates.dateRange s
                    |> Seq.map (fun m -> m.Result)
                    |> List.ofSeq
                printfn "Matches:"
                for r in rs do printfn "%A" r

        0
