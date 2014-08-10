namespace FSharp.ParsEx.Examples

module Dates =
    open System
    open System.Globalization
    open FSharp.ParsEx
    open FSharp.ParsEx.Operators
    module P = FSharp.ParsEx.Parsers

    let private toString cs = String(Array.ofList cs)

    /// Parses an integer by taking as many as 
    let integer = 
        P.takeWhile (fun c -> Char.IsDigit c) P.any
        |> filter (List.isEmpty >> not)
        |> map (fun cs -> Int32.Parse <| toString cs)
