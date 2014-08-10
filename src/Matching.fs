namespace FSharp.FinitePatterns

module Matching =

    /// Checks if a pattern matches the given sequence.
    let matchWith p xs =
        let rec go xs = function
            | Empty             -> 
                [xs]
            | Fail              -> 
                []
            | Literal l         ->
                match xs with
                | []        -> []
                | x :: xs   -> if x = l then [xs] else []
            | Choice (p1, p2)   -> 
                go xs p1 @ go xs p2
            | Sequence (p1,p2)  ->
                [ for ys in go xs p1 do yield! go ys p2]
        go (Seq.toList xs) p
        |> List.exists List.isEmpty

