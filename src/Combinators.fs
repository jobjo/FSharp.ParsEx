namespace FSharp.ParsEx

module Combinators =

    /// Chooses between a sequence of parsers.
    let choose ps = parser <| fun ts -> seq {for p in ps do yield! run p ts}

    /// Given a list of parsers returns a parser producing lists of results.
    let rec sequence ps =
        let rec go = function
            | []        -> result []
            | p :: ps   -> !(fun x xs -> x :: xs) <*> p <*> (go ps)
        Seq.toList ps |> go


    /// Repeat the given parser zero or more times.
    let rec many p = 
        [
            !(fun x xs -> x :: xs) <*> p <*> (delay <| fun _ -> many p)
            result []
        ]
        |> choose

    /// Repeat the given parser one or one times.
    let manyOne p = !(fun x xs -> x :: xs) <*> p <*> many p

    /// Consumes input as long as the given predicate function is matched.
    let takeWhile<'T,'R> f p : Parser<'T,list<'R>> =
        prune <| many (filter f p)

    /// Given a list of parsers returns a parser producing lists of results.
    let (!<) = sequence

    /// Given a list of parsers returns a parser producing lists of results.
    let (!?<) = choose

