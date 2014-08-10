namespace FSharp.ParsEx

module Parsers =
    open Combinators

    /// Parser matching empty input.
    let empty<'T,'U> : Parser<'T, unit> = parser <| fun s ->
        match s with
        | []    -> List.toSeq [mkMatch ()]
        | _     -> Seq.empty

    /// Returns the first token without consuming any input.
    let peek<'T> : Parser<'T,'T> = parser <| function
        | []        -> Seq.empty
        | x :: xs   -> Seq.ofList [{mkMatch x with RemainingTokens = x :: xs}]

    /// Creates a parser that logs a message.
    let log msg = result () |> mapMatches (fun m -> {m with Log = m.Log @ msg})

    /// Accept input of a given a predicate
    let accept f = parser <| function
        | t :: ts   ->
            match f t with
            | Some x    -> 
                [{ mkMatch x with RemainingTokens = ts; ConsumedTokens = [t]}]
                |> List.toSeq 
            | None      -> 
                Seq.empty
        | _         ->
            Seq.empty

    /// Matches any token.
    let any<'K> : Parser<'K,'K> = accept Some

    /// Creates a parser matching the given token.
    let token tk = accept <| fun t -> if t = tk then Some () else None

    /// Creates a parser matching a sequence of tokens.
    let tokens<'T when 'T : equality> : seq<'T> -> Parser<'T,unit> =
        Seq.map token >> sequence >> map ignore

