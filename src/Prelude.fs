namespace FSharp.ParsEx

[<AutoOpen>]
module Prelude =

    /// Maps the result field of a match.
    let private mapResult f m = 
        {
            Result = f m.Result; 
            ConsumedTokens = m.ConsumedTokens; 
            RemainingTokens = m.RemainingTokens; 
            Log = m.Log
        }

    /// Build a parser
    let parser<'T,'U> f : Parser<'T,'U> = {Run = f}

    /// Create a new match result.
    let internal mkMatch x =
        {Result = x; ConsumedTokens = []; RemainingTokens = []; Log = []}

    /// Runs a parser
    let rec run p s =
        p.Run (Seq.toList s)

    /// Creates a parser that produce a single value and does not consume any input.
    let result<'T,'U> (x: 'U) : Parser<'T,'U> =  parser <| fun ts -> 
        seq {
            yield {Result = x; ConsumedTokens = []; RemainingTokens = ts; Log = []; }
        }

    /// A parser that always fails.
    let fail<'T,'U> : Parser<'T,'U> = parser <| fun _ -> Seq.empty

    /// Maps over the matches of a parser.
    let mapMatches f p = parser (fun s -> run p s |> Seq.map f)

    /// Delays the generation of the parser until execution time.
    let delay f = parser <| fun ts ->  run (f ()) ts

    /// Map the result of successful matches.
    let map f = mapMatches (mapResult f)

    /// Constraint parser results with the given predicate function.
    let filter f p = parser (run p >> Seq.filter (fun m -> f m.Result))

    /// Parser that returns the remaining tokens.
    let remainingTokens<'T> : Parser<'T,list<'T>> = parser <| fun ts ->
        List.toSeq [{mkMatch ts with RemainingTokens = ts}]

    /// Resets the consumed input for any matches.
    let resetInput p = parser <| fun ts ->
        run p ts
        |> Seq.map (fun m -> {m with RemainingTokens = ts})

    /// Prevents backtracking by always selecting the first match.
    let prune p = parser (run p >> Seq.truncate 1)

    /// Monadic join.
    let rec join (p: Parser<'T,Parser<'T,'U>>) : Parser<'T,'U> = parser <| fun s ->
        seq {
            for m1 in run p s do
                for m2 in run m1.Result m1.RemainingTokens do
                    yield {
                        Result = m2.Result
                        RemainingTokens = m2.RemainingTokens
                        ConsumedTokens = m1.ConsumedTokens @ m2.ConsumedTokens
                        Log = m1.Log @ m2.Log
                    }
        }

    /// Monadic bind.
    let bind p f = join (map f p)
    
    /// Applicative operator.
    let apply fp p = bind fp <| fun f -> bind p <| fun x -> result (f x)

    /// Defaults with given value in case the parser fails.
    let recoverWith d p = parser <| fun ts ->
        let ms = run p ts
        if Seq.isEmpty ms then
            Seq.ofList [{mkMatch d with RemainingTokens = ts}]
        else
            ms

    /// Creates an optional parser defaulting to None when no matches were found.
    let maybe<'T,'R> : Parser<'T,'R> -> Parser<'T,option<'R>> = 
        map Some >> recoverWith None

    /// Given a parser producing an optional value creates parser that fails
    /// on None values.
    let maybeFail (p: Parser<'T,option<'R>>) = parser <| fun ts ->
        run p ts 
        |> Seq.choose (fun m -> 
            if m.Result.IsNone then 
                None 
            else 
                Some {
                    Result = m.Result.Value;
                    RemainingTokens = m.RemainingTokens
                    Log = m.Log
                    ConsumedTokens = m.ConsumedTokens
                }
        )

    /// Runs parsers in sequence and returns results from both.
    let private (<&>) p1 p2 = 
        bind p1 <| fun x ->  bind p2 <| fun y -> result (x,y)

    /// Runs both parsers in sequence and returns the result of the first parser.
    let second p1 p2 = map snd (p1 <&> p2)
    
    /// Runs both parsers in sequence and returns the result of the second parser.
    let first p1 p2 = map fst (p1 <&> p2)

    /// Composes two parser generating functions.
    let compose f1 f2 = fun x -> bind (f1 x) <| fun y -> f2 y

    /// Chooses between a sequence of parsers.
    let either p1 p2 = parser <| fun ts -> seq {yield! run p1 ts; yield! run p2 ts}

    /// Computation expression for parser
    type ParserBuilder() =
        member this.Return (x) = result x
        member this.Delay(f) = delay f
        member this.Combine (p1,p2) = either p1 p2
        member this.Bind (p, f) = bind p f
        member this.ReturnFrom p = p
        member this.Zero () = fail
        member this.Yield(x) = result x
        member this.YieldFrom(p) = p

    /// Builds a parser work-flow.
    let parse = new ParserBuilder ()

    /// Creates a parser that produce a single value and does not consume any input.
    let (!) = result

    /// Monadic bind.
    let (>>=) = bind

    /// Ignores first parser
    let (>>.) = second

    /// Ignores second parser
    let (.>>) = first

    /// Composition operator for parser functions.
    let (>=>) f1 f2 = compose f1 f2

    /// Left-directional composition operator for parser functions.
    let (<=<) f1 f2 = compose f2 f1

    /// Combines parsers applicative style .
    let (<*>) = apply

    /// Maps the result of a parser.
    let (|>>) p f = map f p

    /// Choose between two parsers.
    let (<|>) p1 p2 = either p1 p2
