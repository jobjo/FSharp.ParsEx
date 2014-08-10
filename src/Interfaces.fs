namespace FSharp.ParsEx

[<AutoOpen>]
module Interfaces =

    /// Represents a successful branch of matching a parser.
    type Match<'T,'R> =
        {
            /// Value produced
            Result : 'R

            /// Consumed tokens
            ConsumedTokens : list<'T>

            /// Remaining tokens
            RemainingTokens : list<'T>

            /// Log messages.
            Log : list<string>
        }

    /// A parser is a function from a list of input tokens to a sequence of
    /// matching results.
    type Parser<'T,'R> = internal {Run : (List<'T> -> seq<Match<'T,'R>>)}

