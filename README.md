ParsEx - An Exhaustive parsing library for F#
===========================
ParsEx is a top-down, backtracking, parser combinator library for F#. It's similar to Parsec (Haskell) and FParsec (FSharp).

There are two major differences compared to FParsec:
    1. ParsEx can operate on input sequences of arbitrary type, while FParsec only supports text-based input.
    2. By default ParsEx parsers backtracks up to an arbitrary level.
    3. ParsEx parsers can find all matching rules, not only the first one.
   
You may consider using ParsEx when you need to parse non string-based input or when you 
want to exhaustively search for matching patterns. Arguably, ParsEx is rather a substitute for regular expressions. 
It is primarily intended to be used on relatively small input with overlapping rules for matching patterns.

It supports *applicative* and *monadic* style of composition and only exposes a small number of basic combinators.


