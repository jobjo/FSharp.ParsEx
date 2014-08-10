namespace FSharp.ParsEx.Tests

module Generators =
    open FSharp.ParsEx
    open FsCheck

    let intListGen () =
        let rec go n=
            if n <= 0 then
                Gen.constant []
            else
                gen {
                    let! x = Gen.choose (-100,100)
                    let! xs = go (n - 1)
                    return x :: xs
                }
        Gen.sized go

    /// Custom generators.
    type CustomGenerators =
        static member Pattern() = Arb.fromGen (intListGen ())
