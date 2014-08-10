namespace FSharp.ParsEx.Tests

module Properties =
    open Generators

    open FSharp.ParsEx
    open FSharp.ParsEx.Combinators
    module P = FSharp.ParsEx.Parsers

    open Xunit
    open Xunit.Sdk
    open FsCheck.Xunit

    let private (!<) = List.forall id

    let private results p ts =
        run p ts
        |> Seq.map (fun m -> m.Result)
        |> List.ofSeq

    let private (==) x y =
        if x = y then
            true
        else
            printfn "%A <> %A" x y
            false

    let private manyEven =
        many (filter (fun n -> n % 2 = 0) P.any )

    let private cap n = Seq.truncate n >> Seq.toList


    [<Property (Arbitrary = [| typeof<CustomGenerators> |])>]
    let ``Fail does not match any input`` (ts: list<int>) =
        results fail<int,unit> ts == []

    [<Property (Arbitrary = [| typeof<CustomGenerators> |])>]
    let ``Empty parser matches empty list`` (ts: list<int>) =
        match ts with
        | []    -> results P.empty ts == [()]
        | _     -> results P.empty ts == []


    [<Property (Arbitrary = [| typeof<CustomGenerators> |])>]
    let ``Delay preserves semantic`` (ts: list<int>) =
        [
            P.any |>> (fun x -> [x])
            P.empty |>> (fun _ -> [1])
            manyEven
        ]
        |> List.forall (fun p ->
            results p ts == results (delay (fun _ -> p)) ts 
        )

    [<Property (Arbitrary = [| typeof<CustomGenerators> |])>]
    let ``First result of many any is the full list`` (ts: list<int>) =
        let ts = cap 10 ts
        let rs =  results (many (P.any)) ts
        !<[
            List.length rs == List.length ts + 1
            List.head rs == ts
            List.head (List.rev rs) == []
        ]

    [<Property (Arbitrary = [| typeof<CustomGenerators> |])>]
    let ``Test that prune defaults to first match`` (ts: list<int>) =
        let ts = cap 10 ts
        let p = prune <| (many (P.any))
        let rs =  results p ts
        !<[
            List.length rs == 1
            List.head rs == ts
        ]

    [<Property (Arbitrary = [| typeof<CustomGenerators> |])>]
    let ``TakeWhile is equivalent to Seq takeWhile`` (ts: list<int>) =
        let isEven n = n % 2 = 0
        let rs1 = List.ofSeq <| Seq.takeWhile isEven ts
        let rs2 = List.concat <| results (takeWhile isEven P.any) ts
        rs1 == rs2

    [<Property (Arbitrary = [| typeof<CustomGenerators> |])>]
    let ``Monadic join properties`` (ts: list<int>) =
        [
            P.any<int>
            manyOne (filter (fun x -> x % 2 = 0) P.any<int>) |>> List.sum
        ]
        |> List.forall (fun p ->
            let pp = map (fun x -> filter ((>) x) P.any) p
            let ppp = map result pp

            // join . fmap join = join . join
            let l1 =
                let pa = join (map join ppp) 
                let pb = join (join ppp)
                results pa ts == results pb ts

            // join . fmap return = join . return = id
            let l2 =
                let pa = join <| map result<int,int> p
                let pb = join (result p)
                results pa ts == results pb ts && results pa ts == results p ts

            /// join . fmap (fmap f) = fmap f . join
            let l3 =
                let f x = [x-1; x; x+1]
                let pa = join <| map (map f) pp
                let pb = map f (join pp)
                results pa ts == results pb ts
            l1 && l2 && l3
        )

    [<Property (Arbitrary = [| typeof<CustomGenerators> |])>]
    let ``Test that filter is effectful`` (ts: list<int>) =
        let ts = ts |> Seq.truncate 10 |> List.ofSeq
        let isEven x = x % 2 = 0
        let parseEven = filter isEven P.any
        !<[
            results parseEven ts |> List.forall isEven
            results (many parseEven) ts |> List.concat |> List.forall isEven
        ]

    [<Property (Arbitrary = [| typeof<CustomGenerators> |])>]
    let ``Maybe and maybeFail are inverse`` (ts: list<int>) =
        let p = filter ((>) 0) P.any
        results (maybeFail (maybe p)) ts == results p ts



