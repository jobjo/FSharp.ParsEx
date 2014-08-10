namespace FSharp.ParsEx.Examples

module Dates =
    open System
    open System.Globalization

    open FSharp.ParsEx
    open FSharp.ParsEx.Combinators
    module P = FSharp.ParsEx.Parsers

    let private toString cs = String(Array.ofList cs)

    /// Parses an integer by reading as many digits as possible.
    let integer = 
        takeWhile (fun c -> Char.IsDigit c) P.any
        |> filter (List.isEmpty >> not)
        |> map (fun cs -> Int32.Parse <| toString cs)

    /// Only parses integers in the range of 1900 to 2100 
    let year = filter (fun n -> n >= 1900 && n <= 2100) integer

    /// Matches integers from 1 to 12 or names of months
    let month =
        let name = 
            manyOne P.any
            |> map (fun cs ->
                [1 .. 12] 
                |> List.tryPick (fun m -> 
                    let n = CultureInfo.CurrentCulture.DateTimeFormat.GetMonthName(m)
                    if n = toString cs then Some m else None
                )
            )
            |> maybeFail
        name <|> (filter (fun n -> n >= 1 && n <= 12) integer)

    /// Matches integers between 1 and 31
    let day = filter (fun n -> n >= 1 && n <= 31) integer

    /// Dash is either a '/' or a '\' or a '-'.
    let dash = P.tokens "/" <|> P.tokens "\\" <|> P.tokens "-" <|> P.tokens " "

    /// Parses a date or a partial date.
    let date =
        !?< [

            // Ex: 2001-09-11
            parse {
                let! y = year
                do! dash
                let! m = month
                do! dash
                let! d = day
                return (y,m,d)
            }

            // Ex: 9/11/2001
            parse {
                let! m = month
                do! dash
                let! d = day
                do! dash
                let! y = year
                return (y,m,d)
            }

            // Ex: February 2010
            parse {
                let! m = month
                do! dash
                let! y = year
                return (y,m,1)
            }

            // Ex: 2010 June
            parse {
                let! y = year
                do! dash
                let! m = month
                return (y,m,1)
            }

        ]
        |> filter (fun (y,m,d) ->
            d <= DateTime.DaysInMonth(y,m)
        )

    /// Parses a sequence of one or more spaces.
    let spaces = P.any |> takeWhile ((=) ' ')  |>> ignore

    /// Parsers the string 'from'
    let from = P.tokens "from"

    /// Matches either 'to' or any dash.
    let toOrDash = many (P.tokens "to" <|> dash)

    /// Matches a date range.
    /// Ex: from january 2010 to 2012-12-31
    let dateRange =
        parse {
            do! spaces >>. maybe from >>. spaces
            let! d1 = date
            do! spaces >>. maybe toOrDash >>. spaces
            let! d2 = date
            return (d1,d2)
        }















