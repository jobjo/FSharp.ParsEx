namespace FSharp.FinitePatterns.Examples

module BookSummary =

    open FSharp.FinitePatterns.Operators
    module P = FSharp.FinitePatterns.PrettyPrinting

    (*
        This is an example of how to use Patterns to render summaries of 
        book items. The structure of the summary depends on which information
        that is available about a particular item.
    *)

    // The book data type contains partial information about a specific book.
    type BookInfo =
        {
            Title : string
            IsDebut : bool
            Year : option<int>
            Authors : list<string>
            Publisher : option<string>
            SalesTrendLastYear : option<float>
            BestSellerRank : option<int>
            Country : option<string>
            Category : option<string>
        }

    /// Helper function for attaching a percent sign to a string.
    /// Given a string, a literal pattern is returned containing the string
    /// followed by a percent sign.
    let withPercent s = P.withRenderNoSpace (!s <+> !"%")

    /// Utility function for render a sting within parenthesis.
    let withParens x = P.withRenderNoSpace !<[ !"("; !x ; !")"]
    
    /// Given a book info object, returns a pattern representing a summary.
    /// Sentences and structure depends on the available properties.
    let summary book =

        // Basic information about a book.
        let basics =

            // Authors separated by commas.
            let authors = 
                !"by" <? P.commaAnd (List.map (!) book.Authors)
        
            // Debut and category pattern. Fails if category not set.
            let category =
                if book.IsDebut && book.Authors.Length = 1 then
                    !"is a debut" <+> !~book.Category
                else
                    !"is a" <? !~book.Category

            let pubCountry =
                [ book.Publisher; book.Country] 
                |> List.map (!?!~)
                |> P.commaOnly
                |> P.withRenderSpace
            
            // Example of using the choice combinator.
            let catAuths =
                (category ?> authors ?> !"and") <|> authors

            !<[
                !book.Title
                catAuths
                !"was first published" <? (map string !?!~book.Year)
                pubCountry >>= withParens
            ]

        // Best seller rank description.
        let bestSeller =
            !"It is currently number" 
            <? (map string !?!~book.BestSellerRank) 
            ?> !"on the Amazon best sellers rank"

        // Information about sales trend.
        let trend =
            let incrOrDecrBy x =
                !<[
                    !(if x > 0. then "increased" else "decreased")
                    !"by"
                    !(string x)
                ]
                |> P.withRenderSpace
            // Example of using the bind operator for composing operations.
            let trend = !?!~book.SalesTrendLastYear >>= incrOrDecrBy >>= withPercent
            !"Sales" <? trend ?> !"since last year"

        !<[
            P.toSentence basics
            P.toSentence bestSeller
            P.toSentence trend
        ]

    /// Example books.
    let book1 =
        {
            Title = "The Catcher in the Rye"
            Year = Some 1951
            IsDebut = true
            Authors = ["J.D. Salinger"]
            BestSellerRank = Some 32
            Publisher = Some "Little, Brown and Company"
            SalesTrendLastYear = Some 7.
            Country = Some "USA"
            Category = Some "Novel"
        }

    let book2 =
        {
            Title = "Real-World Functional Programming"
            Year = Some 2010
            IsDebut = false
            Authors = ["Tomas Petricek"; "John Skeet"]
            BestSellerRank = None
            Publisher = Some "Manning Publications"
            SalesTrendLastYear = None
            Country = None
            Category = None
        }

    let book3 =
        {
            Title = "How to Make a Tree out of your Finger"
            Year = Some 2012
            IsDebut = false
            Authors = [] // ["David Wayne"; "Laura Gregory"; "Shelly Johnson"]
            BestSellerRank = None
            Publisher = Some "Wayny Day's Publication"
            SalesTrendLastYear = None
            Country = Some "Canada"
            Category = Some "Science and Nature book"
        }

    /// Prints a list of summaries.
    let printBookSummaries() =
        for book in [book1; book2; book3] do
            match P.prettyPrintFirst (summary book) with
            | Some s    -> printfn "%s\n" s
            | _         -> ()