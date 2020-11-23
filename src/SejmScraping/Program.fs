open ParliamentMember.Services
open ParliamentMember.Parser

let processMembers ids chunks =
    ids |>
    (Array.splitInto chunks
        >> Array.collect
            (Array.map (download >> parse)
            >> Async.Parallel
            >> Async.RunSynchronously)
        >> Array.sortBy (fun person -> person.Id)
        >> saveToJson)
    printfn "Processed %i member(s)" (Array.length ids)

[<EntryPoint>]
let main argv =
    processMembers [|1..460|] 5
    0
