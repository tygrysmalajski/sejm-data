open ParliamentMember

let processPerson = Services.download >> Parser.parse

[<EntryPoint>]
let main argv =
    let id = (argv.[0] |> int)
    printfn "%s" ((processPerson id).ToString())
    0
