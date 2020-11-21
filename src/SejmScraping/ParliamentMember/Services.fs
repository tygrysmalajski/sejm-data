module ParliamentMember.Services

open FSharp.Data

let download id =
    let id =
        (match id with
        | id when id < 10 -> "00"
        | id when id < 100 -> "0"
        | _ -> "") + id.ToString()
    HtmlDocument.Load (sprintf "https://www.sejm.gov.pl/sejm9.nsf/posel.xsp?id=%s&type=A" id)