module ParliamentMember.Services

open System.IO
open FSharp.Data
open FSharp.Json

let download id =
    let id' =
        (match id with
        | id when id < 10 -> "00"
        | id when id < 100 -> "0"
        | _ -> "") + id.ToString()
    async {
        let! document = HtmlDocument.AsyncLoad (sprintf "https://www.sejm.gov.pl/sejm9.nsf/posel.xsp?id=%s&type=A" id')
        return (id, document)
    }

let saveToJson (members: Types.Person[]) =
    let content =
        {| ImageHost = "https://orka.sejm.gov.pl"
           Members = members |}
    let config = JsonConfig.create(jsonFieldNaming = Json.lowerCamelCase)
    let json = Json.serializeEx config content
    let directory = Directory.CreateDirectory "..\..\data"
    File.WriteAllText(sprintf "%s\parliament-members.json" directory.FullName, json)