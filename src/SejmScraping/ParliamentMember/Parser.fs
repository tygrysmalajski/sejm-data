module ParliamentMember.Parser

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open FSharp.Data
open ParliamentMember.Types

let private tokenYearPattern tokenName =
    tokenName |> (sprintf "(?<%s>.*) \((?<year>.*)\)" >> Regex)

let private name (content: HtmlNode) =
    (content.Descendants "h1" |> Seq.head).InnerText()

let private image (content: HtmlNode) =
    let img = 
        content.Descendants "div" |>
        (Seq.filter (fun x -> x.AttributeValue "class" = "partia")
        >> Seq.collect (fun x -> x.Descendants "img")
        >> Seq.head)
    img.AttributeValue "src"
    |> (Uri >> (fun uri -> uri.AbsolutePath))

let private birth (data: string) =
    let tokens = data.Split ','
    { Date = DateTime.Parse(tokens.[0])
      Place = tokens.[1].Trim() }

let private education (data: string) =
    let tokenName = "level"
    let pattern = tokenYearPattern tokenName
    pattern.Match(data).Groups.[tokenName].Value

let private regular parse (parts: string[]) _ =
    if Array.length parts = 2 then
        let (year, field: string) = parse (Array.last parts) "field"
        let field = Array.head (field.Split('-'))
        Some { Name = parts.[0]
               Year = year
               Field = Some (field.Trim())
               Title = None }
    else
        let (year, name) = parse (Array.last parts) "name"
        let name = Array.head (name.Split('-'))
        Some { Name = name.Trim()
               Year = year
               Field = None
               Title = None }

let private postgraduate parse (parts: string[]) (tokens: string[]) =
    let (year, field) = parse (Array.last parts) "field"
    Some { Name = parts.[0]
           Year = year
           Field = Some field
           Title = Some (tokens.[1].TrimStart()) }

let private graduate parse (parts: string[]) (tokens: string[]) =
    if Array.length tokens = 1 then
        regular parse parts tokens
    else
        let (year, title) = parse (Array.last tokens) "title"
        let field = tokens.[0].Trim()
        Some { Name = parts.[0]
               Year = year
               Field = Some field
               Title = Some title }

let private (|Postgraduate|Graduate|Regular|None|) (education, schoolData: string) =
    if String.IsNullOrEmpty education then None
    elif education = "wyższe" then
        if schoolData.Contains "studia podyplomowe" || schoolData.Contains "studia doktoranckie" then
            Postgraduate
        else Graduate
    else Regular

let private school education (data: string) =
    data.Split "\r\n" |>
    (Array.map(fun data ->
        let parse input (tokenName: string) =
            let pattern = tokenYearPattern tokenName
            let matched = pattern.Match(input).Groups
            let year = matched.["year"].Value |> int
            let token = matched.[tokenName].Value.TrimStart()
            (year, token)
        let parts = data.Split ','
        let tokens = (Array.last parts).Split '-'
        let school =
            match (education, data) with
            | Postgraduate -> postgraduate
            | Graduate -> graduate
            | Regular -> regular
            | None -> (fun _ _ _ -> None)
        school parse parts tokens)
    >> Array.filter (fun school -> school <> None)
    >> Array.map (fun school -> school.Value))

let private person (input: Async<int * HtmlNode * IDictionary<string, string>>) =
    async {
        let! (id, info, properties) = input
        let get label = properties.Item ("lbl" + label)
        let education = "Wyksztalcenie" |> get
        return { Id = id
                 Name = name info
                 Image = image info
                 Party = "Lista" |> get
                 Club = "Klub" |> get
                 Votes = "Glosy" |> (get >> int)
                 Constituency = "Okreg" |> get
                 Birth = "Urodzony" |> (get >> birth)
                 Occupation = "Zawod" |> get
                 Education = education
                 Schools = "Szkola" |> (get >> school education) }
    }

let private parseInfo (input: Async<int * HtmlDocument>) =
    async {
        let! (id, html) = input
        let info =
            html.Descendants "div"
            |> Seq.find (fun x -> x.HasId "title_content")
        return (id, info, info.Descendants "div" |>
            (Seq.filter (fun x ->
                let ``class`` = x.AttributeValue "class"
                ``class`` = "partia" || ``class`` = "cv")
            >> Seq.collect (fun x -> x.Descendants "li")
            >> Seq.toArray))
    }

let private parseProperties (input: Async<int * HtmlNode * HtmlNode[]>) =
    async {
        let! (id, info, items) = input
        return (id, info, items |>
            (Array.map (fun (x: HtmlNode) ->
                x.Descendants "p" |>
                (Seq.pairwise
                >> Seq.map (fun (id, value) ->
                    (id.AttributeValue "id", value.InnerText()))
                >> Seq.head))
            >> dict))
    }

let parse (input: Async<int * HtmlDocument>) =
    async {
        let! (id, _) = input
        let! result =
            Async.Catch(input
            |> (parseInfo
                >> parseProperties
                >> person))
        return
            match result with
            | Choice1Of2 parsed -> parsed
            | Choice2Of2 error ->
                printfn "Parsing failed for id: %i" id
                raise error
    }