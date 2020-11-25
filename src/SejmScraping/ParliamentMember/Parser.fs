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

let private practice (data: string) =
    if data = "brak" then [||]
    else data.Split(',', StringSplitOptions.TrimEntries)

let private scienceDegree (data: string) =
    if String.IsNullOrEmpty data then None
    else data.Split('(', StringSplitOptions.TrimEntries)
        |> Array.head
        |> Some

let private regular parse (parts: string[]) _ =
    let get tokenName =
        let (year, token: string) = parse (Array.last parts) tokenName
        let token = token.Split '-' |> Array.head
        (year, token)
    if Array.length parts = 2 then
        let (year, field) = get "field"
        Some { Name = parts.[0]
               Year = year
               Field = Some (field.Trim())
               Degree = None }
    else
        let (year, name) = get "name"
        Some { Name = name.Trim()
               Year = year
               Field = None
               Degree = None }

let private postgraduate parse (parts: string[]) (tokens: string[]) =
    let (year, field) = parse (Array.last parts) "field"
    Some { Name = parts.[0]
           Year = year
           Field = Some field
           Degree = Some (tokens.[1].TrimStart()) }

let private graduate parse (parts: string[]) (tokens: string[]) =
    if Array.length tokens = 1 then
        regular parse parts tokens
    else
        let (year, degree) = parse (Array.last tokens) "degree"
        let field = tokens.[0].Trim()
        Some { Name = parts.[0]
               Year = year
               Field = Some field
               Degree = Some degree }

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
        let get label =
            match properties.TryGetValue ("lbl" + label) with
            | true, value -> value
            |_ -> String.Empty
        let education = "Wyksztalcenie" |> get
        return { Id = id
                 Name = name info
                 Image = image info
                 Party = "Lista" |> get
                 Practice = "Staz" |> (get >> practice)
                 Club = "Klub" |> get
                 Votes = "Glosy" |> (get >> int)
                 Constituency = "Okreg" |> get
                 Birth = "Urodzony" |> (get >> birth)
                 Occupation = "Zawod" |> get
                 Education = education
                 ScienceDegree = "Tytul" |> (get >> scienceDegree)
                 Schools = "Szkola" |> (get >> school education) }
    }

let private info (input: Async<int * HtmlDocument>) =
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

let private properties (input: Async<int * HtmlNode * HtmlNode[]>) =
    async {
        let! (id, info, items) = input
        return (id, info, items |>
            (Array.map (fun x ->
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
            Async.Catch(input |>
                (info
                >> properties
                >> person))
        return
            match result with
            | Choice1Of2 parsed -> parsed
            | Choice2Of2 error ->
                printfn "Parsing failed for id: %i" id
                raise error
    }