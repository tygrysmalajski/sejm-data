module ParliamentMember.Parser

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open FSharp.Data
open ParliamentMember.Types

let private name (content: HtmlNode) =
    (content.Descendants "h1" |> Seq.head).InnerText()

let private birth (data: string) =
    let tokens = data.Split ','
    { Date = DateTime.Parse(tokens.[0])
      Place = tokens.[1].Trim() }

let private postgraduate parse (tokens: string[]) (parts: string[]) =
    let (year, field) = parse parts.[2] "field"
    Titular { Name = parts.[0]
              Year = year
              Field = field
              Title = tokens.[1].TrimStart() }

let private graduate parse (tokens: string[]) (parts: string[]) =
    let (year, title) = parse tokens.[1] "title"
    Titular { Name = parts.[0]
              Year = year
              Field = parts.[2].Trim()
              Title = title }

let private vocational parse (tokens: string[]) (parts: string[])  =
    let (year, title) = parse tokens.[1] "title"
    Titular { Name = parts.[0]
              Year = year
              Field = parts.[1].Trim()
              Title = title }

let private regular parse data =
    let (year, name) = parse data "name"
    Regular { Name = name
              Year = year }

let private school education (data: string) =
    data.Split "\r\n"
    |> Array.map(fun data ->
        let parse input (tokenName: string) =
            let pattern = sprintf "(?<%s>.*) \((?<year>.*)\)" tokenName |> Regex
            let matched = pattern.Match(input).Groups
            let year = matched.["year"].Value |> int
            let token = matched.[tokenName].Value.TrimStart()
            (year, token)
        let tokens = data.Split '-'
        let parts = tokens.[0].Split ','
        if education = "wyższe" then
            if data.Contains "studia podyplomowe" then
                postgraduate parse tokens parts
            else
                graduate parse tokens parts
        elif education = "średnie zawodowe" then
            vocational parse tokens parts
        else regular parse data)

let private person (info, properties: IDictionary<string, string>) =
    let get label = properties.Item ("lbl" + label)
    let education = "Wyksztalcenie" |> get
    { Name = name info
      Party = "Lista" |> get
      Club = "Klub" |> get
      Votes = "Glosy" |> (get >> int)
      Constituency = "Okreg" |> get
      Birth = "Urodzony" |> (get >> birth)
      Education = education
      Schools = "Szkola" |> (get >> school education)
      Occupation = "Zawod" |> get }

let private parseInfo (personHtml: HtmlDocument) =
    let info =
        personHtml.Descendants "div"
        |> Seq.find (fun x -> x.HasId "title_content")
    (info, info.Descendants "div"
        |> Seq.filter (fun x ->
            let ``class`` = x.AttributeValue "class"
            ``class`` = "partia" || ``class`` = "cv")
        |> Seq.collect (fun x -> x.Descendants "li")
        |> Seq.toArray)

let private parseProperties (info: HtmlNode, items) =
    (info, items
        |> Array.map (fun (x: HtmlNode) ->
            x.Descendants "p"
            |> Seq.toArray
            |> (fun y ->
                (y.[0].AttributeValue "id", y.[1].InnerText())))
        |> dict)

let parse = parseInfo >> parseProperties >> person