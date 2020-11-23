module ParliamentMember.Types

open System

type Birth =
    { Date: DateTime
      Place: string }

type School =
    | None
    | Regular of Regular
    | Titular of Titular
and Regular =
    { Name: string
      Year: int
      Field: option<string> }
and Titular =
    { Name: string
      Year: int
      Field: string
      Title: string }

type Person =
    { Id: int
      Name: string
      Image: string
      Party: string
      Club: string
      Votes: int
      Constituency: string
      Birth: Birth
      Occupation: string 
      Education: string
      Schools: School[] }