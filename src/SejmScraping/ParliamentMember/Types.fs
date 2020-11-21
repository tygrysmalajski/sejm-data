module ParliamentMember.Types

open System

type Birth =
    { Date: DateTime
      Place: string }

type School =
    | Regular of Regular
    | Titular of College
and Regular =
    { Name: string
      Year: int }
and College =
    { Name: string
      Year: int
      Field: string
      Title: string }

type Person =
    { Name: string
      Party: string
      Club: string
      Votes: int
      Constituency: string
      Birth: Birth
      Education: string
      Schools: School[]
      Occupation: string }