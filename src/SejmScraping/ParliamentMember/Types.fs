module ParliamentMember.Types

open System

type Birth = 
  { Date: DateTime
    Place: string }

type School =
  { Name: string
    Year: int
    Field: option<string>
    Degree: option<string> }

type Person =
  { Id: int
    Name: string
    Image: string
    Party: string
    Practice: string[]
    Club: string
    Votes: int
    Constituency: string
    Birth: Birth
    Occupation: string 
    Education: string
    ScienceDegree: option<string>
    Schools: School[] }