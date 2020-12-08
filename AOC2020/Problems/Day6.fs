module AOC2020.Day6

open FParsec
open System

let line = many1 (anyOf ['a'..'z']) .>> optional newline
let lines = sepBy1 (many line) newline

let getCount (input: string array) joiner =
  match run lines (String.Join("\n", input)) with
  | Success (result, _, _) -> result |> List.sumBy (List.map Set.ofList >> joiner >> Set.count)
  | Failure (error, _, _) -> failwithf "%s" error

let union = List.fold (Set.union) Set.empty

let problem1 (input: string array) =
  let totalCount = getCount input union
  
  printfn "Sum of counts: %i" totalCount

let intersect (l: Set<char> list) =
  List.fold (Set.intersect) (List.head l) (List.tail l)

let problem2 (input: string array) =
  let totalCount = getCount input intersect

  printfn "Sum of counts: %i" totalCount

type Solver () =
  static member Problem1 = problem1
  static member Problem2 = problem2
  