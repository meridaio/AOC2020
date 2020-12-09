module AOC2020.Day6

open FParsec
open System

let line = many1 (anyOf ['a'..'z']) .>> optional newline
let lines = sepBy1 (many line) newline

let getCount joiner (input: string array) =
  match run lines (String.Join("\n", input)) with
  | Success (result, _, _) -> result |> List.sumBy (List.map Set.ofList >> joiner >> Set.count)
  | Failure (error, _, _) -> failwith error

let union = List.fold Set.union Set.empty

let intersect = function
  | x::xs -> List.fold Set.intersect x xs
  | [] -> Set.empty

type Solver () =
  static member Problem1 = getCount union >> printfn "Sum of counts: %i"
  static member Problem2 = getCount intersect >> printfn "Sum of counts: %i"
  