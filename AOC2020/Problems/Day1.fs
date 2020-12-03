module AOC2020.Day1

open System

let private toIntList arr = arr |> List.ofArray |> List.map Int32.Parse

let rec private pairs = function
  | h :: t ->
    seq {
      for x in t do yield h, x
      yield! pairs t
    }
  | _ -> Seq.empty

let rec private triples = function
  | h :: t ->
    seq {
      for y, z in pairs t do yield h, y, z
      yield! triples t
    }
  | _ -> Seq.empty

let private ``day 1 question 1`` (input:string array) =
  let numbers = toIntList input
  let combinations = pairs numbers
  let x, y = combinations |> Seq.find (fun (x, y) -> x + y = 2020)
  printfn "Pair: %i, %i; Product: %i" x y (x * y)


let private ``day 1 question 2`` (input: string array) =
  let numbers = toIntList input
  let combinations = triples numbers
  let x, y, z = combinations |> Seq.find (fun (x, y, z) -> x + y + z = 2020)
  printfn "Triple: %i, %i, %i; Product: %i" x y z (x * y * z)

let day1 = function
  | Problem1 -> ``day 1 question 1``
  | Problem2 -> ``day 1 question 2``