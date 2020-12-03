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

let private problem1 (input:string array) =
  let numbers = toIntList input
  let combinations = pairs numbers
  let x, y = combinations |> Seq.find (fun (x, y) -> x + y = 2020)
  printfn "Pair: %i, %i; Product: %i" x y (x * y)


let private problem2 (input: string array) =
  let numbers = toIntList input
  let combinations = triples numbers
  let x, y, z = combinations |> Seq.find (fun (x, y, z) -> x + y + z = 2020)
  printfn "Triple: %i, %i, %i; Product: %i" x y z (x * y * z)

type Solver () =
  static member Problem1 = problem1
  static member Problem2 = problem2