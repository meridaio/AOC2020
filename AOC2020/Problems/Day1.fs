module AOC2020.Day1

open System
open AOC2020.Input

let private toIntList arr = arr |> List.ofArray |> List.map Int32.Parse

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