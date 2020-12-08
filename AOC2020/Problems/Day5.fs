module AOC2020.Day5

open System

let parseBin (l: string) =
  let toDigit = function | 'F' -> "0" | 'B' -> "1" | 'L' -> "0" | 'R' -> "1" | c -> failwithf "invalid char %c" c
  let s =
    l.ToCharArray()
    |> Seq.map toDigit
    |> String.concat ""
  Convert.ToInt32(s, 2)

let problem1 (input: string array) =
  let max = input |> Seq.map parseBin |> Seq.max
  printfn "Max ID is %i" max

let problem2 (input: string array) =
  let seats = input |> Seq.map parseBin |> Seq.sort
  let succ x = x + 1
  // compare seat + 1 with the next seat until we get a mismatch
  let missing, _ = Seq.zip (Seq.map succ seats) (Seq.tail seats) |> Seq.find (fun (x, y) -> succ x <> y)

  printfn "Missing seat ID: %i" missing

type Solver () =
  static member Problem1 = problem1
  static member Problem2 = problem2