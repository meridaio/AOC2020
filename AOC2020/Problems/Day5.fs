module AOC2020.Day5

open System

type Seat = {
  Row: int
  Column: int
}

let parseRowPartition = List.map (function | 'F' -> 0 | 'B' -> 1 | r -> failwithf "invalid row %c" r)
let parseColPartition = List.map (function | 'L' -> 0 | 'R' -> 1 | c -> failwithf "invalid col %c" c)

let intlToBinary (l: int list) =
  let s = l |> Seq.map string |> String.concat ""
  Convert.ToInt32(s, 2)

let parseSeat (pass: string) =
  let rs, cs = pass.ToCharArray() |> List.ofArray |> List.splitAt 7
  {
    Row = parseRowPartition rs |> intlToBinary
    Column = parseColPartition cs |> intlToBinary
  }

let getSeatId (seat: Seat) =
  seat.Row * 8 + seat.Column

let problem1 (input: string array) =
  let max = input |> Seq.ofArray |> Seq.map (parseSeat >> getSeatId) |> Seq.max
  printfn "Max ID is %i" max

let problem2 (input: string array) =
  let seats =
    input
    |> Seq.ofArray
    |> Seq.map (parseSeat >> getSeatId)
    |> Seq.sort
    |> List.ofSeq

  let missing, _ = Seq.zip [List.head seats..List.last seats] seats |> Seq.find (fun (x, y) -> x <> y)
  printfn "Missing seat ID: %i" missing

type Solver () =
  static member Problem1 = problem1
  static member Problem2 = problem2