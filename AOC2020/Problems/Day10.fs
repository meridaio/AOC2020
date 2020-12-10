module AOC2020.Day10

open System

let count1and3 (l: int list) =
  let rec count1and3' (ones, threes) l =
    match l with
    | x::y::xs ->
      printfn "(%i, %i) %A" ones threes l
      if y - x = 1 then count1and3' (ones + 1, threes) (y::xs)
      else if y - x = 3 then count1and3' (ones, threes + 1) (y::xs)
      else count1and3' (ones, threes) (y::xs)
    | s ->
      printfn "%A" s
      (ones, threes + 1) // add one to threes since adapter is 3 higher than highest rated
  
  count1and3' (0, 0) (0::l)

let mult (ones, threes) =
  ones * threes

let rec convertToDiff (l: int list) =
  match l with
  | x::y::xs -> (y - x)::(convertToDiff (y::xs))
  | _ -> l

let rec tribonacci = function
  | 0. -> 1.
  | 1. -> 1.
  | 2. -> 2.
  | x -> (tribonacci (x - 1.)) + (tribonacci (x - 2.)) + (tribonacci (x - 3.))

let rec countArrangements (l: int list) =
  match List.length l with
  | 0 | 1 -> 1.
  | _ ->
    let count = List.takeWhile (fun x -> x = 1) l |> List.length
    countArrangements (List.skip (count + 1) l) * (float count |> tribonacci)

let addStartAndEnd (l: int list) = (0::l)

type Solver () =
  static member Problem1 : string array -> unit = List.ofArray >> List.map int >> List.sort >> count1and3 >> mult >> printfn "Product: %i"
  static member Problem2 : string array -> unit =
    List.ofArray
    >> List.map int
    >> List.sort
    >> addStartAndEnd
    >> convertToDiff
    >> countArrangements
    >> printfn "Num: %f"