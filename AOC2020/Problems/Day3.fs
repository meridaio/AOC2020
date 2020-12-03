module AOC2020.Day3

open System

let getCharAtIndex (seed: string) (index: int) =
  let x = index % seed.Length
  seed.[x]

let countTrees (rightStep: int) (downStep: int) (map: (int -> char) array) =
  let rec countTrees' (rightIndex: int) (downIndex: int) =
    if downIndex >= map.Length then 0
    else if map.[downIndex] rightIndex = '#' then 1 + (countTrees' (rightIndex + rightStep) (downIndex + downStep))
    else countTrees' (rightIndex + rightStep) (downIndex + downStep)
  countTrees' 0 0 |> int64

let problem1 (input: string array) =
  let l = input |> Array.map getCharAtIndex
  let treeCount = countTrees 3 1 l

  printfn "You hit %i trees" treeCount

let problem2 (input: string array) =
  let l = input |> Array.map getCharAtIndex
  let runs = [
    countTrees 1 1 l
    countTrees 3 1 l
    countTrees 5 1 l
    countTrees 7 1 l
    countTrees 1 2 l
  ]
  runs |> List.fold (*) 1L |> printfn "Trees multiplied is %i"

type Solver () =
  static member Problem1 = problem1
  static member Problem2 = problem2