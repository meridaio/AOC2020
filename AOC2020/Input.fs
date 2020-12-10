module AOC2020.Input

open System
open FParsec

let srun p (input: string array) (f: 'a -> unit) =
  match run p (String.Join("\n", input)) with
  | Success (result, _, _) -> f result
  | Failure (error, _, _) -> failwith error