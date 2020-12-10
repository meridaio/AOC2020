module AOC2020.Input

open System
open FParsec

let srun p (input: string array) (f: 'a -> unit) =
  match run p (String.Join("\n", input)) with
  | Success (result, _, _) -> f result
  | Failure (error, _, _) -> failwith error

let rec pairs = function
  | h :: t ->
    seq {
      for x in t do yield h, x
      yield! pairs t
    }
  | _ -> Seq.empty

let rec triples = function
  | h :: t ->
    seq {
      for y, z in pairs t do yield h, y, z
      yield! triples t
    }
  | _ -> Seq.empty
