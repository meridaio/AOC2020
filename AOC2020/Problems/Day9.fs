module AOC2020.Day9

open AOC2020.Input

let readPrelude = List.splitAt 25

let check p (e: int64) =
  match pairs p |> Seq.tryFind (fun (x, y) -> x + y = e) with
  | Some _ -> true
  | None -> false

let findInvalid (l: int64 list) =
  let rec findInvalid' p l =
    match l with
    | x::xs ->
      if check p x then
        findInvalid' (List.tail p @ [x]) xs
      else
        x
    | [] -> failwith "could not find an invalid element"
  let p, xs = readPrelude l
  findInvalid' p xs

let rec trySumTo e d (l: int64 list) =
  let comp = List.take d l
  let s = List.sum comp
  if s = e then Some <| List.min comp + List.max comp
  else if s > e then None
  else trySumTo e (d + 1) l

let rec rotate l f =
  match f l with
  | Some x -> x
  | None -> rotate (List.tail l) f

let getEncryptionWeakness (l: int64 list) =
  let invalid = findInvalid l
  trySumTo invalid 0 |> rotate l

type Solver () =
  static member Problem1 : string array -> unit = List.ofArray >> List.map int64 >> findInvalid >> printfn "Invalid: %i"
  static member Problem2 : string array -> unit = List.ofArray >> List.map int64 >> getEncryptionWeakness >> printfn "Weakness: %i"