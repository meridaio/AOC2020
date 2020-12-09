module AOC2020.Day7

open System
open FParsec

let bag = manyCharsTill anyChar (skipString " bag") .>> optional (pchar 's')
let requirements = sepBy1 (pint32 .>>. (bag |>> fun s -> s.Trim())) (skipString ", ")
let emptyRequirements : Parser<(int32 * string) list, unit> = skipString "no other bags" |>> fun () -> []
let bagWithRequirements = (bag .>> skipString " contain ") .>>. (requirements <|> emptyRequirements)
let pinput = sepEndBy1 bagWithRequirements (pchar '.' .>> optional newline)

let rec references elem (map: Map<string, (int * string) list>) key =
  let e = Map.tryFind key map |> Option.map (Seq.map snd)
  let contains = e |> Option.map (Seq.contains elem)
  match contains, e with
  | Some true, _ -> true
  | Some false, Some x -> x |> Seq.map (references elem map) |> Seq.exists id
  | _, _ -> false

let rec countContained (map: Map<string, (int * string) list>) elem =
  match Map.find elem map with
  | [] -> 0
  | xs -> List.sumBy (fun (x, y) -> x + x * (countContained map y)) xs

type Solver () =
  static member Problem1 : string array -> unit = fun input ->
    match run pinput (String.Join("\n", input)) with
    | Success (result, _, _) ->
      printfn "len %A" <| List.last result
      let map = Map.ofList result
      let keys = Seq.map fst result
      let num = Seq.map (references "shiny gold" map) keys |> Seq.filter id |> Seq.length
      printfn "Num: %i" num
    | Failure (error, _, _) -> failwith error
  static member Problem2 : string array -> unit = fun input ->
    match run pinput (String.Join("\n", input)) with
    | Success (result, _, _) -> countContained (Map.ofList result) "shiny gold" |> printfn "Num contained: %i"
    | Failure (error, _, _) -> failwith error