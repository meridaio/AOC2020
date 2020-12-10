module AOC2020.Day8

open System
open AOC2020.Input
open FParsec

type Instr = Acc of int | Jmp of int | Nop of int

let acc = pstring "acc " >>. pint32 |>> Acc
let jmp = pstring "jmp " >>. pint32 |>> Jmp
let nop = pstring "nop " >>. pint32 |>> Nop
let p = sepBy (acc <|> jmp <|> nop) newline .>> eof

let execute (l: Instr list) =
  let rec execute' ac nxt seen =
    if Set.contains nxt seen then ac, false
    else if nxt < 0 then ac, false
    else if nxt = List.length l then ac, true
    else if nxt > List.length l then ac, false
    else
      match l.[nxt] with
      | Acc x -> execute' (ac + x) (nxt + 1) (Set.add nxt seen)
      | Jmp x -> execute' ac (nxt + x) (Set.add nxt seen)
      | Nop _ -> execute' ac (nxt + 1) (Set.add nxt seen)
  execute' 0 0 Set.empty

let replaceAt i l e =
  let x, y = List.splitAt i l
  x @ e :: List.tail y

let swapJmpNop = function
  | Nop x -> Jmp x
  | Jmp x -> Nop x
  | x -> x

let iterate l = Seq.mapi (fun i instr -> swapJmpNop instr |> replaceAt i l ) l

let getSolution (l: Instr list seq) = Seq.map execute l |> Seq.find snd |> fst

type Solver () =
  static member Problem1 : string array -> unit = fun input ->
    srun p input <| (execute >> fst >> printfn "acc value: %i")
  static member Problem2 : string array -> unit = fun input ->
    srun p input <| (iterate >> getSolution >> printfn "acc value: %i")