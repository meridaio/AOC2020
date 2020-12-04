module AOC2020.Day4

open System
open System.Text.RegularExpressions

type Passport = {
  byr: string option
  iyr: string option
  eyr: string option
  hgt: string option
  hcl: string option
  ecl: string option
  pid: string option
  cid: string option
}

let emptyPassport = {
  byr = None
  iyr = None
  eyr = None
  hgt = None
  hcl = None
  ecl = None
  pid = None
  cid = None
}

let getLines (input: string array) =
  let rec group list = seq {
    let i = list |> Array.tryFindIndex String.IsNullOrEmpty
    match i with
    | Some idx -> 
      let h, t = list |> Array.splitAt idx
      yield h
      yield! group (Array.skip 1 t)
    | None ->
      yield list
  }
  group input

let generatePassport (input: string array) =
  let splitField (f: string) =
    let s = f.Split(':')
    s.[0], s.[1]

  let mapField ((field, value): string * string) (passport: Passport) =
    match field with
    | "byr" -> { passport with byr = Some value }
    | "iyr" -> { passport with iyr = Some value }
    | "eyr" -> { passport with eyr = Some value }
    | "hgt" -> { passport with hgt = Some value }
    | "hcl" -> { passport with hcl = Some value }
    | "ecl" -> { passport with ecl = Some value }
    | "pid" -> { passport with pid = Some value }
    | "cid" -> { passport with cid = Some value }
    | _ -> passport

  let rec generatePassport' (passport: Passport) (fields: (string * string) list) =
    match fields with
    | h :: t -> generatePassport' (mapField h passport) t
    | _ -> passport

  seq { for s in input do yield! s.Split(' ') }
  |> Seq.map splitField
  |> List.ofSeq
  |> generatePassport' emptyPassport


let isPassportReal (passport: Passport) =
  Option.isSome passport.byr &&
  Option.isSome passport.ecl &&
  Option.isSome passport.eyr &&
  Option.isSome passport.hcl &&
  Option.isSome passport.hgt &&
  Option.isSome passport.iyr &&
  Option.isSome passport.pid

let tryContains (s: string) (list: int list) =
  try
    List.contains (int s) list
  with
  | _ -> false

let validateByr (passport: Passport) =
  let validByr = [1920..2002]
  tryContains passport.byr.Value validByr

let validateIyr (passport: Passport) =
  let validIyr = [2010..2020]
  tryContains passport.iyr.Value validIyr

let validateEyr (passport: Passport) =
  let validEyr = [2020..2030]
  tryContains passport.eyr.Value validEyr

let validateHgt (passport: Passport) =
  let validHgtCm = [150..193]
  let validHgtIn = [59..76]
  let units = passport.hgt.Value.Substring(passport.hgt.Value.Length - 2)
  let value = passport.hgt.Value.Substring(0, passport.hgt.Value.Length - 2)
  match units with
  | "cm" -> tryContains value validHgtCm
  | "in" -> tryContains value validHgtIn
  | _ -> false

let hclRegex = Regex("^#([0-9]|[a-f]){6}$", RegexOptions.Compiled)
let validateHcl (passport: Passport) =
  hclRegex.IsMatch(passport.hcl.Value)

let validateEcl (passport: Passport) =
  let validEcl = ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]
  List.contains passport.ecl.Value validEcl

let validatePid (passport: Passport) =
  let isValidLong, _ = Int64.TryParse(passport.pid.Value)
  passport.pid.Value.Length = 9 && isValidLong

let validatePassport (passport: Passport) =
  [
    validateByr
    validateIyr
    validateEyr
    validateHgt
    validateHcl
    validateEcl
    validatePid
  ]
  |> List.map (fun fn -> fn passport)
  |> List.fold (&&) true

let isPassportValid (passport: Passport) =
  isPassportReal passport && validatePassport passport

let getNumValidBy (validator: Passport -> bool) (input: string array) =
    getLines input
    |> Seq.map (generatePassport >> validator)
    |> Seq.filter id
    |> Seq.length

let problem1 (input: string array) =
  let numRealPassports = getNumValidBy isPassportReal input

  printfn "Number of real passports: %i" numRealPassports

let problem2 (input: string array) =
  let numValidPassports = getNumValidBy isPassportValid input

  printfn "Number of valid passports: %i" numValidPassports

type Solver () =
  static member Problem1 = problem1
  static member Problem2 = problem2