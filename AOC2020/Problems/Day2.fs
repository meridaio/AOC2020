module AOC2020.Day2

open System
open System.Text.RegularExpressions

type PasswordPolicy = {
  LowerLimit: int
  UpperLimit: int
  Character: char
}

type Password = {
  Value: string
  Policy: PasswordPolicy
}

let private reg = Regex("""(\d+)\-(\d+)\s([a-z])\:\s([a-z]+)""", RegexOptions.Compiled)

let private parsePassword (input: string) =
  let matches = reg.Match(input)
  {
    Value = matches.Groups.[4].Value
    Policy = {
      LowerLimit = matches.Groups.[1].Value |> Int32.Parse
      UpperLimit = matches.Groups.[2].Value |> Int32.Parse
      Character = matches.Groups.[3].Value.[0]
    }
  }

let private passwordPolicy1 (password: Password) =
  let count = Seq.sumBy (fun c -> if c = password.Policy.Character then 1 else 0) password.Value
  password.Policy.LowerLimit <= count && count <= password.Policy.UpperLimit

let private passwordPolicy2 (password: Password) =
  let check charAt = password.Value.[charAt - 1] = password.Policy.Character
  check password.Policy.LowerLimit <> check password.Policy.UpperLimit

let private checkPasswordByPolicy (policy: Password -> bool) (input: string array) =
  let validPasswords =
    Seq.map parsePassword input
    |> Seq.filter policy
    |> Seq.length
  printfn "%i passwords are valid" validPasswords

let private problem1 (input: string array) =
  checkPasswordByPolicy passwordPolicy1 input

let private problem2 (input: string array) =
  checkPasswordByPolicy passwordPolicy2 input

type Solver () =
  static member Problem1 = problem1
  static member Problem2 = problem2