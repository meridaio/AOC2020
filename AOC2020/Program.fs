﻿open System
open System.IO
open System.Diagnostics
open AOC2020
open Argu

type CliArgs =
    | [<AltCommandLine("-p")>] [<Mandatory>] Problem_Number of day:int * num:int
    | [<AltCommandLine("-f")>] [<Mandatory>] Input_File of path:string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Problem_Number _ -> "problem day (1-25) and number (1-2) to execute"
            | Input_File _ -> "path to input file"

let run = function
    | 1 -> Day1.day1
    | 2 -> Day2.day2
    | _ -> fun _ _ -> ()

[<EntryPoint>]
let main argv =
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<CliArgs>(programName = "aoc2020", errorHandler = errorHandler)
    let results = parser.Parse argv

    let problem, num = results.GetResult Problem_Number
    let fileName = results.GetResult Input_File
    let inputLines = fileName |> File.ReadAllLines
    let problemNumber =
        match num with
        | 1 -> Problem1
        | 2 -> Problem2
        | s -> failwithf "%i is not a valid problem number" s

    printfn "Running Day %i problem %i against %s" problem num fileName
    let watch = Stopwatch()
    watch.Start()
    run problem problemNumber inputLines
    watch.Stop()
    printfn "Problem completed in %M seconds" (decimal watch.ElapsedMilliseconds / 1000M)

    0 // return an integer exit code
