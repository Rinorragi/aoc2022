// Macro to get running time
#time
open System

let nl = "\n"
exception ValueProblem of string

type RPS =
    | Rock
    | Paper
    | Scissors

type RoundEnd =
    | Lose
    | Draw
    | Win

let roundInput =
    System.IO.File.ReadAllText "./input/day02.txt"
    // Replace CRLF to only LF (copy+paste and input in different format)
    |> fun s -> s.Replace("\r\n", nl)
    |> fun s -> s.Split(nl, StringSplitOptions.RemoveEmptyEntries)

let shapeScore (shape) =
    match shape with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let roundScore (roundEnd) =
    match roundEnd with
    | Lose -> 0
    | Draw -> 3
    | Win -> 6

roundInput
|> Array.map (fun row ->
    let (elf, me) =
        match row[0] with
        | 'A' -> Rock
        | 'B' -> Paper
        | 'C' -> Scissors
        | _ -> raise (ValueProblem(sprintf "Invalid elf input: %c" row[0]))
        , match row[2] with
          | 'X' -> Rock
          | 'Y' -> Paper
          | 'Z' -> Scissors
          | _ -> raise (ValueProblem(sprintf "Invalid me input: %c" row[2]))

    let winScore =
        match me with
        | Rock ->
            match elf with
            | Rock -> roundScore Draw
            | Paper -> roundScore Lose
            | Scissors -> roundScore Win
        | Paper ->
            match elf with
            | Rock -> roundScore Win
            | Paper -> roundScore Draw
            | Scissors -> roundScore Lose
        | Scissors ->
            match elf with
            | Rock -> roundScore Lose
            | Paper -> roundScore Win
            | Scissors -> roundScore Draw

    (shapeScore me) + winScore)
|> Array.sum
|> printfn "Answer 1:  %d "

roundInput
|> Array.map (fun row ->
    let (elf, endTarget) =
        match row[0] with
        | 'A' -> Rock
        | 'B' -> Paper
        | 'C' -> Scissors
        | _ -> raise (ValueProblem(sprintf "Invalid elf input: %c" row[0]))
        , match row[2] with
          | 'X' -> Lose
          | 'Y' -> Draw
          | 'Z' -> Win
          | _ -> raise (ValueProblem(sprintf "Invalid endtarget input: %c" row[2]))

    let shapeScore =
        match elf with
        | Rock ->
            match endTarget with
            | Lose -> shapeScore Scissors
            | Win -> shapeScore Paper
            | Draw -> shapeScore elf
        | Paper ->
            match endTarget with
            | Lose -> shapeScore Rock
            | Win -> shapeScore Scissors
            | Draw -> shapeScore elf
        | Scissors ->
            match endTarget with
            | Lose -> shapeScore Paper
            | Win -> shapeScore Rock
            | Draw -> shapeScore elf

    shapeScore + roundScore endTarget)
|> Array.sum
|> printfn "Answer 2:  %d "
