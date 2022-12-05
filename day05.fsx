// Macro to get running time
#time
open System

let nl = "\n"

let createCrates (crateString: string) =
    crateString.Split(nl, StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun s -> s.ToCharArray())
    // Remove nonsense and take only letters by chunking and taking the 1-index things
    |> Array.map (Array.chunkBySize (4))
    |> Array.map (fun arr -> arr |> Array.map (fun chars -> chars.[1]))
    // Pivot columns to rows
    |> Seq.transpose
    |> List.ofSeq
    // Remove the empty chars and numbers to return only the piles
    |> List.map (fun l -> l |> Seq.filter (fun c -> Char.IsLetter(c)) |> List.ofSeq)

let createInstructions (instructionString: string) =
    instructionString.Split(nl, StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun s ->
        let insArr =
            s.Split([| "move "; " from "; " to " |], StringSplitOptions.RemoveEmptyEntries)

        (int insArr[0], int insArr[1], int insArr[2]))

let (crates, instructions) =
    System.IO.File.ReadAllText "./input/day05.txt"
    // Replace CRLF to only LF (copy+paste and input in different format)
    |> fun s -> s.Replace("\r\n", nl)
    |> fun s -> s.Split(nl + nl, StringSplitOptions.RemoveEmptyEntries)
    |> fun arr -> (Array.head arr |> createCrates, Array.last arr |> createInstructions)

// Fold instructions with initial state of current crates
let crateMover appendFunction =
    instructions
    |> Array.fold
        (fun crateState (amount, crateFrom, crateTo) ->
            crateState
            // Move from array to another
            |> List.mapi (fun i l ->
                match i+1 with
                // Remove crates from crateArray
                | num when num = crateFrom -> l |> List.skip amount
                // Take crates dictated by appendFunction function parameter
                | num when num = crateTo -> List.append (crateState.[crateFrom - 1] |> List.take amount |> appendFunction) l
                // Don't change this crate array
                | _ -> l))
        crates
    |> List.map List.head
    |> String.Concat

// CrateMover 9000 that takes units one by one and thus reverses the taking order
crateMover (List.rev) |> printfn "Answer1: %s"
// CrateMover 9001 that takes units as they are
crateMover id |> printfn "Answer2: %s"
