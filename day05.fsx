// Macro to get running time
#time
open System

let nl = "\n"

let createCrates (crateString: string) =
    let charNumberArrays =
        crateString.Split(nl, StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun s -> s.ToCharArray())
        |> Array.map (Array.chunkBySize (4))
        |> Array.map (fun arr -> arr |> Array.map (fun chars -> chars.[1]))
    // Flip rows to columns
    let flippedArray =
        Array2D.init (charNumberArrays.[0].Length) (charNumberArrays.Length) (fun r c -> charNumberArrays.[c].[r])
    // Convert array2d to lists
    let lists =
        [ let height = flippedArray.GetLength 0

          for row in 0 .. height - 1 do
              yield flippedArray.[row, *] |> List.ofArray ]
    // Remove the empty chars and numbers to return only the piles
    lists |> List.map (fun l -> l |> List.filter (fun c -> Char.IsLetter(c)))

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
                if (i + 1 = crateFrom) then
                    // Remove crates from crateArray
                    l |> List.skip amount
                elif (i + 1 = crateTo) then
                    // Take crates dictated by appendFunction function parameter
                    List.append (crateState.[crateFrom - 1] |> List.take amount |> appendFunction) l
                else
                    // Don't change this crate array
                    l))
        crates
    |> List.map List.head
    |> String.Concat

// CrateMover 9000 that takes units one by one and thus reverses the taking order
crateMover (List.rev) |> printfn "Answer1: %s"
// CrateMover 9001 that takes units as they are
crateMover (fun l -> l) |> printfn "Answer2: %s"
