// Macro to get running time
#time
open System

exception ValueProblem of string
let nl = "\n"

let charToIntValue (c : char) =
    match Char.IsLower(c) with 
    | true-> int c - int 'a' + 1
    | false -> int c - int 'A' + 27

let ruckSackInput = 
    System.IO.File.ReadAllText "./input/day03.txt"
    // Replace CRLF to only LF (copy+paste and input in different format)
    |> fun s -> s.Replace("\r\n", nl)
    // Rucksack
    |> fun s -> s.Split(nl, StringSplitOptions.RemoveEmptyEntries)

ruckSackInput
// Separate compartments to two char sets
|> Array.map (fun s -> 
    s.[..(s.Length/2 - 1)].ToCharArray() |> Set.ofArray,
    s.[(s.Length/2)..].ToCharArray() |> Set.ofArray) 
// Find common letter with intersect and return it
|> Array.map (fun (set1, set2) -> Set.intersect set1 set2 |> Set.toArray |> Array.head)
// Get the value of the letters
|> Array.map charToIntValue
|> Array.sum
|> printfn "Answer1: %d"


ruckSackInput
|> Array.map (fun s -> s.ToCharArray())
// Change arrays to sets for intersect
|> Array.map Set.ofArray
|> Seq.ofArray
// Take three row windows at a time
|> Seq.chunkBySize 3
// Find the intersections
|> Seq.map Set.intersectMany
// Intersections from sett to array
|> Seq.map Set.toArray
// Arrays to chars (the only intersection)
|> Seq.map Array.head 
// Sequence to array
|> Array.ofSeq
// Chars to ints
|> Array.map charToIntValue
|> Array.sum
|> printfn "Answer2: %d"