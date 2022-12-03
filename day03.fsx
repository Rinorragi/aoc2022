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
    System.IO.File.ReadAllLines "./input/day03.txt"

ruckSackInput
// Separate compartments to two char sets
|> Array.map (fun s -> 
    s.ToCharArray() 
    |> Array.splitInto 2 
    |> Array.map Set.ofArray) 
// Find common letter with intersect and return it
|> Array.map (fun arr-> 
    Set.intersect  arr.[0] arr.[1] 
    |> Set.maxElement)
// Get the value of the letters
|> Array.sumBy charToIntValue
|> printfn "Answer1: %d"


ruckSackInput
|> Array.map (fun s -> s.ToCharArray())
// Change arrays to sets for intersect
|> Array.map Set.ofArray
// Take three row windows at a time
|> Seq.chunkBySize 3
// Find the intersections
|> Seq.map Set.intersectMany
// Arrays to chars (the only intersection)
|> Seq.map Set.maxElement 
// Chars to ints
|> Seq.sumBy charToIntValue
|> printfn "Answer2: %d"