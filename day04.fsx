// Macro to get running time
#time
open System

let letStringToIntArray (s : string) =
    let arr = s.Split("-",StringSplitOptions.RemoveEmptyEntries)
    Set [Int32.Parse(arr.[0])..Int32.Parse(arr.[1])]

// Function to output set of int arrays
let elfPairInput = 
    System.IO.File.ReadAllLines "./input/day04.txt"
    |> Array.map (fun s -> s.Split(',', StringSplitOptions.RemoveEmptyEntries))
    |> Array.map (fun arr -> 
        (Array.head arr |> letStringToIntArray, 
        Array.last arr |> letStringToIntArray))

// Function to calculate the results, takes solverfunction as a paremeter
let solver filterFunction printString =
    elfPairInput 
    |> Array.filter filterFunction
    |> Array.length
    |> printfn printString

// Solve Answer 1
solver (fun (elf1, elf2) -> 
    Set.isSubset elf1 elf2 || Set.isSuperset elf1 elf2) 
    "Answer1: %d"
// Solve Answer 2
solver (fun (elf1, elf2) -> 
    Set.intersect elf1 elf2 
    |> Seq.isEmpty 
    |> not) 
    "Answer2: %d"