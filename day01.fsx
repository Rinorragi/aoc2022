// Macro to get running time
#time 
// Dependencies
open System

type Elf = {
    Calories : int64 list
    CalorySum : int64
}

// Newline
let nl = "\n"

// string to Elf type
let populateElf (stringInput : string) =
    // string contains numbers that are separated by line change
    // Convert them to elf type
    let caloriesList = 
        stringInput |> fun s -> s.Split(nl,StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int64 
        |> Array.toList
    // Create Elf object
    {
        Calories = caloriesList
        CalorySum = List.sum caloriesList
    }

let inputToElves (filePath : string) = 
    System.IO.File.ReadAllText filePath
    |> fun s -> s.Replace("\r\n", nl)
    |> fun s -> s.Split(nl+nl, StringSplitOptions.RemoveEmptyEntries)
    |> Array.map populateElf
    |> Array.sortByDescending (fun elf -> elf.CalorySum)

let top3ElvesCalorySum (elves : Elf array) = 
    elves 
    |> Array.take 3
    |> Array.sumBy (fun e -> e.CalorySum)

// Read example input
let exampleElfs = inputToElves "./input/day01_example.txt" 
// Verify example
printfn "Example should be 24000 and is %d" exampleElfs[0].CalorySum
// Verify example for answer2
let answer2_example = top3ElvesCalorySum exampleElfs
printfn "Example should be 45000 and is %d" answer2_example

// Read real input
let elves = inputToElves "./input/day01.txt" 
printfn "Answer1 is: %d" elves[0].CalorySum
top3ElvesCalorySum elves |> printfn "Answer2 is: %d"


