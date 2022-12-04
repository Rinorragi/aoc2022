// Macro to get running time
#time
open System

let letStringToIntArray (s : string) =
    let arr = s.Split("-",StringSplitOptions.RemoveEmptyEntries)
    [|Int32.Parse(arr.[0])..Int32.Parse(arr.[1])|]

let elfPairInput = 
    System.IO.File.ReadAllLines "./input/day04.txt"
    |> Array.map (fun s -> s.Split(',',StringSplitOptions.RemoveEmptyEntries))
    |> Array.map (fun arr -> 
        (Array.head arr |> letStringToIntArray, 
        Array.last arr |> letStringToIntArray))

elfPairInput
|> Array.filter (fun (elf1, elf2) ->
    let intersect = Set.intersect (Set.ofArray elf1) (Set.ofArray elf2)
    intersect.Count = elf1.Length || intersect.Count = elf2.Length)
|> Array.length
|> printfn "Answer1: %d"

elfPairInput
|> Array.filter (fun (elf1, elf2) ->
    let intersect = Set.intersect (Set.ofArray elf1) (Set.ofArray elf2)
    intersect.Count > 0)
|> Array.length
|> printfn "Answer2: %d"
