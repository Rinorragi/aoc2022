#time
let cpuInstructionInput =
    System.IO.File.ReadAllLines "./input/day10.txt"

let cycles = 
    [1..240]
    |> List.fold (fun (result: int list, instructionProcessing: string, index: int) signalRound ->
        let oldX = result |> List.last
        match instructionProcessing with
        | "noop" -> 
            let newInstruction = cpuInstructionInput.[index]
            (List.append result [oldX], newInstruction, (index + 1))
        | _ -> 
            let toAdd = instructionProcessing.Split(" ") |> Array.last |> int
            (List.append result [(oldX + toAdd)], "noop", index)
    ) ([1], "noop", 0)
    |> (fun (results, _, _) -> (results))

let signalAtCycle (cycles : int list) (cycle : int) = cycles[cycle - 1] * cycle

[20;60;100;140;180;220]
|> List.map (fun cycle -> cycles[cycle - 1] * cycle)
|> List.sum
|> printfn "Answer1: %d"

let chunkCyclesForCRT skip = cycles |> List.skip skip |> List.take 40

printfn "Answer2: "
[0;40;80;120;160;200]
|> List.map chunkCyclesForCRT
|> List.map (fun chunk -> 
    chunk 
    |> List.mapi (fun pos middlePixelInSprite -> 
        match pos = middlePixelInSprite || pos - 1 = middlePixelInSprite || pos + 1 = middlePixelInSprite with 
        | true -> '#'
        | false -> '.'))
|> List.map (fun chars ->
    let sb = System.Text.StringBuilder(chars.Length)
    chars |> List.iter (sb.Append >> ignore)
    sb.ToString())
|> printfn "%A"