#time

open System

let startOfPacketIndex (datastream : string) (startMarkerLength : int) =
    datastream 
    |> List.ofSeq
    |> List.windowed startMarkerLength
    |> List.findIndex (fun dataWindow -> List.distinct dataWindow |> List.length = startMarkerLength)
    |> (fun f -> f + startMarkerLength)
    
let datastreamInput = System.IO.File.ReadAllText "./input/day06.txt"
startOfPacketIndex datastreamInput 4 |> printfn "Answer1: %d"
startOfPacketIndex datastreamInput 14 |> printfn "Answer2: %d"
