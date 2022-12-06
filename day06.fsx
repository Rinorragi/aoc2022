#time

let startOfPacketIndex (datastream : string) (startMarkerLength : int) =
    datastream 
    |> Seq.windowed startMarkerLength
    |> Seq.findIndex (Seq.distinct >> Seq.length >> (=) startMarkerLength) 
    |> (+) startMarkerLength
    
let datastreamInput = System.IO.File.ReadAllText "./input/day06.txt"
startOfPacketIndex datastreamInput 4 |> printfn "Answer1: %d"
startOfPacketIndex datastreamInput 14 |> printfn "Answer2: %d"
