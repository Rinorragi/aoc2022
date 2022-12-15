#time

type Sensor = {
    x: int 
    y: int
    beaconX: int
    beaconY: int
    closestBeaconManhattan: int
}

let manhattanDistance (x1, y1) (x2, y2) = abs(x1 - x2) + abs (y1 - y2)

let sensorInput =     
    System.IO.File.ReadAllLines "./input/day15.txt"
    |> Array.map (fun s -> 
        let stringToIntTuple (xyString: string) = xyString.Split(", y=") |> (fun arr -> (arr.[0] |> int, arr.[1] |> int))
        let sArr = s.Split(": closest beacon is at x=")
        let sensorXY = sArr.[0].Substring("Sensor at x=".Length) |> stringToIntTuple
        let beaconXY = sArr.[1] |> stringToIntTuple
        {
            x = fst sensorXY
            y = snd sensorXY
            beaconX = fst beaconXY
            beaconY = snd beaconXY
            closestBeaconManhattan = manhattanDistance sensorXY beaconXY 
        })
    |> List.ofArray

let maxX = sensorInput |> List.map (fun f -> [f.x; f.beaconX]) |> List.concat |> List.max |> (fun f-> f*2)
let rowToTest = 2000000
let beaconsAtTestRow = 
    sensorInput 
    |> List.filter (fun f -> f.beaconY = rowToTest && f.beaconX < maxX && f.beaconX > -maxX) 
    |> List.map (fun f -> (f.beaconX,f.beaconY))
    |> List.distinct
    |> List.length

let impossibleBeacons = 
    [-maxX..maxX]
    |> List.map (fun x -> 
        sensorInput 
        |> List.map (fun sensor -> 
            let manhattan = manhattanDistance (sensor.x, sensor.y) (x,rowToTest)
            sensor.closestBeaconManhattan >= manhattan)
        |> List.exists id)
    |> List.filter id
    |> List.length
printfn "%d" (impossibleBeacons - beaconsAtTestRow)