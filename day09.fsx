#time
exception ValueProblem of string

type Coordinate = {
    x : int
    y : int
}

let startPos = {
    x = 0 
    y = 0
}

let move instruction x y =
    match instruction with 
    | 'U' -> (x, y + 1) 
    | 'D' -> (x, y - 1)
    | 'R' -> (x + 1, y)
    | 'L' -> (x - 1, y)
    | _ -> raise (ValueProblem(sprintf "Unknown instruction: %c" instruction)) 

let tailDirection increaseChar decreaseChar targetValue otherValue =
    match targetValue > otherValue with
    | true -> increaseChar
    | false -> decreaseChar

let moveTailX headPos tailX tailY =
    let xDir = tailDirection 'R' 'L' headPos.x tailX
    let xMoved = move xDir tailX tailY
    {
        x = fst xMoved
        y = snd xMoved
    }

let moveTailY headPos tailX tailY =
    let yDir = tailDirection 'U' 'D' headPos.y tailY
    let yMoved = move yDir tailX tailY
    {
        x = fst yMoved
        y = snd yMoved
    }

let rec moveTail (headPos : Coordinate) (oldTail : Coordinate) =
    let xDistance = headPos.x - oldTail.x |> abs
    let yDistance = headPos.y - oldTail.y |> abs
    if (xDistance > 1 && yDistance > 0) || (xDistance > 0 && yDistance > 1)
    then 
        let xMoved = moveTailX headPos oldTail.x oldTail.y
        let yMoved = moveTailY headPos xMoved.x xMoved.y
        yMoved
    elif  xDistance > 1
    then
        let xMoved = moveTailX headPos oldTail.x oldTail.y
        xMoved
    elif yDistance > 1 
    then
        let yMoved = moveTailY headPos oldTail.x oldTail.y
        yMoved
    else 
        oldTail

let headInput =
    System.IO.File.ReadAllLines "./input/day09.txt"
    |> Array.map (fun s -> (s.[0], s.Substring(2) |> int))

let tailPosition rope headMovement = 
    headMovement
    |> Array.fold (fun ropePath (instruction,amount) ->
        [0.. (amount - 1)] 
        |> List.fold (fun (innerPath) _ -> 
            let oldRope = innerPath |> List.last
            let head = oldRope |> List.head
            let headPos = move instruction head.x head.y 
            let headNewCoordinate = {
                x = fst headPos
                y = snd headPos
            } 
            let newRope =
                oldRope
                |> List.skip 1
                |> List.fold (fun (newRope) oldTail -> 
                    let index = newRope |> List.length
                    let nextTail = moveTail newRope[index - 1] oldTail
                    List.append newRope [nextTail]
                ) ([headNewCoordinate])
            //printfn "%A" newRope
            List.append innerPath [newRope]
        ) ropePath
    ) rope


let ropeTailPositionAmount rope =
    headInput 
    |> tailPosition rope
    |> List.map (List.last)
    |> List.distinct
    |> List.length

let startList amount = 
    List.init amount (fun _ -> startPos)

[startList 2] |> ropeTailPositionAmount |> printfn "Answer1: %d" 
[startList 10] |> ropeTailPositionAmount |> printfn "Answer2 %d" 

