#time
exception ValueProblem of string

type Coordinate = {
    x : int
    y : int
    tail : Coordinate option
}

let startPos = {
    x = 0 
    y = 0
    tail = Some({
        x = 0
        y = 0
        tail = None
    })
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

let updatePath path coordinate =
    path |> List.append [coordinate]

let moveTailX headPos tailX tailY =
    let xDir = tailDirection 'R' 'L' headPos.x tailX
    let xMoved = move xDir tailX tailY
    {
        x = fst xMoved
        y = snd xMoved
        tail = None
    }

let moveTailY headPos tailX tailY =
    let yDir = tailDirection 'U' 'D' headPos.y tailY
    let yMoved = move yDir tailX tailY
    {
        x = fst yMoved
        y = snd yMoved
        tail = None
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

let tailPosition tailSolver headMovement = 
    headMovement
    |> Array.fold (fun path (instruction,amount) ->
        [0.. (amount - 1)] 
        |> List.fold (fun (innerPath) _ -> 
            let head = innerPath |> List.head
            let headPos = move instruction head.x head.y 
            let headNewCoordinate = {
                x = fst headPos
                y = snd headPos
                tail = head.tail
            }
            let tailDetails = tailSolver headNewCoordinate
            let newEntry = {
                x = headNewCoordinate.x
                y = headNewCoordinate.y
                tail = tailDetails
            }
            updatePath innerPath newEntry ) 
            path) [startPos]

let shortTailSolver head = 
    let newTail = moveTail head head.tail.Value
    Some({
        x = newTail.x
        y = newTail.y
        tail = None
    })

let rec popTail times coordinate  = 
    if times = 0 
    then 
        coordinate
    else
        match Option.isSome(coordinate.tail) with 
        | true -> coordinate.tail.Value |> popTail (times - 1)
        | false -> coordinate

let longTailSolver head =
    let matchingHead = popTail 8 head
    let newCoordinate = {
        x = head.x
        y = head.y
        tail = Some({
            x = matchingHead.x
            y = matchingHead.y
            tail = None
        })
    } 
    newCoordinate

headInput 
|> tailPosition shortTailSolver 
|> List.filter (fun f -> Option.isSome(f.tail))
|> List.map (popTail 1)
|> List.distinct
|> List.length
|> printfn "Answer1: %d" 


//headInput
//|> tailPosition longTailSolver 
//|> List.map List.last
//|> List.map (fun xy -> (xy.x, xy.y))
//|> List.distinct
//|> List.length
//|> printfn "Answer2: %A" 