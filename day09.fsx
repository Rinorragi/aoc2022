#time
exception ValueProblem of string

type Coordinate = {
    headX : int
    headY : int
    tailX : int
    tailY : int
}

let startPos = {
    headX = 0 
    headY = 0
    tailX = 0
    tailY = 0
}

let move instruction x y amount=
    match instruction with 
    | 'U' -> (x, y + amount) 
    | 'D' -> (x, y - amount)
    | 'R' -> (x + amount, y)
    | 'L' -> (x - amount, y)
    | _ -> raise (ValueProblem(sprintf "Unknown instruction: %c" instruction)) 

let tailDirection increaseChar decreaseChar targetValue otherValue =
    match targetValue > otherValue with
    | true -> increaseChar
    | false -> decreaseChar

let updatePath path coordinate =
    path |> List.append [coordinate]

let moveTailX headPos tailX tailY =
    let xDir = tailDirection 'R' 'L' headPos.headX tailX
    let xMoved = move xDir tailX tailY 1
    {
            headX = headPos.headX
            headY = headPos.headY
            tailX = fst xMoved
            tailY = snd xMoved
    }

let moveTailY headPos tailX tailY =
    let yDir = tailDirection 'U' 'D' headPos.headY tailY
    let yMoved = move yDir tailX tailY 1
    {
            headX = headPos.headX
            headY = headPos.headY
            tailX = fst yMoved
            tailY = snd yMoved
    }

let rec moveTail (headPos : Coordinate) (tailPos : Coordinate) (path : Coordinate list) =
    let xDistance = headPos.headX - tailPos.tailX |> abs
    let yDistance = headPos.headY - tailPos.tailY |> abs
    if (xDistance > 1 && yDistance > 0) || (xDistance > 0 && yDistance > 1)
    then 
        let xMoved = moveTailX headPos tailPos.tailX tailPos.tailY
        let yMoved = moveTailY headPos xMoved.tailX xMoved.tailY
        let newPath = updatePath path yMoved
        moveTail headPos yMoved newPath
    elif  xDistance > 1
    then
        let xMoved = moveTailX headPos tailPos.tailX tailPos.tailY
        let newPath = updatePath path xMoved
        moveTail headPos xMoved newPath
    elif yDistance > 1 
    then
        let yMoved = moveTailY headPos tailPos.tailX tailPos.tailY
        let newPath = updatePath path yMoved
        moveTail headPos yMoved newPath
    else 
        path


let ropeInput =
    System.IO.File.ReadAllLines "./input/day09.txt"
    |> Array.map (fun s -> (s.[0], s.Substring(2) |> int))
let shortRope = 
    ropeInput
    |> Array.fold (fun ((headCoordinate, tailCoordinate), path) (instruction,amount) ->
        
        let headPos = move instruction headCoordinate.headX headCoordinate.headY amount 
        let headNewCoordinate = {
            headX = fst headPos
            headY = snd headPos
            tailX = tailCoordinate.tailX
            tailY = tailCoordinate.tailY
        }
        let updatedPath = moveTail headNewCoordinate tailCoordinate (updatePath path headNewCoordinate)
        let tailNewCoordinate = updatedPath |> List.head
        ((headNewCoordinate, tailNewCoordinate), updatedPath)
    ) ((startPos,startPos),[startPos]) 
    |> snd

shortRope
|> List.map (fun xy -> (xy.tailX, xy.tailY))
|> List.distinct
|> List.length
|> printfn "Answer1: %A" 
