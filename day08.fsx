#time

let forestInput =
    System.IO.File.ReadAllLines "./input/day08.txt"
    |> Array.map Array.ofSeq
    |> Array.map (fun treeRow -> treeRow |> Array.map (fun c -> int c - int '0'))

let verticalFolder (forest: int list list) (cols: int list) rowIndex =
    cols
    |> List.fold
        (fun (treeHeight, visibleTrees) index ->
            match forest.[rowIndex].[index] with
            | aTree when treeHeight < aTree -> (aTree, visibleTrees |> List.append [ (rowIndex, index) ])
            | _ -> (treeHeight, visibleTrees))
        (-1, [])

let horizontalVisible (forest: int list list) =
    [ 0 .. (forest |> Seq.length) - 1 ]
    |> List.fold
        (fun state row ->
            let rowLength = (forest.[row] |> List.length) - 1
            let (_, lefToRight) = verticalFolder forest ([ 0..rowLength ]) row
            let (_, rightToLeft) = verticalFolder forest ([ rowLength .. -1 .. 0 ]) row
            state |> List.append lefToRight |> List.append rightToLeft)
        ([])

let rowVisibility =
    forestInput |> List.ofSeq |> List.map List.ofSeq |> horizontalVisible

let colVisibility =
    forestInput
    |> Seq.transpose
    |> List.ofSeq
    |> List.map List.ofSeq
    |> horizontalVisible
    |> List.map (fun f -> (snd f, fst f))

let visibleTrees = rowVisibility |> List.append colVisibility |> List.distinct

visibleTrees |> List.length |> printfn "Answer1: %d"

let forestWidthLastIndex = (forestInput.[0] |> Array.length) - 1
let forestHeightLastIndex = (forestInput |> Array.length) - 1

let rec travel value directionX directionY x y distance =
    let newX = directionX x
    let newY = directionY y

    if
        newX < 0
        || newY < 0
        || newX > forestWidthLastIndex
        || newY > forestHeightLastIndex
    then
        distance
    elif value <= forestInput.[newY].[newX] then
        distance + 1
    else
        travel value directionX directionY newX newY (distance + 1)

forestInput
|> Array.mapi (fun y row ->
    row
    |> Array.mapi (fun x value ->
        if x = 0 || y = 0 then
            ([ y, x ], 0)
        else
            let topDistance = travel value id (fun f -> f - 1) x y 0
            let botDistance = travel value id ((+) 1) x y 0
            let leftDistance = travel value (fun f -> f - 1) id x y 0
            let rightDistance = travel value ((+) 1) id x y 0
            let scenic = topDistance * botDistance * leftDistance * rightDistance
            ([ y, x ], scenic)))
|> Array.concat
|> Array.maxBy snd
|> snd
|> printfn "Answer2: %d"
