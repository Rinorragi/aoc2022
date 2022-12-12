#time
//Dijkstra's algorithm: Nigel Galloway, August 5th., 2018
//https://rosettacode.org/wiki/Dijkstra%27s_algorithm#F#
[<CustomEquality;CustomComparison>]
type Dijkstra<'N,'G when 'G:comparison>=
    {toN:'N;cost:Option<'G>;fromN:'N}
    override g.Equals n =match n with| :? Dijkstra<'N,'G> as n->n.cost=g.cost|_->false
    override g.GetHashCode() = hash g.cost
    interface System.IComparable with
        member n.CompareTo g =
            match g with
            | :? Dijkstra<'N,'G> as n when n.cost=None -> (-1)
            | :? Dijkstra<'N,'G>      when n.cost=None -> 1
            | :? Dijkstra<'N,'G> as g                  -> compare n.cost g.cost
            | _-> invalidArg "n" "expecting type Dijkstra<'N,'G>"

let inline Dijkstra N G y =
  let rec fN l f=
    if List.isEmpty l 
    then f
    else 
        let n=List.min l
        if n.cost=None 
        then f 
        else
        fN(l|>List.choose(fun n'->
            if n'.toN=n.toN 
            then None 
            else 
                match n.cost,n'.cost,Map.tryFind (n.toN,n'.toN) G with
                    |Some g,None,Some wg->Some {toN=n'.toN;cost=Some(g+wg);fromN=n.toN}
                    |Some g,Some g',Some wg when g+wg<g'->Some {toN=n'.toN;cost=Some(g+wg);fromN=n.toN}
                    |_ ->Some n'))((n.fromN,n.toN)::f)
  let r = fN (N|>List.map(fun n->{toN=n;cost=(Map.tryFind(y,n)G);fromN=y})) []
  (fun n->
    let rec fN z l=
        match List.tryFind(fun (_,g)->g=z) r with
        |Some(n',g') when y=n'->Some(n'::g'::l)
        |Some(n',g') ->fN n' (g'::l)
        |_ ->None
    fN n [])
// End of copy paste


type Coordinate = {
    id: string
    x: int
    y: int
    height: int
    start: bool
    best: bool
}

let rec charHeight c = 
    match c with 
    | 'S' -> charHeight 'a'
    | 'E' -> charHeight 'z'
    | _ -> int c - int 'a'

let mazeInput =     
    System.IO.File.ReadAllLines "./input/day12.txt"
    |> Array.map Array.ofSeq
    |> Array.mapi (fun iy row ->
        row |> Array.mapi (fun ix c -> {
                id = sprintf "%d_%d" ix iy
                x = ix
                y = iy
                height = charHeight c
                start = (c = 'S')
                best = (c = 'E')
            }))

let fetchCoordinate xy =
    if fst xy < 0 
        || fst xy > mazeInput.[0].Length - 1 
        || snd xy < 0 
        || snd xy > mazeInput.Length - 1
    then 
        None
    else 
        Some(mazeInput.[snd xy].[fst xy])

let mazeGraph = 
    mazeInput
    |> Array.mapi (fun y row ->
        row |> Array.mapi (fun x xy -> 
            let up = fetchCoordinate (x, y + 1) 
            let down = fetchCoordinate (x, y - 1)
            let right = fetchCoordinate (x + 1, y)
            let left = fetchCoordinate (x - 1, y)
            [(xy,up);(xy,down);(xy,right);(xy,left)] 
            |> List.filter (snd >> Option.isSome) 
            |> List.map (fun (xy,other) -> (xy, other.Value))
            |> List.filter (fun (xy, other) ->  xy.height + 1 >= other.height))
        |> List.ofArray
        |> List.concat)
    |> List.ofArray
    |> List.concat
    |> List.map (fun (c1: Coordinate, c2: Coordinate) -> ((c1,c2),1))
    |> Map.ofList

let mazePoints = mazeInput |> Array.concat |> List.ofArray
let start = mazePoints |> List.find (fun f -> f.start = true)
let goal = mazePoints |> List.find (fun f -> f.best = true)
 
let paths=Dijkstra mazePoints mazeGraph start
let pathFound = (paths goal)
printfn "Answer1: %d" (pathFound.Value.Length - 1)

mazeGraph 
|> Map.toList
|> List.filter (fun ((c1,c2),value) -> c1.height = 0 && c2.height = 1) // shortest a will have b next to it for sure
|> List.map (fun ((c1,_),_) -> c1)
|> List.distinct
|> List.map (fun startCandidate -> 
    let p2Paths = Dijkstra mazePoints mazeGraph startCandidate
    (p2Paths goal).Value)
|> List.minBy List.length
|> (fun shortestPath -> printfn "Answer2: %d" (shortestPath.Length - 1))