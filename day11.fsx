#time
open System
exception ValueProblem of string
type MonkeyOperation = Sum|Multiple|Exp
type Monkey = {
    id: int
    items: int64 list
    op: MonkeyOperation
    opValue: int64
    test: int64
    testTrueMonkeyId: int
    testFalseMonkeyId: int
    inspectionAmount: int64
}
let nl = "\n"

let monkeyInput =     
    System.IO.File.ReadAllText "./input/day11.txt"
    // Replace CRLF to only LF (copy+paste and input in different format)
    |> fun s -> s.Replace("\r\n", nl)
    |> fun s -> s.Split(nl+nl, StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun s -> 
        let rows = s.Split(nl, StringSplitOptions.RemoveEmptyEntries) 
        let operator = rows.[2].Substring(("  Operation: new = old ").Length)
        let sOpValue = rows.[2].Substring(("  Operation: new = old ? ".Length))
        let (op, (opValue: int64)) = 
            match operator with
            | "* old" -> (MonkeyOperation.Exp, 0) // 0 because irrelevant
            | _ when ("+ " + sOpValue) = operator -> (MonkeyOperation.Sum, sOpValue |> int64)
            | _ when ("* " + sOpValue) = operator -> (MonkeyOperation.Multiple, sOpValue |> int64)
            | _ -> raise(ValueProblem(sprintf "Unrecognized operator: %s" operator))
        {
            id = rows.[0].Split(" ").[1].Substring(0,1) |> int
            items = rows.[1].Substring(("  Starting items: ".Length)).Split(", ", StringSplitOptions.RemoveEmptyEntries) |> Array.map int64 |> List.ofArray
            op = op
            opValue = opValue
            test = rows.[3].Substring("  Test: divisible by ".Length) |> int64
            testTrueMonkeyId = rows.[4].Substring("    If true: throw to monkey ".Length) |> int
            testFalseMonkeyId = rows.[5].Substring("    If false: throw to monkey ".Length) |> int
            inspectionAmount = 0
        })
    |> List.ofArray


let worryInspection monkeyInTurn = 
    monkeyInTurn.items 
    |> List.map (fun item ->
        let worryIncrease = 
            match monkeyInTurn.op with
            | MonkeyOperation.Exp -> item * item
            | MonkeyOperation.Sum -> item + monkeyInTurn.opValue
            | MonkeyOperation.Multiple -> item * monkeyInTurn.opValue
        let boredom = worryIncrease / 3L
        let monkeyId = 
            match boredom % monkeyInTurn.test = 0 with 
            | true -> monkeyInTurn.testTrueMonkeyId
            | false -> monkeyInTurn.testFalseMonkeyId
        (monkeyId, boredom))

let throwItems monkeyInTurn worries monkeyToThrowAt = 
    let thrownToThisMonkeyItems = 
        worries 
        |> List.filter (fun w -> fst w = monkeyToThrowAt.id) 
        |> List.map snd
    let thisMonkeyItemsAndInspection = 
        match monkeyToThrowAt.id = monkeyInTurn.id with
        | true -> (thrownToThisMonkeyItems, monkeyToThrowAt.inspectionAmount + (worries.Length |> int64)) // not sure if it possible to throw to himself, but does not hurt to support it
        | false -> (List.append monkeyToThrowAt.items thrownToThisMonkeyItems, monkeyToThrowAt.inspectionAmount)
    {
        id = monkeyToThrowAt.id
        items = fst thisMonkeyItemsAndInspection
        op = monkeyToThrowAt.op
        opValue = monkeyToThrowAt.opValue
        test = monkeyToThrowAt.test
        testTrueMonkeyId = monkeyToThrowAt.testTrueMonkeyId
        testFalseMonkeyId = monkeyToThrowAt.testFalseMonkeyId
        inspectionAmount = snd thisMonkeyItemsAndInspection
    }

let monkeyGameOutput =
    [1..20] // rounds
    |> List.fold(fun (monkeyState: Monkey list) round ->
        let updatedState = 
            monkeyState
            |> List.fold (fun (innerMonkeyState: Monkey list)  monkeyId ->
                let monkeyInTurn = innerMonkeyState[monkeyId.id]
                let monkeyWorries = worryInspection monkeyInTurn 
                innerMonkeyState 
                |> List.map (throwItems monkeyInTurn monkeyWorries)
            ) monkeyState
        updatedState |> List.map (fun f -> f.items) |> printfn "Round %d: %A" round
        updatedState
    ) monkeyInput

monkeyGameOutput
|> List.map (fun monkey -> monkey.inspectionAmount)
|> List.sortDescending
|> List.take 2
|> List.fold (fun state i -> state * i) 1L
|> printfn "Answer1: %d" 