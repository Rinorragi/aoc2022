// Macro to get the execution time
#time
open System

type File = { name: string; size: int64 }

type Dir =
    { name: string
      files: File list
      subDirs: Dir list }

exception ValueProblem of string

let nl = "\n"

let simplePath (complexPath: string) =
    complexPath.Split('/', StringSplitOptions.RemoveEmptyEntries)
    |> Array.fold
        (fun (arr: string[]) (s: string) ->
            match s with
            | ".." -> arr |> Array.take (arr.Length - 1)
            | _ -> Array.append arr [| s |])
        [||]

let rec fileSystemState (relativeRoot: Dir) (path: string array) (leaf: File option) =
    if (Array.isEmpty path) then
        match leaf with
        | None -> relativeRoot
        | Some newFile ->

            let updatedFiles = relativeRoot.files |> List.append [ newFile ]


            { name = relativeRoot.name
              subDirs = relativeRoot.subDirs
              files = updatedFiles }
    else
        let nextDirName = path |> Array.head
        let dirSeek = relativeRoot.subDirs |> List.tryFind (fun d -> d.name = nextDirName)

        let aDir =
            match dirSeek with
            | Some foundDir -> foundDir
            | None ->
                { name = nextDirName
                  subDirs = []
                  files = [] }

        let updatedDir = fileSystemState aDir (path |> Array.tail) leaf

        let updatedDirs =
            relativeRoot.subDirs
            |> List.filter (fun f -> f.name <> updatedDir.name)
            |> List.append [ updatedDir ]

        { name = relativeRoot.name
          subDirs = updatedDirs
          files = relativeRoot.files }


let fileSystem =
    System.IO.File.ReadAllText "./input/day07.txt"
    |> (fun s -> s.Replace("\r\n", nl).Split('$', StringSplitOptions.RemoveEmptyEntries))
    |> Array.fold
        (fun ((fs: Dir), (currentDir: string)) (s: string) ->
            let command, commandOutput =
                s.Split(nl) |> (fun arr -> (Array.head arr, Array.tail arr))

            match command.Substring(1, 2) with
            | "cd" ->
                let newPath =
                    match command.[4] with
                    | '/' -> currentDir + "/"
                    | _ -> currentDir + command.Substring(4) + "/"

                let fsState = fileSystemState fs (simplePath newPath) None
                fsState, newPath
            | "ls" ->
                let fsState =
                    commandOutput
                    |> Array.filter ((<>) "")
                    |> Array.fold
                        (fun subState s ->
                            match s.StartsWith("dir") with
                            | true -> subState // no need to handle directories as they are created when accessed with cd
                            | false ->
                                let file =
                                    { name = s.Split(" ")[1]
                                      size = s.Split(" ")[0] |> int64 }

                                fileSystemState subState (simplePath currentDir) (Some file))
                        fs

                fsState, currentDir
            | _ -> raise (ValueProblem(sprintf "Unknown command:%s" command)))
        ({ name = "/"; subDirs = []; files = [] }, "")
    |> fst

let directoryTotalFileSize (files: File list) = files |> List.sumBy (fun f -> f.size)

let rec recursiveFiles (dir: Dir) =
    List.append (dir.subDirs |> List.map recursiveFiles |> List.concat) dir.files

let rec directoryFileTotals (dir: Dir) =
    let totalDirs = dir.subDirs |> List.map directoryFileTotals |> List.concat
    let dirTotal = recursiveFiles dir |> List.sumBy (fun f -> f.size)
    List.append totalDirs [ dir, dirTotal ]

let dirTotals =
    directoryFileTotals fileSystem |> List.map (fun f -> ((fst f).name, snd f))

dirTotals
|> List.filter (fun f -> (snd f) <= 100000L)
|> List.sumBy (snd)
|> printfn "Answer1: %A"

let fileSystemMaximumSize = 70000000L
let unusedRequiredSize = 30000000L
let totalUsed = dirTotals |> List.find (fun d -> fst d = "/") |> snd
let minimumToDelete = totalUsed + unusedRequiredSize - fileSystemMaximumSize

dirTotals
|> List.filter (fun f -> (snd f) > minimumToDelete)
|> List.sortBy (snd)
|> List.head
|> printfn "Answer1: %A"
