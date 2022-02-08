namespace MazeProject

open System

module Main =
    [<EntryPoint>]    
    let Main argv =
        let size = ["height", 10; "width", 10] |> Map.ofList
        let maze = Kruskal.make size
        Interactions.displayMaze maze
        printfn "%s" (Test.testingWalls maze size) 
        Console.ReadLine() |> ignore
        0

    