namespace MazeProject

open System

module Interactions =
    let getSize () =
        let mutable height = ""
        let mutable width = ""
        let mutable hValid = false        
        let mutable wValid = false
             
        while not(hValid) || not(wValid) do
            printfn "Enter size of the maze (Positives numbers)"
            printf "Height of the maze: "; height <- Console.ReadLine() 
            printf "Width of the maze: " ; width <- Console.ReadLine()
            hValid <- height |> Seq.forall Char.IsDigit ;
            wValid <- width |> Seq.forall Char.IsDigit               
        ["height", height |> int; "width", width|> int] |> Map.ofList

    let displayMaze (m:array<array<Cell>>)  =
        let size = ["height", (m |> Array.length) ;"width", (m |> Array.head |> Array.length)  ] |> Map.ofList
        printfn ""
        let getIcon icon ori=
            if icon = 1 
                then printf " "
                else if ori = 0 then printf "-" else printf "|"

        for i in 1..size.["width"] do
                printf "+ - "
        printf "+"
        for y in 0..size.["height"]-1 do 
            printfn ""
            getIcon m.[y].[0].west 1
            for x in 0..size.["width"]-1 do 
                printf " "
                printf "%s" m.[y].[x].path
                printf " "
                getIcon m.[y].[x].east 1
            printfn ""
            for x in 0..size.["width"]-1 do
                printf "+ "
                getIcon m.[y].[x].south 0
                printf " "
            printf "+"   

    let getMaze () =
        let saves = ReadWriteMaze.checkSave ()
        printfn ""
        printfn "choose a maze to load:" 
        printfn "%A" saves
        let save = Console.ReadLine()
        ReadWriteMaze.read(__SOURCE_DIRECTORY__ + "\\saved\\" + save + ".txt")


         