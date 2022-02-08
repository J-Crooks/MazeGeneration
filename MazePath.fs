namespace MazeProject

open System

module MazePath =
    let solve (M:array<array<Cell>>) =
        let start = ["y",0; "x",0] |> Map.ofList 
        let directCal = [(-1,0);(0,1);(1,0);(0,-1)]
        let size = ["height", (M |> Array.length); "width", (M |> Array.head |> Array.length)] |> Map.ofList
        let finish = ["y", size.["height"]-1; "x", size.["width"]-1] |> Map.ofList

        let rec loop (M:array<array<Cell>>) (visited:seq<Map<string,int>>) (path:seq<Map<string,int>>) (current:Map<string,int>) = 
            
            let checkDir (current:Map<string,int>) =
                let createDir (current:Map<string,int>) =
                    ["y", current.["y"] + (directCal.[M.[current.["y"]].[current.["x"]].direction] |> fst);
                     "x", current.["x"] + (directCal.[M.[current.["y"]].[current.["x"]].direction] |> snd)] |> Map.ofList 

                let checkWall =
                    match M.[current.["y"]].[current.["x"]].direction with
                    | 0 -> M.[current.["y"]].[current.["x"]].north = 1
                    | 1 -> M.[current.["y"]].[current.["x"]].east = 1
                    | 2 -> M.[current.["y"]].[current.["x"]].south = 1
                    | 3 -> M.[current.["y"]].[current.["x"]].west = 1
                    | _ -> false

                let predicate (check:Map<string,int>) =
                    check.["x"] < size.["width"] && check.["x"] > -1 && 
                    check.["y"] < size.["height"] && check.["y"] > -1 && 
                    not(Seq.contains check visited) &&
                    checkWall 

                createDir current |> predicate

            if current = finish
                then 
                    M.[current.["y"]].[current.["x"]].path <- "E"
                    M.[0].[0].path <- "S"
                    M
                else 
                    if M.[current.["y"]].[current.["x"]].direction = 4 
                        then 
                            M.[current.["y"]].[current.["x"]].path <- " "  
                            loop M visited (path |> Seq.tail) (path |> Seq.tail |> Seq.head)
                        else 
                            if checkDir current 
                                then                                
                                    let pathMake = M.[current.["y"]].[current.["x"]].direction                                   
                                    let newCoord = ["y", current.["y"] + (directCal.[pathMake] |> fst);
                                                    "x", current.["x"] + (directCal.[pathMake] |> snd)] |> Map.ofList
                                    M.[current.["y"]].[current.["x"]].path <- "#"
                                    loop 
                                        M // Maze
                                            (Seq.append ([newCoord] |> Seq.ofList) visited) //visited
                                                (Seq.append ([newCoord] |> Seq.ofList) visited) //path
                                                    newCoord //current
                                else 
                                    M.[current.["y"]].[current.["x"]].direction <- M.[current.["y"]].[current.["x"]].direction + 1
                                    loop M visited path current
                           
        loop M (seq{start}) (seq{start}) start
