namespace MazeProject

open System 

module MazeGenerator =
    let mazeEmpty (size:Map<String,int>)=
            Array.init size.["height"] (fun indexi -> 
                Array.init size.["width"] (fun indexj -> 
                    {north = 0; east = 0; south = 0; west = 0; direction = 0; path = ""}))
                    
    let make (size:Map<string,int>) =
        let initMaze (M:array<array<Cell>>) = 
            let gen = System.Random()
            let randCoord = ["y", gen.Next(0, size.["height"]);"x", gen.Next(0, size.["width"])] |> Map.ofList  
            
            let rec createMaze (M:array<array<Cell>>) (visited:seq<Map<string,int>>) (current:Map<string,int>) (path:seq<Map<string,int>>)= 

                let rndDirect (visited:seq<Map<string,int>>) (current:Map<string,int>) =
                    let createNxt (c:Map<string,int>) (R:Random)=
                            match R.Next(0,4) with 
                            | 0 -> (c.Add("y",c.["y"] + 1), "South")
                            | 1 -> (c.Add("x",c.["x"] + 1), "East")
                            | 2 -> (c.Add("y",c.["y"] - 1), "North")
                            | 3 -> (c.Add("x",c.["x"] - 1), "West")
                            | _ -> (c, "Inavlid")
                            //random direction with coords 

                    let rec loopRndDirect (n:Random) ((nxt, direct):Map<string,int> * string)  =          
                        if nxt.["x"] < size.["width"] && nxt.["x"] > -1 && nxt.["y"] < size.["height"] && nxt.["y"] > -1 && not(Seq.contains nxt visited) 
                            then (nxt,direct) 
                            //Returns next coord with the direction
                            else loopRndDirect (System.Random()) (createNxt current n)
                            //checks if coords valid and not already visited 

                    loopRndDirect (System.Random())  (createNxt current gen)

                let isStuck (M:array<array<Cell>>) (visited:seq<Map<string,int>>) (current:Map<string,int>) =
                    let checkCoord (nxt:Map<string,int>) =
                        nxt.["x"] < size.["width"] && nxt.["x"] > -1 && nxt.["y"] < size.["height"] && nxt.["y"] > -1
                    let x = current.["x"]
                    let y = current.["y"]
                    let around =  [ [ "y", y+1;"x",x]|>Map.ofList;
                                    [ "y", y;"x",x+1]|>Map.ofList;
                                    [ "y", y-1;"x",x]|>Map.ofList;
                                    [ "y", y;"x",x-1]|>Map.ofList; ] |> Seq.ofList //Coords around current 
                    let validPoints = [for a in around do if checkCoord a then yield a] //Validates coords around current 
                    Seq.forall (id) (Seq.map(fun i -> Seq.exists ((=) i) visited) validPoints) //Check if all around coords have been visited
 
                let openDirections (M:array<array<Cell>>) (current:Map<string,int>) (dir:string) =
                    match dir with 
                    | "North" -> 
                        M.[current.["y"]].[current.["x"]].north <- 1
                        M.[current.["y"]-1].[current.["x"]].south <- 1
                    | "East" ->
                        M.[current.["y"]].[current.["x"]].east <-1 
                        M.[current.["y"]].[current.["x"]+1].west <-1 
                    | "South" ->
                        M.[current.["y"]].[current.["x"]].south <- 1
                        M.[current.["y"]+1].[current.["x"]].north <-1 
                    | "West" ->
                        M.[current.["y"]].[current.["x"]].west <- 1 
                        M.[current.["y"]].[current.["x"]-1].east <-1 
                    | _ -> ()
                    M // Open cell wall and other side cell on approiate side

                if path |> Seq.isEmpty 
                    then
                        M
                    else 
                        if isStuck M visited current
                            then  
                                if path |> Seq.length = 1 
                                    then createMaze M visited ([] |> Map.ofList) []
                                    else createMaze M visited (path |> Seq.tail |> Seq.head) (path |> Seq.tail)                                             
                            else 
                                let rnd = rndDirect visited current
                                createMaze (rnd |> snd |> openDirections M current)  (*Maze*)
                                    (Seq.append ([rnd |> fst] |> Seq.ofList) visited) (*Visited*)
                                        (rnd |> fst)                                     (*Current*)
                                            (Seq.append ([rnd |> fst] |>Seq.ofList) path) (*Path*)
                                            //Main driver of funciion,  if stuck backtrack else find new direction
            createMaze M [randCoord] randCoord [randCoord]
        initMaze (mazeEmpty size)
        

    

