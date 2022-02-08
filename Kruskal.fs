namespace MazeProject

open System

module Kruskal =
    type opening = {
            atcord : Map<String,int>
            tocord : Map<String,int>
            direction : String   
        }
        
    let mazeEmpty (size:Map<String,int>)= //INITALIZE CELL FOR EACH CO-ORD
        //let emptyNeigbors = ["North", 0; "East", 0; "South", 0; "West", 0] |> Map.ofList
        Array.init size.["height"] (fun indexi -> 
            Array.init size.["width"] (fun indexj -> 
                {north = 0; east = 0; south = 0; west = 0; path = " "; direction = 0}))

    let initSets (size:Map<String,int>) = //INITALIZE EACH CO-ORD IN A ARRAY 
        Array.init (size.["height"] * size.["width"]) (fun i -> [| ["y", i/size.["width"] ;"x", i%size.["width"]] |> Map.ofList|] )
        
    let make (size:Map<string,int>) = //SHUFFLES SETS FOR RANDOM MAZE CREATION
        let rec loop (M:array<array<Cell>>) (sets: array<array<Map<string,int>>>) =
            let borderCheck (coord:Map<String,int>) = //CHECKS IF OUTSIDE MAZE BOUNDS
                coord.["x"] < size.["width"] && coord.["x"] > -1 && 
                    coord.["y"] < size.["height"] && coord.["y"] > -1

            let getNonStuckCord (set:array<Map<String,int>>) =
                let rec loop (cord:Map<String,int>) index =                      
                    let directions = //CREATES CORDS FOR EACH DIRECTION
                        [ ( [ "y", cord.["y"]-1 ;"x",cord.["x"]   ] |> Map.ofList, "North" ); //N
                          ( [ "y", cord.["y"]   ;"x",cord.["x"]+1 ] |> Map.ofList, "East"  ); //E
                          ( [ "y", cord.["y"]+1 ;"x",cord.["x"]   ] |> Map.ofList, "South" ); //S
                          ( [ "y", cord.["y"]   ;"x",cord.["x"]-1 ] |> Map.ofList, "West"  )] //W

                    let possibleDirs = [ for i in directions do 
                                            if not(Array.contains (i |> fst) set) &&                                                
                                                borderCheck(i |> fst)
                                                    then yield i ] //CHECKS IF EACH DIRECTION IS VALID 

                    if (possibleDirs |> List.length) = 0 
                        then 
                            loop (set.[index+1]) (index+1)
                        else 
                            let rndIndex = Random().Next(0, (possibleDirs |> List.length) ) //RANDOME INDEX FOR POSSIBLE DIRECTIONS
                            let actualDir = possibleDirs.[rndIndex]
                            {atcord = set.[index];  tocord = (actualDir |> fst) ; direction = (actualDir |> snd ) }
                loop set.[0] 0
                       

            
            let openWalls (M:array<array<Cell>>) (toOpen:opening)= //OPENS WALLS DEPENDING ON DIRECTION
                match toOpen.direction with 
                | "North" -> 
                    M.[toOpen.atcord.["y"]].[toOpen.atcord.["x"]].north <- 1
                    M.[toOpen.tocord.["y"]].[toOpen.tocord.["x"]].south <- 1
                | "East" ->
                    M.[toOpen.atcord.["y"]].[toOpen.atcord.["x"]].east <- 1
                    M.[toOpen.tocord.["y"]].[toOpen.tocord.["x"]].west <- 1
                | "South" ->
                    M.[toOpen.atcord.["y"]].[toOpen.atcord.["x"]].south <- 1
                    M.[toOpen.tocord.["y"]].[toOpen.tocord.["x"]].north <- 1
                | "West" ->
                    M.[toOpen.atcord.["y"]].[toOpen.atcord.["x"]].west <- 1
                    M.[toOpen.tocord.["y"]].[toOpen.tocord.["x"]].east <- 1
                | _ -> ()
                M 

            let joinSets  xi yi = //JOINS THE TWO SETS OF CO-ORDS TOGETHER AND DELETES EACH ONE FROM THE OVERALL SETS AND APPENDS THE JOINED ON
                [| for i in 0..(sets |> Array.length)-1 do 
                    if not(i = xi || i = yi)
                        then yield sets.[i] |] 
                |> Array.append  [| ( Array.append sets.[xi] sets.[yi] ) |]

            let findSet (cord:Map<String,int>) = //FIND THE SET OF THE GOING TO CO-ORD FOR JOINING OF SETS
                let rec loop i =
                    if Array.contains cord sets.[i]
                        then i
                        else loop (i+1)
                loop 0
                
            if (sets |> Array.length) = 1
                then
                    M
                else 
                    let rndSet = Random().Next(0, (sets |> Array.length )) //RANDOME INDEX OF SELECTED OF SET
                    let toOpen = getNonStuckCord sets.[rndSet]                              
                    loop 
                        (openWalls M toOpen) //NEW MAZE
                            (joinSets rndSet (findSet toOpen.tocord )) //UPDATED SETS

        loop (mazeEmpty size) (initSets size)
    