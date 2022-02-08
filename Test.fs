namespace MazeProject

open System

module Test =
    let testingWalls (maze:array<array<Cell>>) (size:Map<string,int>)=
        let rec loop y x w =
            let checkWall y x w  = 
                let checkBoudary y x = //Checks if coordinate is in wall boundary                     
                     x < size.["width"] && x > -1 && y < size.["height"] && y > -1
                match w with 
                | 0 ->  //Checks north wall broken 
                    if maze.[y].[x].north = 0
                    then 
                        true 
                    else 
                        if checkBoudary (y-1) x 
                        then 
                            if maze.[y-1].[x].south = 1
                            then 
                                true
                            else 
                                false
                        else true
                | 1 -> //Checks if east wall broken
                    if maze.[y].[x].east = 0
                    then 
                        true 
                    else 
                        if checkBoudary y (x+1) 
                        then 
                            if maze.[y].[x+1].west = 1
                            then 
                                true
                            else 
                                false
                        else true 
                | 2 -> //Checks if south wall broken
                    if maze.[y].[x].south = 0
                    then 
                        true 
                    else 
                        if checkBoudary (y+1) x 
                        then 
                            if maze.[y+1].[x].north = 1
                            then 
                                true
                            else 
                                false
                        else true
                | 3 -> //Checks if west wall broke
                    if maze.[y].[x].west = 0
                    then 
                        true 
                    else 
                        if checkBoudary y (x-1) 
                        then 
                            if maze.[y].[x-1].east = 1
                            then 
                                true
                            else 
                                false
                        else true       
                | _ -> false //shouldn't get here for testing broken walls              

            
            if y >= size.["height"] //If reached end of maze
            then 
                "walls are broken both ways"
            else 
                if x >= size.["width"] //If end of row increment row
                then 
                    loop (y+1) 0 0
                else 
                    if w >= 4 //If end num of walls increment cell
                    then 
                        loop y (x+1) 0
                    else 
                        if checkWall y x w //hecks if theres a wall and if wall is broken
                        then 
                            loop y x (w+1)
                        else 
                            "walls are NOT broken both ways"    
        loop 0 0 0
       
