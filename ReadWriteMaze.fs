namespace MazeProject

open System.IO
open System
module ReadWriteMaze =

    let targetDirect = __SOURCE_DIRECTORY__ + "\\saved"

    let getNextSave (saves:array<string>) = 
        let rec loop (i:int) = 
            if i > (saves.Length-1) 
                then i.ToString()
                else if Array.contains ("maze" + i.ToString()) saves 
                    then loop (i+1)
                    else i.ToString()
        loop 0

    let checkSave () =
        let saves = Directory.GetFiles(targetDirect) 
        [| for i in 0..saves.Length-1 do Path.GetFileNameWithoutExtension(saves.[i]) |]
        
    let write (maze:array<array<Cell>>) =
        let name = (checkSave () |>  getNextSave )
        let path = targetDirect + "\\maze" + name + ".txt"
        let mutable txt = ""
        let size = ["height",maze |> Array.length; "width", maze |> Array.head |> Array.length] |> Map.ofList
        
        for y in 0..size.["height"]-1 do 
            for x in 0..size.["width"]-1 do 
                let c = maze.[y].[x]
                txt <- txt + c.north.ToString() 
                            + c.east.ToString() 
                             + c.south.ToString() 
                              + c.west.ToString() 
            txt <- txt + "\n"
            //writes cell's data to txt file 
        File.WriteAllText(path, txt)
        "maze" + name

        
    
    let read (path:string) =
        let actucalPath = __SOURCE_DIRECTORY__ + "\\saved\\" + path + ".txt"
        let lines = seq{
            use sr = new StreamReader (actucalPath)
            while not sr.EndOfStream do 
                yield sr.ReadLine()
        } // Reads each line from txt file
        
        let charToInt c = int c - int '0' //char to int converter 
        let data = [for i in lines do yield Seq.chunkBySize 4 i] |> Seq.ofList //Splits data every 4 spaces
        let size = ["height", data |> Seq.length; "width", data |> Seq.head |> Seq.length] |> Map.ofList //width and height of read maze
        
        let maze (d:seq<seq<char array>>) =
            let m = MazeGenerator.mazeEmpty size

            for i in 0..size.["height"]-1 do 
                for j in 0..size.["width"]-1 do 
                    let north = d |> Seq.item i |> Seq.item j |> Array.item 0 |> charToInt
                    let east = d |> Seq.item i |> Seq.item j |> Array.item 1 |> charToInt
                    let south = d |> Seq.item i |> Seq.item j |> Array.item 2 |> charToInt
                    let west = d |> Seq.item i |> Seq.item j |> Array.item 3 |> charToInt
                    let cell = {north = north; east = east; south = south; west = west; direction = 0; path = ""}
                    m.[i].[j] <- cell                      
            m
            //places data values into maze object       
        (maze data, size)
     
    let deleteSave (path:string) = 
        let p = __SOURCE_DIRECTORY__ + "\\saved\\" + path + ".txt"
        File.Delete(p)
        //Deletes desired maze from saved location
    
    let createSaveDirectory () =
        if not(Directory.Exists(targetDirect)) then 
            Directory.CreateDirectory(targetDirect) |>ignore
        //Ceates a saved foler directory if one doesnt exist
        
    