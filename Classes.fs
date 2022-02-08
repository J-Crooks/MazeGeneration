namespace MazeProject

type Cell = {
    mutable north : int;
    mutable east : int;
    mutable south : int;
    mutable west : int;
    mutable path : string; 
    mutable direction : int; 
}    