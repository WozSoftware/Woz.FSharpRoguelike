module Program

open LevelFactory
open GameLoop

[<EntryPoint>]
let main argv = 
    gameLoop testLevel
    0 // return an integer exit code
