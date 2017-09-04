module Program

open GameLoop
open RenderEngine
open LevelFactory

[<EntryPoint>]
let main argv = 
    render testLevel |> gameLoop
    0 // return an integer exit code
