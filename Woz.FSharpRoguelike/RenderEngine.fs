module RenderEngine

open System
open Aether
open GameTypes
open GameTypes.Level
open Vector
open Queries.Level

let private tileToChar tile =
    match tile with
    | Wall -> '#' 
    | Floor -> '.' 
    | Water -> '~'
    | _ -> ' '

let private renderTile level location =
    match level |> Optic.get (mapActorAt_ location) with
    | Some _ -> '@'
    | None -> level |> getTile location |> tileToChar

let private xs map = seq{0 .. map.topRight.x}
let private ys map = seq{(map.topRight.y - 1) .. -1 .. 0}

let render level = 
    let buildRow map currentY = 
        xs map 
            |> Seq.map (fun nextX -> {x = nextX; y = currentY})
            |> Seq.map (renderTile level)
            |> Seq.toArray
            |> System.String
    
    Console.Clear()
    ys level.map |> Seq.map (buildRow level.map) |> Seq.iter (printfn "%s")
        
