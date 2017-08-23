module RenderEngine

open System
open Aether
open GameTypes
open GameTypes.Level
open GameTypes.Map
open Vector
open Queries.Level
open Monads.Maybe

let private tileToChar tile =
    match tile with
    | Wall -> '#' 
    | Floor -> '.' 
    | Water -> '~'
    | _ -> ' '

let private doorToChar door =
    match door with
    | Open -> '-'
    | Closed -> '+'
    | Locked _ -> '*'

let private renderTile level location =
    let char = maybeOrElse {
        return! level 
            |> Optic.get (mapActorAt_ location) 
            |> Option.bind (fun actorId -> Some '@')
        return! level 
            |> getDoor location 
            |> Option.bind (fun door -> Some (doorToChar door))
        return! level 
            |> getTile location 
            |> tileToChar |> Some
    }
    match char with
    | Some c -> c
    | None -> ' '

let private xs map = seq{0 .. (topRight map).x}
let private ys map = seq{((topRight map).y - 1) .. (-1) .. 0}

let render level = 
    let buildRow map currentY = 
        xs map 
            |> Seq.map (fun nextX -> vector.create nextX currentY)
            |> Seq.map (renderTile level)
            |> Seq.toArray
            |> System.String
    
    Console.Clear()
    ys level.map 
        |> Seq.map (buildRow level.map) 
        |> Seq.iter (printfn "%s")
        
