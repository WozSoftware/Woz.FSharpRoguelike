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

let private asOption value = if value then Some () else None

let private maybeActor location = 
    Optic.get (mapActorAt_ location) 
    >> Option.bind (fun actorId -> Some '@')

let private maybeItems location =
    hasItems location 
    >> asOption 
    >> Option.bind (fun _ -> Some '?')

let private maybeDoor location =
    findDoor location 
    >> Option.bind (fun door -> Some (doorToChar door))

let private maybeTile location =
    getTile location >> tileToChar >> Some

let private renderTile level location =
    let char = maybeOrElse {
        return! level |> maybeActor location
        return! level |> maybeItems location 
        return! level |> maybeDoor location 
        return! level |> maybeTile location 
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
    
    let print strings = 
        strings |> Seq.iter (printfn "%s")
        printfn ""

    Console.Clear()

    ys level.map |> Seq.map (buildRow level.map) |> print
    level.messages |> List.rev |> print

    level
