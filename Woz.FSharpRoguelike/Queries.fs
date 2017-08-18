module Queries

open Microsoft.FSharp.Core.Option
open Aether
open GameTypes
open GameTypes.Level
open Vector

module Level =
    let hasCoordinate location level = 
        location >= Map.bottomLeft && 
        location <= level.map.topRight

    let getTile location level = 
        level.map.tiles.[location.y].[location.y]

    let private isBlockingTile location level =
        match level |> getTile location with
        | Void | Wall -> true
        | Floor | Water -> false

    let private isBlockingDoor location level = 
        match level |> Optic.get (doorAt_ location) with
        | Some door -> 
            match door with 
            | Closed | Locked _ -> true
            | Open -> false
        | None -> false

    let getActor actorId level =
        level |> Optic.get (actorWithId_ actorId)

    let expectActor actorId level =
        level |> Optic.get (expectActorWithId_ actorId)

    let hasActor location level =
        level |> Optic.get (mapActorAt_ location) |> isSome

    let blockView = [isBlockingTile; isBlockingDoor]
    let blockMove = [isBlockingTile; isBlockingDoor; hasActor]

    let checkLocationFor predicates location level =
        if hasCoordinate location level then 
            let testPredicate = (fun predicate -> level |> predicate location)
            predicates |> Seq.exists testPredicate
        else true

    let getItems location level =
        // Build a lens
        match level.mapItems.TryFind location with
        | Some items -> items
        | None -> []
