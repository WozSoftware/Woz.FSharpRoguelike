module Validation

open Microsoft.FSharp.Core
open Aether
open GameTypes
open GameTypes.Actor
open Queries.Level
open Vector
open Monads
open Monads.Result

module Level =

    // Building blocks

    let private actorExists actorId level =
        match level |> getActor actorId with
        | Some actor -> Valid actor
        | None -> Invalid "The actor does not exist"

    let private doorExists location level =
        match level |> getDoor location with
        | Some door -> Valid door
        | None -> Invalid "There is no door there"
    
    let private canDoorBeOpened door =
        match door with
        | Closed -> Valid door
        | Open -> Invalid "Thet door is already open" 
        | Locked _ -> Invalid "That door is locked" 
    
    let private canDoorBeClosed door =
        match door with
        | Open -> Valid door
        | Closed -> Invalid "Thet door is already closed" 
        | Locked _ -> Invalid "That door is locked closed" 

    let private isValidLocation location level =
        if level |> hasCoordinate location then
            Valid location
        else
            Invalid "That location is not on the map"

    let private isEmptyTile location level =
        if level |> locationBlocksMove location then
            Invalid "You can't move to that location" 
        else
            Valid (getTile location level)

    let private canReachDoor target location =
        if location |> distanceFrom target <= 1.0 then
            Valid target
        else
            Invalid "You can't reach that door" 

    let private isValidMoveDistance target location =
        if location |> distanceFrom target <= 1.0 then
            Valid target
        else
            Invalid "You can't move that far" 

    let private isValidDirection direction actorId level =
        result {
            let! actor = level |> actorExists actorId 
            let targetLocation = (actor |> Optic.get location_) + direction
            let! validTarget = level |> isValidLocation targetLocation
            return (actor, validTarget)
        }
    
    let private testDoor test direction actorId level =
        result {
            let! (actor, validTarget) = level |> isValidDirection direction actorId 
            let! door = level |> doorExists validTarget 
            let! canReach = actor.location |> canReachDoor validTarget 
            let! _ = door |> test
            return level
        }

    // Validators

    let isValidMove direction actorId level =
        result {
            let! (actor, validTarget) = level |> isValidDirection direction actorId 
            let! tile = level |> isEmptyTile validTarget 
            let! validMove = actor.location |> isValidMoveDistance validTarget 
            return level
        }

    let canOpenDoor = testDoor canDoorBeOpened 
    
    let canCloseDoor = testDoor canDoorBeClosed 
