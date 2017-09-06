module Validation

open Microsoft.FSharp.Core
open Aether
open GameTypes
open GameTypes.Actor
open Queries.Level
open Vector
open Monads
open Monads.Result
open GameTypes.Item

// Building blocks

let private actorExists actorId level =
    match level |> findActor actorId with
    | Some actor -> Valid actor
    | None -> Invalid "The actor does not exist"

let private doorExists location level =
    match level |> findDoor location with
    | Some door -> Valid door
    | None -> Invalid "There is no door there"

// actor ignored, give better shape for later
let private canDoorBeOpened actor door =
    match door with
    | Closed -> Valid door
    | Open -> Invalid "Thet door is already open" 
    | Locked _ -> Invalid "That door is locked" 
    
// actor ignored, give better shape for later
let private canDoorBeClosed actor door =
    match door with
    | Open -> Valid door
    | Closed -> Invalid "That door is already closed" 
    | Locked _ -> Invalid "That door is locked closed" 

// actor ignored, give better shape for later
let private canDoorBeUnlocked actor door =
    match door with
    | Locked keyName -> Valid keyName
    | _ -> Invalid "That door is not locked" 

let private hasKeyForDoor keyName actor =
    if actor |> hasKey keyName then
        Valid keyName
    else
        Invalid ("You need " + keyName + " to unlock that door")

let private isValidLocation location level =
    if level |> hasCoordinate location then
        Valid location
    else
        Invalid "That location is not on the map"

let private isEmptyTile location level =
    if level |> locationBlocksMove location then
        Invalid "You can't move there" 
    else
        Valid (getTile location level)

let private canReach target location =
    if location |> distanceFrom target <= 1.0 then
        Valid target
    else
        Invalid "You can't reach that" 

let private isValidMoveDistance target location =
    if location |> distanceFrom target <= 1.0 then
        Valid target
    else
        Invalid "You can't move that far" 

let private itemsAtLocation location level =
    let items = level |> itemsAt location
    if items |> Seq.isEmpty then
        Invalid "No items to take" 
    else
        Valid items

let private isValidDirection direction actorId level =
    result {
        let! actor = level |> actorExists actorId 
        let targetLocation = (actor |> Optic.get location_) + direction
        let! validTarget = level |> isValidLocation targetLocation
        return actor, validTarget
    }
    
let private testDoorWith test direction actorId level =
    result {
        let! actor, validTarget = level |> isValidDirection direction actorId 
        let! _ = actor.location |> canReach validTarget 
        let! door = level |> doorExists validTarget 
        let! _ = door |> test actor
        return level
    }

let private hasKeyForLockedDoor actor door =
    result {
        let! keyName = door |> canDoorBeUnlocked actor
        let! _ = actor |> hasKeyForDoor keyName
        return door
    }

// Validators

let isValidMove direction actorId level =
    result {
        let! actor, validTarget = level |> isValidDirection direction actorId 
        let! _ = actor.location |> isValidMoveDistance validTarget 
        let! _ = level |> isEmptyTile validTarget 
        return level
    }

let canOpenDoor = testDoorWith canDoorBeOpened
    
let canCloseDoor = testDoorWith canDoorBeClosed

let isLockedDoor = testDoorWith canDoorBeUnlocked

let canUnlockDoor = testDoorWith hasKeyForLockedDoor

let canTakeItems direction actorId level =
    result {
        let! actor, validTarget = level |> isValidDirection direction actorId 
        let! _ = actor.location |> canReach validTarget 
        let! _ = level |> itemsAtLocation validTarget 
        return level
    }
