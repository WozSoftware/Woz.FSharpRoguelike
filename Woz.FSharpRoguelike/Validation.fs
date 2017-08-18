module Validation

open Microsoft.FSharp.Core
open GameTypes
open Queries.Level
open Vector
open Monads
open Monads.Result

module Level =
    let actorExists actorId level =
        match level |> getActor actorId with
        | Some actor -> Valid actor
        | None -> Invalid "The actor does not exist"

    let isValidLocation location level =
        if level |> hasCoordinate location then
            Valid location
        else
            Invalid "That location is not on the map"

    let isEmptyTile location level =
        if level |> checkLocationFor blockMove location then
            Invalid "You can't move to that location" 
        else
            Valid (getTile location level)

    let isValidMoveDistance target location =
        if location |> distanceFrom target <= 1.0 then
            Valid target
        else
            Invalid "You can't move that far" 

    let isValidMove target actorId level =
        result {
            let! validTarget = level |> isValidLocation target 
            let! tile = level |> isEmptyTile validTarget 
            let! actor = level |> actorExists actorId 
            let! validMove = actor.location |> isValidMoveDistance validTarget 
            return level
        }
        