module PlayerInput

open System
open Monads
open GameTypes
open Commands
open Vector.Directions
open Validation

let private selectActorCommand direction actorId level =
    match level |> isLockedDoor direction actorId with
    | Valid l -> l |> buildUnlockDoorCommand direction actorId
    | Invalid _ ->
        match level |> buildOpenDoorCommand direction actorId with
        | Valid l -> Valid l
        | Invalid _ -> level |> buildMoveActorCommand direction actorId 

let rec handleKeyPress activeBuilder actorId =
    let workingBuilder = 
        match activeBuilder with
        | Some builder -> builder
        | None -> selectActorCommand

    match Console.ReadKey().Key with
    | ConsoleKey.O -> handleKeyPress (Some buildOpenDoorCommand) actorId
    | ConsoleKey.C -> handleKeyPress (Some buildCloseDoorCommand) actorId
    | ConsoleKey.T -> handleKeyPress (Some buildTakeItemsCommand) actorId
    | ConsoleKey.W -> workingBuilder north actorId
    | ConsoleKey.A -> workingBuilder west actorId
    | ConsoleKey.S -> workingBuilder south actorId
    | ConsoleKey.D -> workingBuilder east actorId
    | ConsoleKey.Spacebar -> idleCommand
    | _ -> invalidCommand

let getCommandForActor = handleKeyPress None 

let getPlayerCommand level = 
    level.playerId |> getCommandForActor

