module PlayerInput

open System
open Monads.Result
open Commands
open Vector.Directions
open Validation.Level

let selectCommand validator operation direction actorId level =
    level 
        |> validator direction actorId 
        |> bind (fun l -> operation direction actorId l)

let private selectActorCommand direction actorId level =
    resultOrElse {
        return! selectCommand isValidMove buildMoveActorCommand direction actorId level
        return! selectCommand canOpenDoor buildOpenDoorCommand direction actorId level
        return! (invalidCommand level)
    }

let rec handleKeyPress activeBuilder actorId =
    let workingBuilder = 
        match activeBuilder with
        | Some builder -> builder
        | None -> selectActorCommand

    match Console.ReadKey().Key with
    | ConsoleKey.O -> handleKeyPress (Some buildOpenDoorCommand) actorId
    | ConsoleKey.C -> handleKeyPress (Some buildCloseDoorCommand) actorId
    | ConsoleKey.W -> workingBuilder north actorId
    | ConsoleKey.A -> workingBuilder west actorId
    | ConsoleKey.S -> workingBuilder south actorId
    | ConsoleKey.D -> workingBuilder east actorId
    | ConsoleKey.Spacebar -> idleCommand
    | _ -> invalidCommand


let getPlayerCommand actorId = handleKeyPress None actorId

