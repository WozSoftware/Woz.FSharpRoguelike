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
        //return! selectCommand canCloseDoor buildCloseDoorCommand direction actorId level
    }

let getPlayerCommand actorId =
    match Console.ReadKey().Key with
    | ConsoleKey.W -> selectActorCommand north actorId
    | ConsoleKey.A -> selectActorCommand west actorId
    | ConsoleKey.S -> selectActorCommand south actorId
    | ConsoleKey.D -> selectActorCommand east actorId
    | _ -> buildIdleCommand actorId

