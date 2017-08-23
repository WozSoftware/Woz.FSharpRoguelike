module Commands

open Monads
open Monads.Result
open GameTypes
open Operations.Level
open Validation.Level

let private composeCommand validation operation level =
    result {
        let! validLevel = validation level
        return operation validLevel
    }

let private buildCommand validator operation direction actorId =
    let test = validator direction actorId
    let action = operation direction actorId
    composeCommand test action

let buildIdleCommand (actorId: actorId) level = Valid level

let buildMoveActorCommand = buildCommand isValidMove moveActor 

let buildOpenDoorCommand = buildCommand canOpenDoor openDoor 

let buildCloseDoorCommand = buildCommand canCloseDoor closeDoor 
    