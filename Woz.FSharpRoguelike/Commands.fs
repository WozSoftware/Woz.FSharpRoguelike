module Commands

open Monads
open Monads.Result
open GameTypes
open Operations
open Validation

let private composeCommand validation operation level =
    result {
        let! validLevel = validation level
        return operation validLevel
    }

let private buildCommand validator operation direction actorId =
    let test = validator direction actorId
    let action = operation direction actorId
    composeCommand test action

let invalidCommand (level: level) = Invalid "Unknow command"

let idleCommand (level: level) = Valid level

let buildMoveActorCommand = buildCommand isValidMove moveActor 

let buildOpenDoorCommand = buildCommand canOpenDoor openDoor 

let buildCloseDoorCommand = buildCommand canCloseDoor closeDoor 

let buildUnlockDoorCommand = buildCommand canUnlockDoor unlockDoor 

let buildTakeItemsCommand = buildCommand canTakeItems takeItems
    