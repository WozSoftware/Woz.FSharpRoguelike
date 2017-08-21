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

let buildIdleCommand (actorId: actorId) = (fun (level: level) -> Valid level)

let buildMoveActorCommand direction actorId =
    let validator = isValidMove direction actorId
    let operation = moveActor direction actorId
    composeCommand validator operation