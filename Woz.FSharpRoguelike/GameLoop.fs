module GameLoop

open GameTypes
open PlayerInput
open Commands
open Queries.Level
open Monads
open Monads.Result
open RenderEngine

// Stub
let private getAiCommand actorId = idleCommand

let private runAiCommand level command  =
    match level |> command with
    | Valid updatedLevel -> updatedLevel
    | Invalid _ -> level

let private runAi (level: level) =
    let npdCommands = level |> npcIds |> Seq.map getAiCommand
    Valid (npdCommands |> Seq.fold runAiCommand level)

let private runTurn playerCommand level = 
    let turnResult = 
        result {
            let! playerMoved = level |> playerCommand
            let! aiMoved = playerMoved |> runAi
            return aiMoved
        }

    match turnResult with
    | Valid turnLevel -> turnLevel
    | Invalid _ -> level

let rec gameLoop level =
    render level

    let playerCommand = getPlayerCommand level.playerId
    let turnLevel = level |> runTurn playerCommand
    let player = level |> expectActor turnLevel.playerId
    if not (isAlive player) then
        ()
    else
        gameLoop turnLevel





