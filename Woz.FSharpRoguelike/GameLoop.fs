module GameLoop

open GameTypes
open PlayerInput
open Commands
open Queries.Level
open Monads
open Monads.Result
open RenderEngine

// Stub
let private getAiCommand = buildIdleCommand

let runAiCommand level command  =
    match level |> command with
    | Valid updatedLevel -> updatedLevel
    | Invalid _ -> level

let runAi (level: level) =
    let npdCommands = level |> npcIds |> Seq.map getAiCommand
    Valid (npdCommands |> Seq.fold runAiCommand level)

let runTurn playerCommand level = 
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
    let playerCommand = getPlayerCommand level.playerId
    let turnLevel = level |> runTurn playerCommand
    let player = level |> expectActor turnLevel.playerId
    if not (isAlive player) then
        ()
    else
        render turnLevel
        gameLoop turnLevel





