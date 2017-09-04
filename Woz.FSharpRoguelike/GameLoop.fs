module GameLoop

open GameTypes
open PlayerInput
open Commands
open Queries.Level
open Monads
open Operations
open Monads.Result
open RenderEngine

// Stub
let private getAiCommand actorId = idleCommand

let private runAiCommand level command  =
    match level |> command with
    | Valid updatedLevel -> updatedLevel
    | Invalid _ -> level

let private runAi (level: level) =
    level 
        |> npcIds 
        |> Seq.map getAiCommand
        |> Seq.fold runAiCommand level
        |> Valid

let private runTurn playerCommand level = 
    let turnResult = 
        result {
            let! playerMoved = level |> playerCommand
            let! aiMoved = playerMoved |> runAi
            return aiMoved
        }

    match turnResult with
    | Valid turnLevel -> turnLevel
    | Invalid message -> level |> log message

let rec gameLoop level =
    let playerCommand = level |> getPlayerCommand 
    let turnLevel = level |> runTurn playerCommand |> render |> flush 
    if turnLevel |> isPlayerDead then
        ()
    else
        turnLevel |> gameLoop





