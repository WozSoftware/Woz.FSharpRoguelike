module GameLoop

//open GameTypes
//open PlayerInput
//open Commands
//open Queries.Level
//open Monads
//open Monads.Result
//
//// Stub
//let private getAiCommand = buildIdleCommand
//
//let runAiCommand level command  =
//    match level |> command with
//    | Valid updatedLevel -> updatedLevel
//    | Invalid _ -> level
//
//let runAi (level: level) =
//    let npdCommands = level |> npcIds |> Seq.map getAiCommand
//    Valid (npdCommands |> Seq.fold runAiCommand level)
//
//let rec gameLoop level =
//    let turnResult = 
//        result {
//            let! playerMoved = level |> getPlayerCommand level.playerId
//            let! aiMoved = playerMoved |> runAi
//            return aiMoved
//        }
//    match turnResult with
//    | Valid turnLevel -> 
//    | Invalid
//
//




