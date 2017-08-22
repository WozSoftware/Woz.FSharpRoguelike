module PlayerInput

open System
open Commands
open Vector.Directions

let getPlayerCommand actorId =
    match Console.ReadKey().Key with
    | ConsoleKey.W -> buildMoveActorCommand north actorId
    | ConsoleKey.A -> buildMoveActorCommand west actorId
    | ConsoleKey.S -> buildMoveActorCommand south actorId
    | ConsoleKey.D -> buildMoveActorCommand east actorId
    | _ -> buildIdleCommand actorId

