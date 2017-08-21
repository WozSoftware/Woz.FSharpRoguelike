module PlayerInput

open System
open Commands
open Vector.Directions

let getPlayerCommand =
    match Console.ReadKey().Key with
    | ConsoleKey.W -> buildMoveActorCommand north
    | ConsoleKey.A -> buildMoveActorCommand east
    | ConsoleKey.S -> buildMoveActorCommand south
    | ConsoleKey.D -> buildMoveActorCommand west
    | _ -> buildIdleCommand

