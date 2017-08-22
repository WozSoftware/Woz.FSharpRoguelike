module LevelFactory

open GameTypes
open Vector
open Operations.Level

let private testLevelTemplate =
    [
        "                                            "; // y = 0
        "    ################                        "; // y = 1
        "    #..............#         ###########    "; // y = 2
        "    #..............#         #......~~~#    "; // y = 3
        "    #..............###########......~~~#    "; // y = 4
        "    #................................###    "; // y = 5
        "    #..............###########.......#      "; // y = 6
        "    #..............#         #########      "; // y = 7
        "    ################                        "; // y = 8
        "                                            "; // y = 9
        "                                            "; // y = 10
        "                                            "  // y = 11
    ]

let private width = testLevelTemplate.[0].Length
let private height = testLevelTemplate.Length

let private testPlayer =
    {
        id = 1;
        isNpc = false;
        name = "player";
        stats = [(Health, {current = 10; max = 10})] |> Map.ofSeq
        location = {x = 9; y = 6};
        backpack = []
    }

let private charToTile character =
    match character with
    | '#' -> Wall
    | '.' -> Floor
    | '~' -> Water
    | _ -> Void

let private testMap =
    let rowToTiles row = row |> Seq.map charToTile |> Array.ofSeq
    testLevelTemplate |> Seq.rev |> Seq.map rowToTiles |> Array.ofSeq

let testLevel =
    let level = 
        {
            playerId = testPlayer.id;
            map = {tiles = testMap; topRight = {x = testMap.[0].Length - 1; y = testMap.Length - 1}}
            doors = Map.empty<vector, door>
            actors = Map.empty<actorId, actor>
            items = Map.empty<itemId, item>
            mapActors = Map.empty<vector, actorId>
            mapItems = Map.empty<vector, List<itemId>>
        }

    level |> spawnActor testPlayer 
