module LevelFactory

open GameTypes
open Vector
open Operations

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

let private testPlayer =
    {
        id = 1
        isNpc = false
        name = "player"
        stats = [(Health, {current = 10; max = 10})] |> Map.ofSeq
        location = vector.create 9 6
        backpack = Map.empty<int, item>
        equipped = Map.empty<slot, item>
        weapon = None
    }

let private charToTile character =
    match character with
    | '#' -> Wall
    | '.' -> Floor
    | '~' -> Water
    | _ -> Void

let private testItem1 = 
    Key 
        {
            id = 2
            name = "Basic key"
        }

let private testItem2 = 
    Potion 
        {
            id = 3
            name = "Minor health potion"
            stat = Health
            effect = 5
        }

let private testMap =
    let rowToTiles row = row |> Seq.map charToTile |> Array.ofSeq
    testLevelTemplate |> Seq.rev |> Seq.map rowToTiles |> Array.ofSeq

let testLevel =
    let level = 
        {
            playerId = testPlayer.id;
            map = testMap
            doors = Map.empty<vector, door>
            actors = Map.empty<int, actor>
            items = Map.empty<vector, List<item>>
            mapActors = Map.empty<vector, int>
        }

    level 
        |> spawnActor testPlayer 
        |> placeDoor Open (vector.create 19 6)
        |> placeDoor Closed (vector.create 29 6)
        |> placeItem testItem1 (vector.create 17 5)
        |> placeItem testItem2 (vector.create 16 7)
