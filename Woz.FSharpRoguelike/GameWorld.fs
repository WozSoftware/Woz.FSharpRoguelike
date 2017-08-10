namespace Woz.FSharpRoguelike

[<AutoOpenAttribute>]

type public Vector = 
    {
        x: int
        y: int
    }

    static member op_Addition (lhs, rhs) = 
        {x = lhs.x + rhs.x; y = lhs.y + rhs.y}

    static member op_Subtraction (lhs, rhs) = 
        {x = lhs.x - rhs.x; y = lhs.y - rhs.y}

    static member op_Multiply (vector: Vector, scale: int) = 
        {x = vector.x * scale; y = vector.y * scale}

    static member op_LessThanOrEqual (lhs, rhs) = 
        lhs.x <= rhs.x && lhs.y <= rhs.y

    static member op_Equals (lhs, rhs) = 
        lhs.x = rhs.x && lhs.y = rhs.y

    static member op_LessThan (lhs, rhs) = 
        lhs.x < rhs.x && lhs.y < rhs.y

    static member op_GreaterThanOrEqual (lhs, rhs) = 
        lhs.x >= rhs.x && lhs.y >= rhs.y

    static member op_GreaterThan (lhs, rhs) = 
        lhs.x > rhs.x && lhs.y > rhs.y

type Tile = Void | Floor | Wall | Water 

type Map = 
    {
        tiles: Tile[][]
        topRight: Vector
    }

    static member bottomLeft = {x = 0; y = 0}

type Door = 
    | Open
    | Closed
    | Locked of string // Key name
  
type ItemId = int
  
type Item = {id: ItemId; name: string }
  
type Stats = 
    | Health 
    | Strength 
    | Intelligence 
    | Stamina 
    | Dexterity
  
type Stat = {current: int; max: int}
  
type ActorId = int
  
type Actor = 
    {
        id: int
        isNpc: bool
        name: string
        stats: Map<Stats, Stat>
        location: Vector
        backpack: List<ItemId>
    }
  
type Level = 
    {
        playerId: ActorId
  
        map: Map; 
        doors: Map<Vector, Door>; 
        actors: Map<ActorId, Actor>
        items: Map<ItemId, Item>
  
        mapActors: Map<Vector, ActorId>
        mapItems: Map<Vector, List<ItemId>>
    }

module GameWorld =

    let hasCoordinate level location = 
        location >= Map.bottomLeft && 
        location <= level.map.topRight

    let getTile level location = 
        level.map.tiles.[location.y].[location.y]

    let isBlockingTile level location =
        match getTile level location with
        | Void | Wall -> true
        | Floor | Water -> false

    let getDoor level location = 
        level.doors.TryFind location

    let isBlockingDoor level location = 
        match getDoor level location with
        | Some door -> 
            match door with 
            | Closed | Locked _ -> true
            | Open -> false
        | None -> false

    let getActorById level actorId =
        level.actors.TryFind actorId

    let getActorByLocation level location =
        level.mapActors.TryFind location

    let hasActor level location =
        (getActorByLocation level location).IsSome

    let blockView = [isBlockingTile; isBlockingDoor]
    let blockMove = [isBlockingTile; isBlockingDoor; hasActor]
    
    let checkLocationFor predicates level location =
        if hasCoordinate level location then 
            let testPredicate = (fun predicate -> predicate level location)
            predicates |> Seq.forall testPredicate
        else true

    let getItems level location =
        match level.mapItems.TryFind location with
        | Some items -> items
        | None -> []
