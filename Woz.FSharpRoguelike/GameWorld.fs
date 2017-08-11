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

module Stat =
    let increaseCurrent amount stat =
        {stat with current = min (stat.current + amount) stat.max}
    
    let decreaseCurrent amount stat =
        {stat with current = max (stat.current - amount) 0}
  
    let increaseMax amount stat =
        let newValue = stat.max + amount
        {stat with current = newValue; max = newValue}

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
 
module Actor =
    let updateStat editor stat actor =
        let current = editor (actor.stats.TryFind stat).Value
        {actor with stats = actor.stats.Add(stat, editor current)}

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

module Level =
    open Actor

    let hasCoordinate location level = 
        location >= Map.bottomLeft && 
        location <= level.map.topRight

    let getTile location level = 
        level.map.tiles.[location.y].[location.y]

    let isBlockingTile location level =
        match getTile location level with
        | Void | Wall -> true
        | Floor | Water -> false

    let getDoor location level = 
        level.doors.TryFind location

    let isBlockingDoor location level = 
        match getDoor location level with
        | Some door -> 
            match door with 
            | Closed | Locked _ -> true
            | Open -> false
        | None -> false

    let getActorById actorId level =
        level.actors.TryFind actorId

    let getActorByLocation location level =
        level.mapActors.TryFind location

    let hasActor location level =
        (getActorByLocation location level).IsSome

    let blockView = [isBlockingTile; isBlockingDoor]
    let blockMove = [isBlockingTile; isBlockingDoor; hasActor]
    
    let checkLocationFor predicates location level =
        if hasCoordinate location level then 
            let testPredicate = (fun predicate -> predicate location level)
            predicates |> Seq.exists testPredicate
        else true

    let getItems location level =
        match level.mapItems.TryFind location with
        | Some items -> items
        | None -> []

    let addActor actor level =
        {
            level with 
                actors = level.actors.Add(actor.id, actor)
                mapActors = level.mapActors.Add(actor.location, actor.id)
        }
    
    let removeActor actor level =
        {
            level with 
                actors = level.actors.Remove(actor.id)
                mapActors = level.mapActors.Remove(actor.location)
        }

    let updateActor editor actor level =
        level |> removeActor actor |> addActor (editor actor)
    
    let updateActorStat editor stat actor level =
        level |> updateActor (updateStat editor stat) actor

//    let lowerActorStat amount stat actor level =
//        level |> updateActorStat (decreaseCurrent amount) stat actor 

    