namespace Woz.FSharpRoguelike

module GameWorld =

    type Vector = {x: int; y: int}

    type Tile = Void | Floor | Wall | Water 

    type Map = {tiles: Tile[][]; mapSize: Vector}

    type Door = 
      | Open
      | Closed
      | Locked of string // Key name

    type ItemId = int

    type Item = {id: ItemId; name: string; }

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

        mapItems: Map<Vector, List<ItemId>>
      }

// Location lookup cache should this be in level?
//mapActors: Map<Vector, ActorId>; 

