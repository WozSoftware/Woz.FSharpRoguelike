module GameTypes

open Aether
open Aether.Operators
open Aether.Optics
open AetherExtensions.Optics
open AetherExtensions.Optics.Map
open Vector

// -----------------------------------------

type tile = Void | Floor | Wall | Water 

type map = 
    {
        tiles: tile[][]
        topRight: vector
    } 

module Map = 
    let bottomLeft = {x = 0; y = 0}

// -----------------------------------------

type door = 
    | Open
    | Closed
    | Locked of string // Key name

// -----------------------------------------

type itemId = int

type item = {id: itemId; name: string }

// -----------------------------------------

type stats = 
    | Health 
    | Strength 
    | Intelligence 
    | Stamina 
    | Dexterity

type stat = {current: int; max: int}

module Stat =
    let current_ =
        (fun stat -> stat.current), 
        (fun current stat -> {stat with current = current})    

    let max_ =
        (fun stat -> stat.max), 
        (fun max stat -> {stat with max = max})    

// -----------------------------------------

type actorId = int

type actor = 
    {
        id: int
        isNpc: bool
        name: string
        stats: Map<stats, stat>
        location: vector
        backpack: List<itemId>
    }

module Actor =
    let stats_ =
        (fun actor -> actor.stats), 
        (fun stats actor -> { actor with stats = stats})    

    let statFor_ stat = 
        stats_ >-> Map.value_ stat 

    let expectStatFor_ stat = 
        stats_ >-> expectValue_ stat 

    let location_ =
        (fun actor -> actor.location), 
        (fun location actor -> { actor with location = location})    

// -----------------------------------------

type level = 
    {
        playerId: actorId

        map: map; 
        doors: Map<vector, door>; 
        actors: Map<actorId, actor>
        items: Map<itemId, item>

        mapActors: Map<vector, actorId>
        mapItems: Map<vector, List<itemId>>
    }

module Level =
    let playerId_ =
        (fun level -> level.playerId), 
        (fun playerId level -> { level with playerId = playerId})    

    let doors_ =
        (fun level -> level.doors), 
        (fun doors level -> { level with doors = doors})    

    let doorAt_ location = 
        doors_ >-> Map.value_ location 

    let expectDoorAt_ location = 
        doors_ >-> expectValue_ location 

    let actors_ =
        (fun level -> level.actors), 
        (fun actors level -> { level with actors = actors})    

    let actorWithId_ actorId = 
        actors_ >-> Map.value_ actorId 

    let expectActorWithId_ actorId = 
        actors_ >-> expectValue_ actorId 

    let mapActors_ =
        (fun level -> level.mapActors), 
        (fun mapActors level -> { level with mapActors = mapActors})    

    let mapActorAt_ location = 
        mapActors_ >-> Map.value_ location 

    let expectMapActorAt_ location = 
        mapActors_ >-> expectValue_ location 
 