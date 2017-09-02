module GameTypes

open Aether
open Aether.Operators
open Aether.Optics
open AetherExtensions.Optics
open AetherExtensions.Optics.Map
open AetherExtensions.Optics.List
open Vector

// -----------------------------------------

type tile = Void | Floor | Wall | Water 

type map = tile[][]

module Map = 
    let bottomLeft = vector.create 0 0
    let width (map: map) = map.[0].Length - 1
    let height (map: map) = map.Length - 1
    let topRight tiles = vector.create (width tiles) (height tiles)

// -----------------------------------------

type door = 
    | Open
    | Closed
    | Locked of string // Key name

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

type slot = | Helmet | Torso | Legs | Gloves | Boots

type key = {id: int; name: string}
type armor = {id: int; name: string; slot: slot; defence: int; absorbs: int}
type weapon = {id: int; name: string; attack: int; damage: int}
type potion = {id: int; name: string; stat: stats; effect: int}

type item =
    | Key of key
    | Armor of armor
    | Weapon of weapon
    | Potion of potion

module Item =
    let idOf item = 
        match item with
        | Key k -> k.id
        | Armor a -> a.id
        | Weapon w -> w.id
        | Potion p -> p.id

    let nameOf item = 
        match item with
        | Key k -> k.name
        | Armor a -> a.name
        | Weapon w -> w.name
        | Potion p -> p.name

    let hasId id item = (idOf item) = id

// -----------------------------------------

type actor = 
    {
        id: int
        isNpc: bool
        name: string
        stats: Map<stats, stat>
        location: vector
        backpack: Map<int, item>
        equiped: Map<slot, int>
        weapon: int option
    }

module Actor =
    let hasId id stat = stat.id = id

    let stats_ =
        (fun actor -> actor.stats), 
        (fun stats actor -> {actor with stats = stats})    

    let statFor_ stat = 
        stats_ >-> Map.value_ stat 

    let expectStatFor_ stat = 
        stats_ >-> expectValue_ stat 

    let currentHealth_ = 
        (expectStatFor_ Health) >-> Stat.current_

    let location_ =
        (fun actor -> actor.location), 
        (fun location actor -> {actor with location = location})    

    let backpack_ =
        (fun actor -> actor.backpack), 
        (fun backpack actor -> {actor with backpack = backpack})    

    let backpackItemWithId_ id = 
        backpack_ >-> Map.value_ id

    let expectBackpackItemWithId_ id = 
        backpack_ >-> expectValue_ id

// -----------------------------------------

type level = 
    {
        playerId: int

        map: map; 
        doors: Map<vector, door>; 
        actors: Map<int, actor>
        items: Map<vector, List<item>>

        mapActors: Map<vector, int>
    }

module Level =
    // Player

    let playerId_ =
        (fun level -> level.playerId), 
        (fun playerId level -> {level with playerId = playerId})    
    
    // Doors

    let private doors_ =
        (fun level -> level.doors), 
        (fun doors level -> {level with doors = doors})    

    let doorAt_ location = 
        doors_ >-> Map.value_ location 

    let expectDoorAt_ location = 
        doors_ >-> expectValue_ location 
    
    // Actors

    let actors_ =
        (fun level -> level.actors), 
        (fun actors level -> {level with actors = actors})    

    let actorWithId_ actorId = 
        actors_ >-> Map.value_ actorId 

    let expectActorWithId_ actorId = 
        actors_ >-> expectValue_ actorId 

    let private mapActors_ =
        (fun level -> level.mapActors), 
        (fun mapActors level -> {level with mapActors = mapActors})    

    let mapActorAt_ location = 
        mapActors_ >-> Map.value_ location 

    let expectMapActorAt_ location = 
        mapActors_ >-> expectValue_ location 

    // Items

    let private items_ = 
        (fun level -> level.items), 
        (fun items level -> {level with items = items})    

    let itemsAt_ location = 
        items_ >-> Map.value_ location >-> notEmpty_

    let itemWithId_ location id = 
        itemsAt_ location >-> where_ (Item.hasId id)

    let expectItemWithId_ location id = 
        itemsAt_ location >-> expectWhere_ (Item.hasId id)

   