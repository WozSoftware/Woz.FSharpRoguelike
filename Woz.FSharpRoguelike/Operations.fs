module Operations

open Aether
open Aether.Operators
open GameTypes
open GameTypes.Actor
open GameTypes.Item
open GameTypes.Level
open Queries.Level

// Messages

let log message level =
    let messages = message :: (level |> Optic.get messages_)
    level |> Optic.set messages_ messages

let logAll newMessages level =
    let messages = 
        level 
            |> Optic.get messages_
            |> Seq.append newMessages
            |> List.ofSeq
    level |> Optic.set messages_ messages

let flush = Optic.set messages_ []

// Stats

let private increaseCurrent amount stat =
    {stat with current = min (stat.current + amount) stat.max}
    
let private decreaseCurrent amount stat =
    {stat with current = max (stat.current - amount) 0}
  
let private increaseMax amount stat =
    let newValue = stat.max + amount
    {stat with current = newValue; max = newValue}

// Actor

let actorTarget direction actorId level =
    let actor = level |> expectActor actorId
    let targetLocation = actor.location + direction
    actor, targetLocation

let spawnActor actor =
    Optic.set (expectActorWithId_ actor.id) actor
    >> Optic.set (expectMapActorAt_ actor.location) actor.id
    
let removeActor actorId level =
    let actor = level |> expectActor actorId
    level 
        |> Optic.set (actorWithId_ actorId) None
        |> Optic.set (mapActorAt_ actor.location) None

let moveActor direction actorId level =
    let actor, targetLocation = level |> actorTarget direction actorId
    let movedActor = actor |> Optic.set location_ targetLocation
    level 
        |> Optic.set (expectActorWithId_ actorId) movedActor
        |> Optic.set (mapActorAt_ actor.location) None
        |> Optic.set (expectMapActorAt_ targetLocation) actorId
        |> log (actor.name + " moved")
    
let hurtActor damage actorId level =
    let actor = level |> Optic.get (expectActorWithId_ actorId)
    let actorHealth_ = expectActorWithId_ actorId >-> expectStatFor_ Health
    let updatedHealth = decreaseCurrent damage (level |> Optic.get actorHealth_)
    level 
        |> Optic.set actorHealth_ updatedHealth
        |> log (actor.name + " took damage")

// Not really needed so dropped
//let addItemToBackpack item actorId level =
//    let actor = level |> expectActor actorId 
//    let newActor = actor |> Optic.set (expectBackpackItemWithId_ (idOf item)) item
//    level 
//        |> Optic.set (expectActorWithId_ actorId) newActor
//        |> log (actor.name + " took " + (nameOf item))

// Door

let placeDoor state location =
    Optic.set (expectDoorAt_ location) state
    
let openDoor direction actorId level = 
    let actor, targetLocation = level |> actorTarget direction actorId
    level 
        |> placeDoor Open targetLocation 
        |> log (actor.name + " opened a door")

let closeDoor direction actorId level = 
    let actor, targetLocation = level |> actorTarget direction actorId
    level 
        |> placeDoor Closed targetLocation 
        |> log (actor.name + " closed a door")

let unlockDoor direction actorId level = 
    let actor, targetLocation = level |> actorTarget direction actorId
    level 
        |> placeDoor Closed targetLocation 
        |> log (actor.name + " unlocked a door")

// Items

let placeItem item location =
    Optic.set (itemWithId_ location (idOf item)) (Some item)

let private mergeItemMaps items1 items2 =
    let folder items id item = Map.add id item items
    Map.fold folder items1 items2

let takeItems direction actorId level =
    let actor, targetLocation = level |> actorTarget direction actorId
    let locationItems = level |> itemsAt targetLocation
    let newBackpack = 
        actor 
            |> Optic.get backpack_ 
            |> mergeItemMaps (locationItems |> toItemMap)
    let newActor = actor |> Optic.set backpack_ newBackpack
    let messages = 
        locationItems 
            |> Seq.map (fun item -> actor.name + " took " + (nameOf item))
    level 
        |> Optic.set (expectActorWithId_ actorId) newActor
        |> Optic.set (itemsAt_ targetLocation) []
        |> logAll messages

