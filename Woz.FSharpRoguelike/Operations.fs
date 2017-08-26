module Operations

open Aether
open Aether.Operators
open GameTypes
open GameTypes.Actor
open GameTypes.Level
open Queries.Level

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
    
let hurtActor damage actorId level =
    let actorHealth_ = expectActorWithId_ actorId >-> expectStatFor_ Health
    let updatedHealth = decreaseCurrent damage (level |> Optic.get actorHealth_)
    level |> Optic.set actorHealth_ updatedHealth

let addItemToBackpack (item: item) actorId level =
    let actor = level |> expectActor actorId 
    let newActor = actor |> Optic.set (expectBackpackItemWithId_ item.id) item
    level |> Optic.set (expectActorWithId_ actorId) newActor

// Door

let placeDoor state location =
    Optic.set (expectDoorAt_ location) state
    
let openDoor direction actorId level = 
    let _, targetLocation = level |> actorTarget direction actorId
    level |> placeDoor Open targetLocation 

let closeDoor direction actorId level = 
    let _, targetLocation = level |> actorTarget direction actorId
    level |> placeDoor Closed targetLocation 

//let unlockDoor = placeDoor (Some Closed)

// Items

let placeItem (item: item) location =
    Optic.set (itemWithId_ location item.id) (Some item)

let takeItems direction actorId level =
    let actor, targetLocation = level |> actorTarget direction actorId
    let locationItems = level |> Optic.get (itemsAt_ targetLocation)
    let newBackpack = List.concat [locationItems; actor |> Optic.get backpack_]
    let newActor = actor |> Optic.set backpack_ newBackpack
    level 
        |> Optic.set (expectActorWithId_ actorId) newActor
        |> Optic.set (itemsAt_ targetLocation) []



    