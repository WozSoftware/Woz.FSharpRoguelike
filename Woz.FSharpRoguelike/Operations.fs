module Operations

open Aether
open Aether.Operators
open GameTypes
open GameTypes.Actor
open GameTypes.Level

module Stat =
    let increaseCurrent amount stat =
        {stat with current = min (stat.current + amount) stat.max}
    
    let decreaseCurrent amount stat =
        {stat with current = max (stat.current - amount) 0}
  
    let increaseMax amount stat =
        let newValue = stat.max + amount
        {stat with current = newValue; max = newValue}

module Level =
    open Stat
    open Queries.Level

    let spawnActor actor level =
        level
            |> Optic.set (expectActorWithId_ actor.id) actor
            |> Optic.set (expectMapActorAt_ actor.location) actor.id
    
    let removeActor actorId level =
        let actor = level |> getActor actorId
        level 
            |> Optic.set (actorWithId_ actorId) None
            |> Optic.set (mapActorAt_ actor.location) None

    let moveActor location actorId level =
        let actor = level |> getActor actorId
        let movedActor = actor |> Optic.set location_ location 
        level 
            |> Optic.set (expectActorWithId_ actorId) movedActor
            |> Optic.set (mapActorAt_ actor.location) None
            |> Optic.set (expectMapActorAt_ location) actorId
    
    let hurtActor damage actorId level =
        let actorHealth_ = expectActorWithId_ actorId >-> expectStatFor_ Health
        let updatedHealth = decreaseCurrent damage (level |> Optic.get actorHealth_)
        level |> Optic.set actorHealth_ updatedHealth


    