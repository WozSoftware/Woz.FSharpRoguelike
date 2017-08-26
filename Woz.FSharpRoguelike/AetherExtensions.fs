module AetherExtensions

open Core
open Aether 

module Optics =

    module Option =
        let expect_ : Isomorphism<'a option, 'a> =
            (fun valueOption -> valueOption.Value), 
            (fun value -> Some value)

    module Map =
        let expectValue_ (k: 'k) : Lens<Map<'k,'v>, 'v> =
            Map.find k,
            (fun v m -> Map.add k v m)

    module List =
        let notEmpty_ : Isomorphism<List<'a> option, List<'a>> =
            (fun listOption -> 
                match listOption with
                | Some list -> list
                | None -> []), 
            (fun list -> 
                match Seq.isEmpty list with
                | true -> None
                | false -> Some list)

        let expectWhere_ (predicate: ('a -> bool)) : Lens<list<'a>, 'a> =
            List.find predicate,
            (fun item list -> 
                let cleanList = List.filter (not predicate) list
                item :: cleanList)

        let where_ (predicate: ('a -> bool)) : Lens<list<'a>, 'a option> =
            List.tryFind predicate,
            (fun itemOption list -> 
                let cleanList = List.filter (not predicate) list
                match itemOption with
                | Some item -> item :: cleanList
                | None -> list)
