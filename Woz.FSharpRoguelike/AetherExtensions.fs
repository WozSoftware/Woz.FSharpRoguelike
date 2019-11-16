module AetherExtensions

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
            (function 
                | Some list -> list
                | None -> []), 
            (fun list -> 
                match Seq.isEmpty list with
                | true -> None
                | false -> Some list)

        let private without predicate =
            List.filter (predicate >> not) 

        let where_ predicate : Lens<list<'a>, 'a option> =
            List.tryFind predicate,
            (fun itemOption list -> 
                let cleanList = list |> without predicate
                match itemOption with
                | Some item -> item :: cleanList
                | None -> cleanList)

        let expectWhere_ predicate : Lens<list<'a>, 'a> =
            List.find predicate,
            (fun item list -> 
                let cleanList = list |> without predicate
                item :: cleanList)
