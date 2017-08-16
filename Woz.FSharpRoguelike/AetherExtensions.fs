module AetherExtensions

open Aether 

module Optics =

    module Option =
        let expect_ : Lens<'a option, 'a> =
            (fun valueOption -> valueOption.Value), 
            (fun value valueOption -> Some value)

    module Map =
        let expectValue_ (k: 'k) : Lens<Map<'k,'v>, 'v> =
            Map.find k,
            (fun v m -> Map.add k v m)
