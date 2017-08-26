module Core

let not (predicate: ('a -> bool)) =
    (fun value -> predicate value |> not)
