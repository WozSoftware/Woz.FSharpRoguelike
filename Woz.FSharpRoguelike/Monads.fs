module Monads

// Maybe (option)
module Maybe =
    let (>>=) monad func = Option.bind func monad

    type maybeFactory() =
        member this.Bind(monad, func) = monad >>= func
        member this.Return(value) = Some value

    type maybeOrElseFactory() =
        member this.ReturnFrom(x) = x
        member this.Combine (firstMonad,secondMonad) = 
            match firstMonad with
            | Some _ -> firstMonad
            | None _ -> secondMonad
        member this.Delay(f) = f()

    let maybe = new maybeFactory()
    let maybeOrElse = new maybeOrElseFactory()

// Result

type result<'a> =
    | Valid of 'a
    | Invalid of string

module Result =
    let bind func monad =
        match monad with
        | Valid value -> func value
        | Invalid error -> Invalid error

    let (>>=) monad func = bind func monad

    type resultFactory() =
        member this.Bind(monad, func) = monad >>= func
        member this.Return(value) = Valid value

    type resultOrElseFactory() =
        member this.ReturnFrom(x) = x
        member this.Combine (firstMonad,secondMonad) = 
            match firstMonad with
            | Valid _ -> firstMonad
            | Invalid _ -> secondMonad
        member this.Delay(f) = f()
    
    let result = new resultFactory()
    let resultOrElse = new resultOrElseFactory()

// State

//type state<'s, 'a> = State of ('s -> 'a * 's)
//
//module State =
//    let bind func monad =
//        (fun state ->
//            let result, state = monad state
//            func result state)
//
//    let (>>=) monad func = bind func monad
//
//    let get = (fun s -> s, s)
//    let set = (fun s -> (), s)
//    let run m s = m s
//
//    type stateFactory() =
//        member this.Bind(monad, func) = monad >>= func
//        member this.Return(value) = Valid value

