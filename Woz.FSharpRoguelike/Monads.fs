module Monads

type result<'a> =
    | Valid of 'a
    | Invalid of string

module Result =
    let bind func monad =
        match monad with
        | Valid value -> func value
        | Invalid error -> Invalid error

    type resultFactory() =
        member this.Bind(monad, func) = bind func monad
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

module Maybe =
    type maybeFactory() =
        member this.Bind(monad, func) = Option.bind func monad
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
