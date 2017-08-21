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

    let result = new resultFactory()

module Maybe =
    type maybeFactory() =
        member this.Bind(monad, func) = Option.bind func monad

        member this.Return(value) = Some value

    let maybe = new maybeFactory()
