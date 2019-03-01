module Validation.Builder

open System.Threading.Tasks
open FSharp.Utils.Tasks

open Validation.Operators.AsyncValidation

[<AutoOpen>]
module internal Internals =
    let zero
        ()
        : Result<'value, 'error list> Task =
        Task.FromResult (Error [])

    let ``yield``
        ()
        : Result<'value, 'error list> Task =
        Task.FromResult (Ok (unbox<_> null))

    let ``return``
        (x: 'value)
        : Result<'value, 'error list> Task =
        Task.FromResult (Ok x)

    let ``return!``
        (x: Result<'value, 'error list> Task)
        : Result<'value, 'error list> Task =
        x

    let ``for``
        (x: Result<'value, 'error list> Task)
        (f: 'value -> Result<'result, 'error list> Task)
        : Result<'result, 'error list> Task =
        AsyncValidation.bind f x

    let validate
        (outer: Result<'outer, 'error list> Task)
        (inner: Result<'inner, 'error list> Task)
        (f: 'outer -> 'inner -> 'result)
        : Result<'result, 'error list> Task =
        f <!> outer <*> inner

    let validateSync
        (outer: Result<'outer, 'error list> Task)
        (inner: Result<'inner, 'error list>)
        (f: 'outer -> 'inner -> 'result)
        : Result<'result, 'error list> Task =
        f <!> outer <*> (Task.FromResult inner)

type ValidationBuilder () =
    member __.Zero () = zero ()
    member __.Yield (_: unit) = ``yield`` ()
    member __.For (x, f) = ``for`` x f
    [<CustomOperation ("validate", IsLikeZip = true)>]
    member __.Validate (outer, inner, f) = validate outer inner f
    member __.Validate (outer, inner, f) = validateSync outer inner f
    member __.Return x = ``return`` x
    member __.ReturnFrom x = ``return!`` x

let validation = ValidationBuilder ()
