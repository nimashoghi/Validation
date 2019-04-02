module Validation.Builder

open System.Threading.Tasks
open FSharp.Utils.Tasks

open Validation.Operators.AsyncValidation

[<AutoOpen>]
module internal Internals =
    let zero (): Result<'value, 'error list> ValueTask = vtask { return Error [] }

    let ``yield`` (): Result<'value, 'error list> ValueTask = vtask { return Ok Unchecked.defaultof<_> }

    let ``return`` (x: 'value): Result<'value, 'error list> ValueTask = vtask { return Ok x }

    let ``return!``
        (x: Result<'value, 'error list> ValueTask)
        : Result<'value, 'error list> ValueTask =
        vtask { return! x }

    let ``return!Task``
        (x: Result<'value, 'error list> Task)
        : Result<'value, 'error list> ValueTask =
        vtask { return! x }

    let ``for``
        (x: Result<'value, 'error list> ValueTask)
        (f: 'value -> Result<'result, 'error list> ValueTask)
        : Result<'result, 'error list> ValueTask =
        AsyncValidation.bind f x

    let ``forTask``
        (x: Result<'value, 'error list> ValueTask)
        (f: 'value -> Result<'result, 'error list> Task)
        : Result<'result, 'error list> ValueTask =
        AsyncValidation.bind (fun value -> vtask { return! f value }) x

    let validate
        (outer: Result<'outer, 'error list> ValueTask)
        (inner: Result<'inner, 'error list> ValueTask)
        (f: 'outer -> 'inner -> 'result)
        : Result<'result, 'error list> ValueTask =
        f <!> outer <*> inner

    let validateTask
        (outer: Result<'outer, 'error list> ValueTask)
        (inner: Result<'inner, 'error list> Task)
        (f: 'outer -> 'inner -> 'result)
        : Result<'result, 'error list> ValueTask =
        f <!> outer <*> vtask { return! inner }

    let validateSync
        (outer: Result<'outer, 'error list> ValueTask)
        (inner: Result<'inner, 'error list>)
        (f: 'outer -> 'inner -> 'result)
        : Result<'result, 'error list> ValueTask =
        f <!> outer <*> vtask { return inner }

type ValidationBuilder () =
    member __.Zero () = zero ()
    member __.Yield (_: unit) = ``yield`` ()
    member __.For (x, f) = ``for`` x f
    member __.For (x, f) = ``forTask`` x f
    [<CustomOperation ("validate", IsLikeZip = true)>]
    member __.Validate (outer, inner, f) = validate outer inner f
    member __.Validate (outer, inner, f) = validateTask outer inner f
    member __.Validate (outer, inner, f) = validateSync outer inner f
    member __.Return x = ``return`` x
    member __.ReturnFrom x = ``return!`` x
    member __.ReturnFrom x = ``return!Task`` x

let validation = ValidationBuilder ()
