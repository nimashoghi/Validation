module Validation.AsyncValidation

open System.Threading.Tasks
open FSharp.Utils.Tasks

/// **Description**
///     Applies the function `f` to the value inside container `x`.
let map
    (f: 'input -> 'output)
    (x: Result<'input, 'error list> ValueTask)
    : Result<'output, 'error list> ValueTask =
    vtask {
        match! x with
        | Ok value -> return Ok (f value)
        | Error errors -> return Error errors
    }

/// **Description**
///     Applies the function `f` to the error inside container `x`.
let mapError
    (f: 'error list -> 'newError list)
    (x: Result<_, 'error list> ValueTask)
    : Result<_, 'newError list> ValueTask =
    vtask {
        match! x with
        | Ok value -> return Ok value
        | Error errors -> return Error (f errors)
    }


/// **Description**
///     Applies the function `f` to the error inside container `x` and flattens the result.
let bind
    (f: 'input -> Result<'result, 'error list> ValueTask)
    (x: Result<'input, 'error list> ValueTask)
    : Result<'result, 'error list> ValueTask =
    vtask {
        match! x with
        | Ok value -> return! f value
        | Error errors -> return Error errors
    }

/// **Description**
///     Applies the function inside the container `f` to the value inside the container `x`.
let apply
    (f: Result<'input -> 'output, 'error list> ValueTask)
    (x: Result<'input, 'error list> ValueTask)
    : Result<'output, 'error list> ValueTask =
    vtask {
        do! Task.WhenAll [f.AsTask () :> Task; x.AsTask () :> Task]
        match f.Result, x.Result with
        | Ok f, Ok x -> return Ok (f x)
        | Error fErrors, Error xErrors -> return Error (fErrors @ xErrors)
        | Error errors, _ | _, Error errors -> return Error errors
    }
