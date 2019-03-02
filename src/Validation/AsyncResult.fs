module Validation.AsyncValidation

open System.Threading.Tasks
open FSharp.Utils

/// **Description**
///     Applies the function `f` to the value inside container `x`.
let map
    (f: 'input -> 'output)
    (x: Result<'input, 'error list> Task)
    : Result<'output, 'error list> Task =
    Task.map (Result.map f) x

/// **Description**
///     Applies the function `f` to the error inside container `x`.
let mapError
    (f: 'error list -> 'newError list)
    (x: Result<_, 'error list> Task)
    : Result<_, 'newError list> Task =
    Task.map (Result.mapError f) x

/// **Description**
///     Applies the function `f` to the error inside container `x` and flattens the result.
let bind
    (f: 'input -> Result<'result, 'error list> Task)
    (x: Result<'input, 'error list> Task)
    : Result<'result, 'error list> Task =
    Task.bind (function
        | Ok value -> f value
        | Error errors -> Task.FromResult (Error errors)
    ) x

/// **Description**
///     Applies the function inside the container `f` to the value inside the container `x`.
let apply
    (f: Result<'input -> 'output, 'error list> Task)
    (x: Result<'input, 'error list> Task)
    : Result<'output, 'error list> Task =
    let source = TaskCompletionSource<Result<'output, 'error list>>()

    Task
        .WhenAll(f, x)
        .ContinueWith(
            fun (task: Task) ->
                if task.IsCompleted then
                    Validation.apply f.Result x.Result
                    |> source.SetResult
                else if task.IsCanceled then source.SetCanceled()
                else if task.IsFaulted then source.SetException task.Exception
        )
    |> ignore

    source.Task
